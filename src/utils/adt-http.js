'use strict';

/**
 * ADT HTTP client for SAP ABAP Development Tools REST API.
 *
 * Uses axios with a single instance per AdtHttp — all requests share the same
 * connection pool, cookie jar, and CSRF token. This mirrors abap-adt-api's
 * approach and is required for ADT debug sessions where SAP pins the debug
 * work process to the originating HTTP session.
 */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const path = require('path');
const { extractBodyDetail } = require('./format-error');
const os = require('os');
const crypto = require('crypto');

class AdtHttp {
  constructor(config) {
    this.config = config;
    this.csrfToken = null;
    this.cookies = null;
    this.stateful = '';  // Set to 'stateful' to pin all requests to one WP

    const isHttp = config.protocol === 'http';
    const baseURL = `${config.protocol || 'https'}://${config.host}:${config.sapport}`;
    this._axios = axios.create({
      baseURL,
      ...(isHttp
        ? { httpAgent: new (require('http').Agent)({ keepAlive: true }) }
        : { httpsAgent: new https.Agent({ rejectUnauthorized: false, keepAlive: true }) }),
      maxRedirects: 0,
      validateStatus: () => true,
    });

    // Request interceptor: inject auth, CSRF, cookies, session type on every request
    this._axios.interceptors.request.use((reqConfig) => {
      reqConfig.headers['Authorization'] =
        `Basic ${Buffer.from(`${config.user}:${config.password}`).toString('base64')}`;
      reqConfig.headers['sap-client'] = config.client;
      reqConfig.headers['sap-language'] = config.language || 'EN';
      // Session type header on EVERY request (like abap-adt-api line 324).
      // When stateful, forces SAP to create/maintain SAP_SESSIONID.
      if (this.stateful) reqConfig.headers['X-sap-adt-sessiontype'] = this.stateful;
      if (this.cookies) reqConfig.headers['Cookie'] = this.cookies;
      // Send CSRF on ALL requests (like abap-adt-api). SAP validates it for session routing.
      if (this.csrfToken) reqConfig.headers['X-CSRF-Token'] = this.csrfToken;
      if (process.env.DEBUG_ADT === '1') {
        const cookieNames = (this.cookies || '').split(';').map(c => c.trim().split('=')[0]).filter(Boolean).join(',');
        const hasStateful = reqConfig.headers['X-sap-adt-sessiontype'] || '-';
        const hasCsrf = this.csrfToken ? 'yes' : 'no';
        process.stderr.write(`[adt] → ${(reqConfig.method || 'GET').toUpperCase()} ${reqConfig.url} stateful=${hasStateful} csrf=${hasCsrf} cookies=[${cookieNames}]\n`);
      }
      return reqConfig;
    });

    // Response interceptor: merge Set-Cookie headers into the cookie jar.
    // Skip on error responses (>= 400) — they may set stale cookies that
    // overwrite valid session state (e.g. sap-usercontext on 400).
    this._axios.interceptors.response.use((resp) => {
      if (process.env.DEBUG_ADT === '1') {
        const newCookies = (resp.headers['set-cookie'] || []).map(c => c.split('=')[0]).join(',');
        const sock = resp.request && resp.request.socket;
        const port = sock ? sock.localPort : '?';
        process.stderr.write(`[adt] ← ${resp.status} port=${port} set-cookie=[${newCookies}]\n`);
      }
      if (resp.status >= 400) return resp;
      const setCookie = resp.headers['set-cookie'];
      if (setCookie) {
        const incoming = Array.isArray(setCookie)
          ? setCookie.map(c => c.split(';')[0])
          : [setCookie.split(';')[0]];
        const jar = new Map();
        if (this.cookies) {
          this.cookies.split(';').forEach(pair => {
            const trimmed = pair.trim();
            if (trimmed) {
              const eq = trimmed.indexOf('=');
              jar.set(eq === -1 ? trimmed : trimmed.slice(0, eq).trim(), trimmed);
            }
          });
        }
        incoming.forEach(pair => {
          const trimmed = pair.trim();
          if (trimmed) {
            const eq = trimmed.indexOf('=');
            jar.set(eq === -1 ? trimmed : trimmed.slice(0, eq).trim(), trimmed);
          }
        });
        this.cookies = [...jar.values()].join('; ');
      }
      return resp;
    });

    const configHash = crypto.createHash('md5')
      .update(`${config.host}:${config.user}:${config.client}`)
      .digest('hex')
      .substring(0, 8);

    this.sessionFile = path.join(os.tmpdir(), `abapgit-adt-session-${configHash}.json`);
    this.loadSession();
  }

  loadSession() {
    if (!fs.existsSync(this.sessionFile)) return;
    try {
      const session = JSON.parse(fs.readFileSync(this.sessionFile, 'utf8'));
      const safetyMargin = 2 * 60 * 1000;
      if (session.expiresAt > Date.now() + safetyMargin) {
        this.csrfToken = session.csrfToken;
        this.cookies = session.cookies;
      } else {
        this.clearSession();
      }
    } catch (e) {
      this.clearSession();
    }
  }

  saveSession() {
    const expiresAt = Date.now() + (15 * 60 * 1000);
    try {
      fs.writeFileSync(this.sessionFile, JSON.stringify({
        csrfToken: this.csrfToken,
        cookies: this.cookies,
        expiresAt,
        savedAt: Date.now()
      }));
    } catch (e) {
      // Ignore write errors
    }
  }

  clearSession() {
    this.csrfToken = null;
    this.cookies = null;
    try {
      if (fs.existsSync(this.sessionFile)) fs.unlinkSync(this.sessionFile);
    } catch (e) {
      // Ignore deletion errors
    }
  }

  /**
   * Fetch CSRF token via GET /sap/bc/adt/discovery with X-CSRF-Token: fetch
   */
  async fetchCsrfToken() {
    const resp = await this._axios.get('/sap/bc/adt/discovery', {
      headers: {
        'X-CSRF-Token': 'fetch',
        'Accept': 'application/atomsvc+xml'
      }
    });
    this.csrfToken = resp.headers['x-csrf-token'] || this.csrfToken;
    this.saveSession();
    return this.csrfToken;
  }

  /**
   * Make HTTP request to ADT endpoint with automatic retry on auth failure.
   * Returns { body: string, headers: object, statusCode: number }.
   */
  async request(method, urlPath, body = null, options = {}) {
    try {
      return await this._makeRequest(method, urlPath, body, options);
    } catch (error) {
      if (this._isAuthError(error) && !options.isRetry) {
        this.csrfToken = null;
        await this.fetchCsrfToken();
        return await this._makeRequest(method, urlPath, body, { ...options, isRetry: true });
      }
      throw error;
    }
  }

  _isAuthError(error) {
    if (error.statusCode === 401) return true;
    if (error.statusCode === 403) return true;
    const msg = (error.message || '').toLowerCase();
    return msg.includes('csrf') || msg.includes('unauthorized') || msg.includes('forbidden');
  }

  async _makeRequest(method, urlPath, body = null, options = {}) {
    const headers = {
      'Content-Type': options.contentType || 'application/atom+xml',
      'Accept': options.accept || 'application/vnd.sap.as+xml, application/atom+xml, application/xml',
      ...options.headers
    };

    const resp = await this._axios.request({
      method,
      url: urlPath,
      data: body || undefined,
      headers,
      responseType: 'text',
      // Long timeout for listener long-polls (up to 5 min)
      timeout: options.timeout || 300000,
    });

    const statusCode = resp.status;

    if (statusCode === 401) {
      throw { statusCode: 401, message: 'Authentication failed: 401' };
    }
    if (statusCode === 403) {
      throw { statusCode: 403, message: 'Missing debug authorization. Grant S_ADT_RES (ACTVT=16) to user.' };
    }
    if (statusCode === 404) {
      throw { statusCode: 404, message: `HTTP 404: ${urlPath}`, body: resp.data || '' };
    }
    if (statusCode >= 400) {
      const detail = extractBodyDetail(resp.data || '');
      const message = detail
        ? `(HTTP ${statusCode}) ${detail}`
        : `(HTTP ${statusCode}) ${resp.statusText || 'Internal Server Error'}`;
      throw { statusCode, message, body: resp.data || '' };
    }

    return { body: resp.data || '', headers: resp.headers, statusCode };
  }

  async get(urlPath, options = {}) {
    return this.request('GET', urlPath, null, options);
  }

  async post(urlPath, body = null, options = {}) {
    return this.request('POST', urlPath, body, options);
  }

  /**
   * Fire-and-forget POST: sends the request but does not wait for a full response.
   * Used by detach() (stepContinue) where ADT may long-poll indefinitely.
   */
  async postFire(urlPath, body = null, options = {}) {
    const headers = {
      'Content-Type': options.contentType || 'application/atom+xml',
      'Accept': options.accept || 'application/vnd.sap.as+xml, application/atom+xml, application/xml',
      ...options.headers
    };
    try {
      await this._axios.post(urlPath, body || undefined, {
        headers,
        timeout: 5000, // short timeout — we don't need the response
        responseType: 'text',
      });
    } catch (e) {
      // Ignore — fire-and-forget
    }
  }

  async put(urlPath, body = null, options = {}) {
    return this.request('PUT', urlPath, body, options);
  }

  async delete(urlPath, options = {}) {
    return this.request('DELETE', urlPath, null, options);
  }

  /**
   * Extract an attribute value from a simple XML element using regex.
   */
  static extractXmlAttr(xml, tag, attr) {
    if (attr) {
      const re = new RegExp(`<${tag}[^>]*\\s${attr}="([^"]*)"`, 'i');
      const m = xml.match(re);
      return m ? m[1] : null;
    }
    const re = new RegExp(`<${tag}[^>]*>([^<]*)<\/${tag}>`, 'i');
    const m = xml.match(re);
    return m ? m[1].trim() : null;
  }

  /**
   * Extract all occurrences of a tag's content or attribute from XML.
   */
  static extractXmlAll(xml, tag, attr) {
    const results = [];
    if (attr) {
      const re = new RegExp(`<${tag}[^>]*\\s${attr}="([^"]*)"`, 'gi');
      let m;
      while ((m = re.exec(xml)) !== null) results.push(m[1]);
    } else {
      const re = new RegExp(`<${tag}[^>]*>([^<]*)<\/${tag}>`, 'gi');
      let m;
      while ((m = re.exec(xml)) !== null) results.push(m[1].trim());
    }
    return results;
  }
}

module.exports = { AdtHttp };
