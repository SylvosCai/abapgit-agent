'use strict';

/**
 * ABAP HTTP client for the custom abapgit-agent REST handler.
 *
 * Uses axios with connection pooling (keepAlive). Handles CSRF tokens,
 * cookie management, session caching, and automatic auth retry.
 *
 * Responses are parsed as JSON (the /sap/bc/z_abapgit_agent/ handler
 * always returns JSON). ABAP-specific newline escaping is applied
 * before parsing.
 */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const path = require('path');
const { extractBodyDetail } = require('./format-error');
const os = require('os');
const crypto = require('crypto');

class AbapHttp {
  constructor(config) {
    this.config = config;
    this.csrfToken = null;
    this.cookies = null;

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

    // Request interceptor: inject auth, CSRF, cookies, sap-client on every request
    this._axios.interceptors.request.use((reqConfig) => {
      reqConfig.headers['Authorization'] =
        `Basic ${Buffer.from(`${config.user}:${config.password}`).toString('base64')}`;
      reqConfig.headers['sap-client'] = config.client;
      reqConfig.headers['sap-language'] = config.language || 'EN';
      if (this.cookies) reqConfig.headers['Cookie'] = this.cookies;
      if (['post', 'delete'].includes(reqConfig.method) && this.csrfToken) {
        reqConfig.headers['X-CSRF-Token'] = this.csrfToken;
      }
      return reqConfig;
    });

    // Response interceptor: merge Set-Cookie headers into cookie jar.
    // Skip on error responses (>= 400) to avoid stale cookie overwrites.
    this._axios.interceptors.response.use((resp) => {
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

    // Session cache file path
    const configHash = crypto.createHash('md5')
      .update(`${config.host}:${config.user}:${config.client}`)
      .digest('hex')
      .substring(0, 8);

    this.sessionFile = path.join(os.tmpdir(), `abapgit-session-${configHash}.json`);
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
   * Detect if error is due to expired/invalid session.
   */
  isAuthError(error) {
    if (error.statusCode === 401) return true;
    if (error.statusCode === 403) return true;

    const message = (error.message || error.error || '').toLowerCase();
    if (message.includes('csrf')) return true;
    if (message.includes('token')) return true;
    if (message.includes('session')) return true;
    if (message.includes('expired')) return true;
    if (message.includes('unauthorized')) return true;
    if (message.includes('forbidden')) return true;

    if (error.body) {
      const bodyStr = JSON.stringify(error.body).toLowerCase();
      if (bodyStr.includes('csrf')) return true;
      if (bodyStr.includes('session')) return true;
      if (bodyStr.includes('expired')) return true;
    }

    return false;
  }

  /**
   * Fetch CSRF token via GET /health with X-CSRF-Token: fetch
   */
  async fetchCsrfToken() {
    const resp = await this._axios.get('/sap/bc/z_abapgit_agent/health', {
      headers: {
        'X-CSRF-Token': 'fetch',
        'Content-Type': 'application/json'
      }
    });
    this.csrfToken = resp.headers['x-csrf-token'] || this.csrfToken;
    this.saveSession();
    return this.csrfToken;
  }

  /**
   * Make HTTP request with automatic retry on auth failure.
   * Returns parsed JSON.
   */
  async request(method, urlPath, data = null, options = {}) {
    try {
      return await this._makeRequest(method, urlPath, data, options);
    } catch (error) {
      if (this.isAuthError(error) && !options.isRetry) {
        this.clearSession();
        await this.fetchCsrfToken();
        return await this._makeRequest(method, urlPath, data, { ...options, isRetry: true });
      }
      throw error;
    }
  }

  /**
   * Internal request — returns parsed JSON.
   */
  async _makeRequest(method, urlPath, data = null, options = {}) {
    const headers = {
      'Content-Type': 'application/json',
      ...options.headers
    };

    const resp = await this._axios.request({
      method,
      url: urlPath,
      data: data ? JSON.stringify(data) : undefined,
      headers,
      responseType: 'text',
      timeout: 120000,
    });

    const statusCode = resp.status;

    if (statusCode === 401 || statusCode === 403) {
      throw { statusCode, message: `Authentication failed: ${statusCode}`, isAuthError: true };
    }

    if (statusCode >= 400) {
      const detail = extractBodyDetail(resp.data || '');
      const message = detail
        ? `(HTTP ${statusCode}) ${detail}`
        : `(HTTP ${statusCode}) ${resp.statusText || 'Internal Server Error'}`;
      throw { statusCode, message, body: resp.data || '' };
    }

    // Parse JSON response — handle ABAP unescaped newlines
    const body = resp.data || '';
    try {
      const cleanedBody = body.replace(/\n/g, '\\n');
      const parsed = JSON.parse(cleanedBody);

      // Check for CSRF/session errors in response body
      if (this.isAuthError(parsed)) {
        throw { statusCode, message: 'CSRF token or session error', body: parsed, isAuthError: true };
      }

      return parsed;
    } catch (e) {
      if (e.isAuthError) throw e;
      // Fallback: try to extract JSON from response
      const jsonMatch = body.match(/\{[\s\S]*\}/);
      if (jsonMatch) {
        try {
          const parsed = JSON.parse(jsonMatch[0].replace(/\n/g, '\\n'));
          return parsed;
        } catch (e2) {
          throw { statusCode, message: 'Failed to parse JSON response', body, error: e2.message };
        }
      }
      throw { statusCode, message: 'Invalid response format (not JSON)', body, error: e.message };
    }
  }

  async get(urlPath, options = {}) {
    return this.request('GET', urlPath, null, options);
  }

  async post(urlPath, data, options = {}) {
    return this.request('POST', urlPath, data, options);
  }

  async delete(urlPath, options = {}) {
    return this.request('DELETE', urlPath, null, options);
  }
}

module.exports = { AbapHttp };
