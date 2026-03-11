'use strict';

/**
 * ADT HTTP client for SAP ABAP Development Tools REST API
 * Handles XML/AtomPub content, CSRF token, cookie session caching.
 */
const https = require('https');
const http = require('http');
const fs = require('fs');
const path = require('path');
const os = require('os');
const crypto = require('crypto');

/**
 * ADT HTTP client with CSRF token, cookie, and session caching.
 * Mirrors AbapHttp but targets /sap/bc/adt/* with XML content-type.
 */
class AdtHttp {
  constructor(config) {
    this.config = config;
    this.csrfToken = null;
    this.cookies = null;

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
    return new Promise((resolve, reject) => {
      const url = new URL('/sap/bc/adt/discovery', `${this.config.protocol || 'https'}://${this.config.host}:${this.config.sapport}`);
      const options = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname,
        method: 'GET',
        headers: {
          'Authorization': `Basic ${Buffer.from(`${this.config.user}:${this.config.password}`).toString('base64')}`,
          'sap-client': this.config.client,
          'sap-language': this.config.language || 'EN',
          'X-CSRF-Token': 'fetch',
          'Accept': 'application/atomsvc+xml'
        },
        agent: this.config.protocol === 'http' ? undefined : new https.Agent({ rejectUnauthorized: false })
      };

      const req = (this.config.protocol === 'http' ? http : https).request(options, (res) => {
        const csrfToken = res.headers['x-csrf-token'];
        const setCookie = res.headers['set-cookie'];
        if (setCookie) {
          this.cookies = Array.isArray(setCookie)
            ? setCookie.map(c => c.split(';')[0]).join('; ')
            : setCookie.split(';')[0];
        }

        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          this.csrfToken = csrfToken;
          this.saveSession();
          resolve(csrfToken);
        });
      });

      req.on('error', reject);
      req.end();
    });
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
        this.clearSession();
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
    return new Promise((resolve, reject) => {
      const url = new URL(urlPath, `${this.config.protocol || 'https'}://${this.config.host}:${this.config.sapport}`);

      const headers = {
        'Content-Type': options.contentType || 'application/atom+xml',
        'Accept': options.accept || 'application/vnd.sap.as+xml, application/atom+xml, application/xml',
        'sap-client': this.config.client,
        'sap-language': this.config.language || 'EN',
        ...options.headers
      };

      headers['Authorization'] = `Basic ${Buffer.from(`${this.config.user}:${this.config.password}`).toString('base64')}`;

      if (['POST', 'PUT', 'DELETE'].includes(method) && this.csrfToken) {
        headers['X-CSRF-Token'] = this.csrfToken;
      }

      if (this.cookies) {
        headers['Cookie'] = this.cookies;
      }

      const bodyStr = body || '';
      headers['Content-Length'] = bodyStr ? Buffer.byteLength(bodyStr) : 0;

      const reqOptions = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname + url.search,
        method,
        headers,
        agent: this.config.protocol === 'http' ? undefined : new https.Agent({ rejectUnauthorized: false })
      };

      const req = (url.protocol === 'https:' ? https : http).request(reqOptions, (res) => {
        if (res.statusCode === 401) {
          reject({ statusCode: 401, message: 'Authentication failed: 401' });
          return;
        }
        if (res.statusCode === 403) {
          const errMsg = 'Missing debug authorization. Grant S_ADT_RES (ACTVT=16) to user.';
          reject({ statusCode: 403, message: errMsg });
          return;
        }
        if (res.statusCode === 404) {
          let respBody = '';
          res.on('data', chunk => respBody += chunk);
          res.on('end', () => {
            reject({ statusCode: 404, message: `HTTP 404: ${reqOptions.path}`, body: respBody });
          });
          return;
        }
        if (res.statusCode >= 400) {
          let respBody = '';
          res.on('data', chunk => respBody += chunk);
          res.on('end', () => {
            reject({ statusCode: res.statusCode, message: `HTTP ${res.statusCode} error`, body: respBody });
          });
          return;
        }

        // Update cookies from any response that sets them.
        // Merge by name so that updated values (e.g. SAP_SESSIONID) replace
        // their old counterparts rather than accumulating duplicates.
        // Duplicate SAP_SESSIONID cookies would cause the ICM to route requests
        // to a stale work process ("Service cannot be reached").
        if (res.headers['set-cookie']) {
          const incoming = Array.isArray(res.headers['set-cookie'])
            ? res.headers['set-cookie'].map(c => c.split(';')[0])
            : [res.headers['set-cookie'].split(';')[0]];
          // Parse existing cookies into a Map (preserves insertion order)
          const jar = new Map();
          if (this.cookies) {
            this.cookies.split(';').forEach(pair => {
              const trimmed = pair.trim();
              if (trimmed) {
                const eq = trimmed.indexOf('=');
                const k = eq === -1 ? trimmed : trimmed.slice(0, eq);
                jar.set(k.trim(), trimmed);
              }
            });
          }
          // Overwrite with incoming cookies
          incoming.forEach(pair => {
            const trimmed = pair.trim();
            if (trimmed) {
              const eq = trimmed.indexOf('=');
              const k = eq === -1 ? trimmed : trimmed.slice(0, eq);
              jar.set(k.trim(), trimmed);
            }
          });
          this.cookies = [...jar.values()].join('; ');
        }

        let respBody = '';
        res.on('data', chunk => respBody += chunk);
        res.on('end', () => {
          resolve({ body: respBody, headers: res.headers, statusCode: res.statusCode });
        });
      });

      req.on('error', reject);
      if (bodyStr) req.write(bodyStr);
      req.end();
    });
  }

  async get(urlPath, options = {}) {
    return this.request('GET', urlPath, null, options);
  }

  async post(urlPath, body = null, options = {}) {
    return this.request('POST', urlPath, body, options);
  }

  /**
   * Fire-and-forget POST: resolves when the request bytes have been flushed
   * to the TCP send buffer — does NOT wait for a response.
   *
   * Used by detach() (stepContinue) where:
   *   - ADT long-polls until the next breakpoint fires → response may never come
   *   - We only need ADT to *receive* the request, not respond to it
   *   - Using the existing stateful session (cookies/CSRF) is mandatory
   *
   * The socket is deliberately left open so the OS TCP stack can finish
   * delivering the data to ADT after we return from this method.
   *
   * @param {string} urlPath - URL path
   * @param {string} body    - Request body (may be empty string)
   * @param {object} options - Same options as post() (contentType, headers, etc.)
   * @returns {Promise<void>} Resolves when req.end() callback fires
   */
  async postFire(urlPath, body = null, options = {}) {
    return new Promise((resolve, reject) => {
      const url = new URL(urlPath, `${this.config.protocol || 'https'}://${this.config.host}:${this.config.sapport}`);

      const headers = {
        'Content-Type': options.contentType || 'application/atom+xml',
        'Accept': options.accept || 'application/vnd.sap.as+xml, application/atom+xml, application/xml',
        'sap-client': this.config.client,
        'sap-language': this.config.language || 'EN',
        ...options.headers
      };

      headers['Authorization'] = `Basic ${Buffer.from(`${this.config.user}:${this.config.password}`).toString('base64')}`;

      if (this.csrfToken) {
        headers['X-CSRF-Token'] = this.csrfToken;
      }

      if (this.cookies) {
        headers['Cookie'] = this.cookies;
      }

      const bodyStr = body || '';
      headers['Content-Length'] = bodyStr ? Buffer.byteLength(bodyStr) : 0;

      const reqOptions = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname + url.search,
        method: 'POST',
        headers,
        agent: this.config.protocol === 'http' ? undefined : new https.Agent({ rejectUnauthorized: false })
      };

      const req = (url.protocol === 'https:' ? https : http).request(reqOptions, (_res) => {
        // Drain response body to prevent socket hang; we don't use the data.
        _res.resume();
      });

      // Resolve as soon as the request is fully written and flushed.
      req.on('error', resolve); // ignore errors — fire-and-forget
      if (bodyStr) req.write(bodyStr);
      req.end(() => resolve());
    });
  }

  async put(urlPath, body = null, options = {}) {
    return this.request('PUT', urlPath, body, options);
  }

  async delete(urlPath, options = {}) {
    return this.request('DELETE', urlPath, null, options);
  }

  /**
   * Extract an attribute value from a simple XML element using regex.
   * e.g. extractXmlAttr(xml, 'adtcore:uri', null) for text content
   *      extractXmlAttr(xml, 'entry', 'id') for attribute
   * @param {string} xml - XML string
   * @param {string} tag - Tag name (may include namespace prefix)
   * @param {string|null} attr - Attribute name, or null for text content
   * @returns {string|null} Extracted value or null
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
   * @param {string} xml - XML string
   * @param {string} tag - Tag name
   * @param {string|null} attr - Attribute name, or null for text content
   * @returns {string[]} Array of matched values
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
