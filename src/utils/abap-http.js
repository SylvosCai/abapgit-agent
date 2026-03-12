/**
 * ABAP HTTP request wrapper with CSRF token and session management
 */
const https = require('https');
const http = require('http');
const { extractBodyDetail } = require('./format-error');
const fs = require('fs');
const path = require('path');
const os = require('os');
const crypto = require('crypto');

/**
 * ABAP HTTP client with CSRF token, cookie, and session caching
 */
class AbapHttp {
  constructor(config) {
    this.config = config;
    this.csrfToken = null;
    this.cookies = null;

    // Session cache file path
    const configHash = crypto.createHash('md5')
      .update(`${config.host}:${config.user}:${config.client}`)
      .digest('hex')
      .substring(0, 8);

    this.sessionFile = path.join(os.tmpdir(), `abapgit-session-${configHash}.json`);

    // Try to load cached session
    this.loadSession();
  }

  /**
   * Load session from cache file if valid
   */
  loadSession() {
    if (!fs.existsSync(this.sessionFile)) {
      return;
    }

    try {
      const session = JSON.parse(fs.readFileSync(this.sessionFile, 'utf8'));

      // Check if expired (with 2-minute safety margin)
      const now = Date.now();
      const safetyMargin = 2 * 60 * 1000;  // 2 minutes

      if (session.expiresAt > now + safetyMargin) {
        this.csrfToken = session.csrfToken;
        this.cookies = session.cookies;
        // Silent - no console output for cached session
      } else {
        // Session expired
        this.clearSession();
      }
    } catch (e) {
      // Corrupted cache file - clear it
      this.clearSession();
    }
  }

  /**
   * Save session to cache file
   */
  saveSession() {
    // Conservative expiration: 15 minutes
    // (ABAP default session timeout is typically 20 minutes)
    const expiresAt = Date.now() + (15 * 60 * 1000);

    try {
      fs.writeFileSync(this.sessionFile, JSON.stringify({
        csrfToken: this.csrfToken,
        cookies: this.cookies,
        expiresAt,
        savedAt: Date.now()
      }));
    } catch (e) {
      // Ignore write errors - session caching is optional
    }
  }

  /**
   * Clear cached session
   */
  clearSession() {
    this.csrfToken = null;
    this.cookies = null;

    try {
      if (fs.existsSync(this.sessionFile)) {
        fs.unlinkSync(this.sessionFile);
      }
    } catch (e) {
      // Ignore file deletion errors
    }
  }

  /**
   * Detect if error is due to expired/invalid session
   * @param {Error|object} error - Error object or response
   * @returns {boolean} True if auth error
   */
  isAuthError(error) {
    // HTTP status codes
    if (error.statusCode === 401) return true;  // Unauthorized
    if (error.statusCode === 403) return true;  // Forbidden

    // Error message patterns
    const message = (error.message || error.error || '').toLowerCase();
    if (message.includes('csrf')) return true;
    if (message.includes('token')) return true;
    if (message.includes('session')) return true;
    if (message.includes('expired')) return true;
    if (message.includes('unauthorized')) return true;
    if (message.includes('forbidden')) return true;

    // Response body patterns
    if (error.body) {
      const bodyStr = JSON.stringify(error.body).toLowerCase();
      if (bodyStr.includes('csrf')) return true;
      if (bodyStr.includes('session')) return true;
      if (bodyStr.includes('expired')) return true;
    }

    return false;
  }

  /**
   * Fetch CSRF token using GET /health with X-CSRF-Token: fetch
   * @returns {Promise<string>} CSRF token
   */
  async fetchCsrfToken() {
    const url = new URL(`/sap/bc/z_abapgit_agent/health`, `${this.config.protocol || 'https'}://${this.config.host}:${this.config.sapport}`);

    return new Promise((resolve, reject) => {
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
          'Content-Type': 'application/json'
        },
        agent: this.config.protocol === 'http' ? undefined : new https.Agent({ rejectUnauthorized: false })
      };

      const req = (this.config.protocol === 'http' ? http : https).request(options, (res) => {
        const csrfToken = res.headers['x-csrf-token'];

        // Save cookies from response
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

          // Save session to cache
          this.saveSession();

          resolve(csrfToken);
        });
      });

      req.on('error', reject);
      req.end();
    });
  }

  /**
   * Make HTTP request to ABAP REST endpoint with automatic retry on auth failure
   * @param {string} method - HTTP method (GET, POST, DELETE)
   * @param {string} urlPath - URL path
   * @param {object} data - Request body (for POST)
   * @param {object} options - Additional options (headers, csrfToken, isRetry)
   * @returns {Promise<object>} Response JSON
   */
  async request(method, urlPath, data = null, options = {}) {
    try {
      // Try with current session (cached or fresh)
      return await this._makeRequest(method, urlPath, data, options);

    } catch (error) {
      // Check if it's an authentication/authorization failure
      if (this.isAuthError(error) && !options.isRetry) {
        // Session expired - refresh and retry once
        console.error('⚠️  Session expired, refreshing...');

        // Clear stale session
        this.clearSession();

        // Fetch fresh token/cookies
        await this.fetchCsrfToken();

        // Retry ONCE with fresh session
        return await this._makeRequest(method, urlPath, data, {
          ...options,
          isRetry: true  // Prevent infinite loop
        });
      }

      // Not an auth error or already retried - propagate
      throw error;
    }
  }

  /**
   * Internal request implementation (no retry logic)
   * @param {string} method - HTTP method
   * @param {string} urlPath - URL path
   * @param {object} data - Request body
   * @param {object} options - Additional options
   * @returns {Promise<object>} Response JSON
   */
  async _makeRequest(method, urlPath, data = null, options = {}) {
    return new Promise((resolve, reject) => {
      const url = new URL(urlPath, `${this.config.protocol || 'https'}://${this.config.host}:${this.config.sapport}`);

      const headers = {
        'Content-Type': 'application/json',
        'sap-client': this.config.client,
        'sap-language': this.config.language || 'EN',
        ...options.headers
      };

      // Add authorization
      headers['Authorization'] = `Basic ${Buffer.from(`${this.config.user}:${this.config.password}`).toString('base64')}`;

      // Add CSRF token for POST
      if (method === 'POST' && options.csrfToken) {
        headers['X-CSRF-Token'] = options.csrfToken;
      }

      // Add cookies if available
      if (this.cookies) {
        headers['Cookie'] = this.cookies;
      }

      const reqOptions = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname + url.search,
        method,
        headers,
        agent: this.config.protocol === 'http' ? undefined : new https.Agent({ rejectUnauthorized: false })
      };

      const req = (url.protocol === 'https:' ? https : http).request(reqOptions, (res) => {
        // Check for auth errors
        if (res.statusCode === 401 || res.statusCode === 403) {
          reject({
            statusCode: res.statusCode,
            message: `Authentication failed: ${res.statusCode}`,
            isAuthError: true
          });
          return;
        }

        // Check for other HTTP errors (4xx, 5xx)
        if (res.statusCode >= 400) {
          let body = '';
          res.on('data', chunk => body += chunk);
          res.on('end', () => {
            const detail = extractBodyDetail(body);
            const message = detail
              ? `(HTTP ${res.statusCode}) ${detail}`
              : `(HTTP ${res.statusCode}) ${res.statusMessage || 'Internal Server Error'}`;
            reject({
              statusCode: res.statusCode,
              message,
              body: body
            });
          });
          return;
        }

        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          try {
            // Handle unescaped newlines from ABAP - replace actual newlines with \n
            const cleanedBody = body.replace(/\n/g, '\\n');
            const parsed = JSON.parse(cleanedBody);

            // Check for CSRF/session errors in response body
            if (this.isAuthError(parsed)) {
              reject({
                statusCode: res.statusCode,
                message: 'CSRF token or session error',
                body: parsed,
                isAuthError: true
              });
              return;
            }

            resolve(parsed);
          } catch (e) {
            // Fallback: try to extract JSON from response
            const jsonMatch = body.match(/\{[\s\S]*\}/);
            if (jsonMatch) {
              try {
                const parsed = JSON.parse(jsonMatch[0].replace(/\n/g, '\\n'));
                resolve(parsed);
              } catch (e2) {
                // JSON parse failed - reject instead of resolve
                reject({
                  statusCode: res.statusCode,
                  message: 'Failed to parse JSON response',
                  body: body,
                  error: e2.message
                });
              }
            } else {
              // No JSON found in body - reject instead of resolve
              reject({
                statusCode: res.statusCode,
                message: 'Invalid response format (not JSON)',
                body: body,
                error: e.message
              });
            }
          }
        });
      });

      req.on('error', reject);

      if (data) {
        req.write(JSON.stringify(data));
      }
      req.end();
    });
  }

  /**
   * Convenience method for GET requests
   * @param {string} urlPath - URL path
   * @param {object} options - Additional options
   * @returns {Promise<object>} Response JSON
   */
  async get(urlPath, options = {}) {
    return this.request('GET', urlPath, null, options);
  }

  /**
   * Convenience method for POST requests
   * @param {string} urlPath - URL path
   * @param {object} data - Request body
   * @param {object} options - Additional options (must include csrfToken)
   * @returns {Promise<object>} Response JSON
   */
  async post(urlPath, data, options = {}) {
    return this.request('POST', urlPath, data, options);
  }

  /**
   * Convenience method for DELETE requests
   * @param {string} urlPath - URL path
   * @param {object} options - Additional options
   * @returns {Promise<object>} Response JSON
   */
  async delete(urlPath, options = {}) {
    return this.request('DELETE', urlPath, null, options);
  }
}

module.exports = {
  AbapHttp
};
