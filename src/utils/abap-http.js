/**
 * ABAP HTTP request wrapper with CSRF token management
 */
const https = require('https');
const http = require('http');

/**
 * ABAP HTTP client with CSRF token and cookie management (in-memory)
 */
class AbapHttp {
  constructor(config) {
    this.config = config;
    this.csrfToken = null;
    this.cookies = null;
  }

  /**
   * Fetch CSRF token using GET /health with X-CSRF-Token: fetch
   * @returns {Promise<string>} CSRF token
   */
  async fetchCsrfToken() {
    const url = new URL(`/sap/bc/z_abapgit_agent/health`, `https://${this.config.host}:${this.config.sapport}`);

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
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = https.request(options, (res) => {
        const csrfToken = res.headers['x-csrf-token'];

        // Save cookies in memory from response
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
          resolve(csrfToken);
        });
      });

      req.on('error', reject);
      req.end();
    });
  }

  /**
   * Make HTTP request to ABAP REST endpoint
   * @param {string} method - HTTP method (GET, POST, DELETE)
   * @param {string} urlPath - URL path
   * @param {object} data - Request body (for POST)
   * @param {object} options - Additional options (headers, csrfToken)
   * @returns {Promise<object>} Response JSON
   */
  async request(method, urlPath, data = null, options = {}) {
    return new Promise((resolve, reject) => {
      const url = new URL(urlPath, `https://${this.config.host}:${this.config.sapport}`);

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

      // Add cookies from memory if available
      if (this.cookies) {
        headers['Cookie'] = this.cookies;
      }

      const reqOptions = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname + url.search,
        method,
        headers,
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = (url.protocol === 'https:' ? https : http).request(reqOptions, (res) => {
        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          try {
            // Handle unescaped newlines from ABAP - replace actual newlines with \n
            const cleanedBody = body.replace(/\n/g, '\\n');
            resolve(JSON.parse(cleanedBody));
          } catch (e) {
            // Fallback: try to extract JSON from response
            const jsonMatch = body.match(/\{[\s\S]*\}/);
            if (jsonMatch) {
              try {
                resolve(JSON.parse(jsonMatch[0].replace(/\n/g, '\\n')));
              } catch (e2) {
                resolve({ raw: body, error: e2.message });
              }
            } else {
              resolve({ raw: body, error: e.message });
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
