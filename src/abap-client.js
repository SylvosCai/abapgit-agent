/**
 * ABAP Client - Connects to SAP ABAP system via REST/HTTP
 */

const https = require('https');
const http = require('http');
const fs = require('fs');
const path = require('path');
const { getAbapConfig } = require('./config');
const logger = require('./logger');

class ABAPClient {
  constructor() {
    this.config = null;
    this.cookieFile = path.join(__dirname, '..', '.abapgit_agent_cookies.txt');
    this.csrfToken = null;
  }

  /**
   * Get ABAP configuration
   */
  getConfig() {
    if (!this.config) {
      const cfg = getAbapConfig();
      this.config = {
        baseUrl: `https://${cfg.host}:${cfg.sapport || 44300}/sap/bc/z_abapgit_agent`,
        username: cfg.user,
        password: cfg.password,
        client: cfg.client,
        language: cfg.language || 'EN'
      };
    }
    return this.config;
  }

  /**
   * Make HTTP request
   */
  async request(method, path, data = null, options = {}) {
    const cfg = this.getConfig();

    return new Promise((resolve, reject) => {
      const url = new URL(`${cfg.baseUrl}${path}`);

      const headers = {
        'Content-Type': 'application/json',
        'sap-client': cfg.client,
        'sap-language': cfg.language
      };

      // Add authorization
      if (cfg.username) {
        headers['Authorization'] = `Basic ${Buffer.from(`${cfg.username}:${cfg.password}`).toString('base64')}`;
      }

      // Add CSRF token for POST
      if (method === 'POST' && options.csrfToken) {
        headers['X-CSRF-Token'] = options.csrfToken;
      }

      // Add cookies if available
      if (fs.existsSync(this.cookieFile)) {
        const cookies = fs.readFileSync(this.cookieFile, 'utf8');
        headers['Cookie'] = cookies;
      }

      const reqOptions = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname,
        method,
        headers,
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = (url.protocol === 'https:' ? https : http).request(reqOptions, (res) => {
        // Update cookies
        const setCookie = res.headers['set-cookie'];
        if (setCookie) {
          const cookies = Array.isArray(setCookie)
            ? setCookie.map(c => c.split(';')[0]).join('; ')
            : setCookie.split(';')[0];
          fs.writeFileSync(this.cookieFile, cookies);
        }

        // Get CSRF token from response headers (for GET /pull with fetch)
        if (res.headers['x-csrf-token'] && !this.csrfToken) {
          this.csrfToken = res.headers['x-csrf-token'];
        }

        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          try {
            if (res.statusCode >= 400) {
              logger.error(`REST request failed`, { status: res.statusCode, body });
              reject(new Error(`REST request failed: ${res.statusCode}`));
            } else if (body) {
              resolve(JSON.parse(body));
            } else {
              resolve({});
            }
          } catch (e) {
            resolve(body);
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
   * Fetch CSRF token using GET /pull with X-CSRF-Token: fetch
   */
  async fetchCsrfToken() {
    const cfg = this.getConfig();

    return new Promise((resolve, reject) => {
      const url = new URL(`${cfg.baseUrl}/pull`);

      const options = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname,
        method: 'GET',
        headers: {
          'Authorization': `Basic ${Buffer.from(`${cfg.username}:${cfg.password}`).toString('base64')}`,
          'sap-client': cfg.client,
          'sap-language': cfg.language,
          'X-CSRF-Token': 'fetch',
          'Content-Type': 'application/json'
        },
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = https.request(options, (res) => {
        const setCookie = res.headers['set-cookie'];
        if (setCookie) {
          const cookies = Array.isArray(setCookie)
            ? setCookie.map(c => c.split(';')[0]).join('; ')
            : setCookie.split(';')[0];
          fs.writeFileSync(this.cookieFile, cookies);
        }

        this.csrfToken = res.headers['x-csrf-token'];

        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          resolve({ token: this.csrfToken });
        });
      });

      req.on('error', reject);
      req.end();
    });
  }

  /**
   * Pull repository and activate
   */
  async pull(repoUrl, branch = 'main', username = null, password = null) {
    const cfg = this.getConfig();

    // Fetch CSRF token first (using GET /pull with X-CSRF-Token: fetch)
    await this.fetchCsrfToken();

    const data = {
      url: repoUrl,
      branch: branch
    };
    // Use config credentials if no override provided
    data.username = username || cfg.username;
    data.password = password || cfg.password;

    return await this.request('POST', '/pull', data, { csrfToken: this.csrfToken });
  }

  /**
   * Health check
   */
  async healthCheck() {
    try {
      const result = await this.request('GET', '/health');
      return { status: 'healthy', abap: 'connected', ...result };
    } catch (error) {
      return { status: 'unhealthy', abap: 'disconnected', error: error.message };
    }
  }
}

// Singleton instance
let instance = null;

function getClient() {
  if (!instance) {
    instance = new ABAPClient();
  }
  return instance;
}

module.exports = {
  ABAPClient,
  getClient
};
