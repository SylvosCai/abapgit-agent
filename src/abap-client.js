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
    this.cookieFile = path.join(__dirname, '..', '.cookies.txt');
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
   * Fetch CSRF token and cookies
   */
  async fetchCsrfToken() {
    const cfg = this.getConfig();
    const url = new URL(`${cfg.baseUrl}/health`);

    return new Promise((resolve, reject) => {
      const options = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname,
        method: 'GET',
        headers: {
          'Authorization': `Basic ${Buffer.from(`${cfg.username}:${cfg.password}`).toString('base64')}`,
          'sap-client': cfg.client,
          'sap-language': cfg.language,
          'X-CSRF-Token': 'fetch'
        },
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = https.request(options, (res) => {
        // Store CSRF token
        this.csrfToken = res.headers['x-csrf-token'];

        // Save cookies
        const setCookie = res.headers['set-cookie'];
        if (setCookie) {
          const cookies = Array.isArray(setCookie)
            ? setCookie.map(c => c.split(';')[0]).join('; ')
            : setCookie.split(';')[0];
          fs.writeFileSync(this.cookieFile, cookies);
        }

        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          resolve({ token: this.csrfToken, body: body });
        });
      });

      req.on('error', reject);
      req.end();
    });
  }

  /**
   * Make HTTP request
   */
  async request(method, path, data = null, options = {}) {
    const cfg = this.getConfig();
    const url = new URL(`${cfg.baseUrl}${path}`);

    return new Promise((resolve, reject) => {
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
      if (method === 'POST' && this.csrfToken) {
        headers['X-CSRF-Token'] = this.csrfToken;
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

        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          try {
            if (res.statusCode >= 400) {
              logger.error(`REST request failed`, { status: res.statusCode, body });
              reject(new Error(`REST request failed: ${res.statusCode} ${res.statusMessage}`));
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
   * Pull repository and activate
   */
  async pull(repoUrl, branch = 'main', username = null, password = null) {
    // Fetch CSRF token first
    await this.fetchCsrfToken();

    const data = {
      url: repoUrl,
      branch: branch
    };
    if (username) data.username = username;
    if (password) data.password = password;

    return await this.request('POST', '/pull', data);
  }

  /**
   * Health check
   */
  async healthCheck() {
    try {
      // Fetch CSRF token for subsequent requests
      await this.fetchCsrfToken();

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
