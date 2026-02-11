/**
 * Command Client - Uses new command-based API for ABAP Git Agent
 */

// Command constants - must match ABAP ZIF_ABGAGT_COMMAND constants
const COMMANDS = {
  PULL: 'PULL',
  INSPECT: 'INSPECT',
  UNIT: 'UNIT'
};

const https = require('https');
const http = require('http');
const fs = require('fs');
const path = require('path');
const { getAbapConfig } = require('./config');
const logger = require('./logger');

class CommandClient {
  constructor() {
    this.config = null;
    this.cookieFile = path.join(__dirname, '..', '.abapgit_agent_cookies.txt');
    this.csrfToken = null;
    this.useCommandApi = true; // Feature flag for gradual rollout
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
        language: cfg.language || 'EN',
        gitUsername: cfg.gitUsername,
        gitPassword: cfg.gitPassword
      };
    }
    return this.config;
  }

  /**
   * Read cookies from Netscape format cookie file
   */
  readNetscapeCookies() {
    if (!fs.existsSync(this.cookieFile)) return '';

    const content = fs.readFileSync(this.cookieFile, 'utf8');
    const lines = content.split('\n');
    const cookies = [];

    for (const line of lines) {
      const trimmed = line.trim();
      // Skip empty lines and only the header comments (starting with #)
      // but NOT HttpOnly cookies which start with #HttpOnly_
      if (!trimmed || (trimmed.startsWith('#') && !trimmed.startsWith('#HttpOnly'))) continue;

      const parts = trimmed.split('\t');
      if (parts.length >= 7) {
        // Format: domain, flag, path, secure, expiration, name, value
        cookies.push(`${parts[5]}=${parts[6]}`);
      }
    }

    return cookies.join('; ');
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

      // Add cookies if available (handle Netscape format)
      const cookieHeader = this.readNetscapeCookies();
      if (cookieHeader) {
        headers['Cookie'] = cookieHeader;
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

        // Get CSRF token from response headers
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
   * Fetch CSRF token using GET /command with X-CSRF-Token: fetch
   */
  async fetchCsrfToken() {
    const cfg = this.getConfig();

    return new Promise((resolve, reject) => {
      const url = new URL(`${cfg.baseUrl}/command`);

      // Clear stale cookies before fetching new token
      if (fs.existsSync(this.cookieFile)) {
        fs.unlinkSync(this.cookieFile);
      }

      // Read cookies for sending (handle Netscape format)
      const cookieHeader = this.readNetscapeCookies();

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
          'Content-Type': 'application/json',
          ...(cookieHeader && { 'Cookie': cookieHeader })
        },
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = https.request(options, (res) => {
        const csrfToken = res.headers['x-csrf-token'];

        // Save new cookies from response
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
          // Store token in instance for use by POST
          this.csrfToken = csrfToken;
          resolve({ token: csrfToken });
        });
      });

      req.on('error', reject);
      req.end();
    });
  }

  /**
   * Execute command via new /command endpoint
   */
  async execute(command, params = {}) {
    const cfg = this.getConfig();

    // Fetch CSRF token first
    await this.fetchCsrfToken();

    const data = {
      command,
      ...params
    };

    logger.info(`Executing command: ${command}`, { params: Object.keys(params) });

    return await this.request('POST', '/command', data, { csrfToken: this.csrfToken });
  }

  /**
   * Pull repository and activate (using command API)
   * @param {string} repoUrl - Repository URL
   * @param {string} branch - Branch name (default: 'main')
   * @param {string} gitUsername - Git username (optional)
   * @param {string} gitPassword - Git password/token (optional)
   * @param {Array} files - Array of file paths to pull (optional)
   * @param {string} transportRequest - Transport request number (optional)
   * @returns {object} Pull result
   */
  async pull(repoUrl, branch = 'main', gitUsername = null, gitPassword = null, files = null, transportRequest = null) {
    const cfg = this.getConfig();

    const params = {
      url: repoUrl,
      branch: branch || 'main'
    };

    // Add files if specified
    if (files && files.length > 0) {
      params.files = files;
    }

    // Add transport request if specified
    if (transportRequest) {
      params.transport_request = transportRequest;
    }

    // Use config git credentials if no override provided
    params.username = gitUsername || cfg.gitUsername;
    params.password = gitPassword || cfg.gitPassword;

    logger.info('Starting pull operation via command API', { repoUrl, branch, transportRequest, service: 'abapgit-agent' });

    return await this.execute(COMMANDS.PULL, params);
  }

  /**
   * Check syntax of an ABAP object (using command API)
   */
  async syntaxCheck(objectType, objectName) {
    // For syntax check, we need to find the file first
    // This is a simplified version - full implementation would look up the file
    logger.info('Starting syntax check', { objectType, objectName, service: 'abapgit-agent' });

    // Map to INSPECT command
    const params = {
      source_name: `${objectName.toLowerCase()}.${this.mapTypeToExt(objectType)}.abap`
    };

    return await this.execute(COMMANDS.INSPECT, params);
  }

  /**
   * Map ABAP object type to file extension
   */
  mapTypeToExt(objType) {
    const typeMap = {
      'CLAS': 'clas',
      'INTF': 'intf',
      'PROG': 'prog',
      'FUGR': 'fugr',
      'TABL': 'tabl',
      'DDLS': 'ddls'
    };
    return typeMap[objType.toUpperCase()] || objType.toLowerCase();
  }

  /**
   * Run unit tests for package or objects (using command API)
   */
  async unitTest(packageName = null, objects = []) {
    const params = {};

    if (packageName) {
      params.package = packageName;
    }

    if (objects && objects.length > 0) {
      params.objects = objects;
    }

    logger.info('Starting unit tests via command API', { package: packageName, objects, service: 'abapgit-agent' });

    return await this.execute(COMMANDS.UNIT, params);
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

  /**
   * Enable/disable command API
   */
  setUseCommandApi(enabled) {
    this.useCommandApi = enabled;
  }

  /**
   * Check if command API is enabled
   */
  isUsingCommandApi() {
    return this.useCommandApi;
  }
}

// Singleton instance
let instance = null;

function getClient() {
  if (!instance) {
    instance = new CommandClient();
  }
  return instance;
}

module.exports = {
  CommandClient,
  getClient
};
