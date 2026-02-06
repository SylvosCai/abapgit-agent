/**
 * ABAP Client - Connects to SAP ABAP system via REST/HTTP
 */

const fetch = require('node-fetch');
const { getAbapConfig } = require('./config');
const logger = require('./logger');

class ABAPClient {
  constructor() {
    this.config = null;
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
   * Make REST request
   */
  async request(method, path, data = null, queryParams = {}) {
    const cfg = this.getConfig();

    // Build URL with query params
    const url = new URL(`${cfg.baseUrl}${path}`);
    for (const [key, value] of Object.entries(queryParams)) {
      url.searchParams.append(key, value);
    }

    logger.debug(`REST request: ${method} ${url.toString()}`, data);

    const headers = {
      'Accept': 'application/json',
      'Content-Type': 'application/json',
      'sap-client': cfg.client,
      'sap-language': cfg.language
    };

    if (cfg.username) {
      const auth = Buffer.from(`${cfg.username}:${cfg.password}`).toString('base64');
      headers['Authorization'] = `Basic ${auth}`;
    }

    const options = {
      method,
      headers
    };

    if (data) {
      options.body = JSON.stringify(data);
    }

    try {
      const response = await fetch(url.toString(), options);

      if (!response.ok) {
        const errorText = await response.text();
        logger.error(`REST request failed`, { status: response.status, body: errorText });
        throw new Error(`REST request failed: ${response.status} ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      logger.error(`REST request error`, { error: error.message, url: url.toString() });
      throw error;
    }
  }

  /**
   * Pull repository and activate
   */
  async pull(repoUrl, branch = 'main') {
    return await this.request('POST', '/pull', {
      url: repoUrl,
      branch: branch
    });
  }

  /**
   * Get job status
   */
  async getJobStatus(jobId) {
    return await this.request('GET', '/status', null, { job_id: jobId });
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
