/**
 * ABAP Client - Connects to SAP ABAP system via OData/HTTP
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
        baseUrl: `https://${cfg.host}:${cfg.sapport || 443}/sap/opu/odata/sap/Z_ABAPGIT_AGENT_SRV`,
        username: cfg.user,
        password: cfg.password,
        client: cfg.client,
        language: cfg.language || 'EN'
      };
    }
    return this.config;
  }

  /**
   * Make OData request
   */
  async request(method, path, data = null) {
    const cfg = this.getConfig();
    const url = `${cfg.baseUrl}/${path}`;

    logger.debug(`OData request: ${method} ${url}`, data);

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
      const response = await fetch(url, options);

      if (!response.ok) {
        const errorText = await response.text();
        logger.error(`OData request failed`, { status: response.status, body: errorText });
        throw new Error(`OData request failed: ${response.status} ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      logger.error(`OData request error`, { error: error.message, url });
      throw error;
    }
  }

  /**
   * Pull repository and activate
   */
  async pull(repoUrl, branch = 'main') {
    return await this.request('POST', 'PullCommandSet', {
      Url: repoUrl,
      Branch: branch
    });
  }

  /**
   * Get job status
   */
  async getJobStatus(jobId) {
    return await this.request('GET', `JobStatusSet('${jobId}')`);
  }

  /**
   * Get activation log
   */
  async getActivationLog(jobId) {
    const result = await this.request('GET', `LogEntrySet?$filter=JobId eq '${jobId}'`);
    return result.d?.results || [];
  }

  /**
   * Health check - call ping entity
   */
  async healthCheck() {
    try {
      await this.request('GET', 'HealthCheckSet');
      return { status: 'healthy', abap: 'connected' };
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
