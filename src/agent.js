/**
 * ABAP Git Agent - Main agent class
 * Uses AbapHttp for all ABAP communication
 */

const { AbapHttp } = require('./utils/abap-http');
const { getAbapConfig } = require('./config');
const logger = require('./logger');

class ABAPGitAgent {
  constructor() {
    this.config = getAbapConfig();
    this.http = new AbapHttp(this.config);
  }

  /**
   * Pull repository and activate objects
   * @param {string} repoUrl - Git repository URL
   * @param {string} branch - Branch name (default: main)
   * @param {string} username - Git username (optional)
   * @param {string} password - Git password/token (optional)
   * @param {Array} files - Specific files to pull (optional)
   * @param {string} transportRequest - Transport request number (optional)
   * @returns {object} Pull result with success, job_id, message, error_detail
   */
  async pull(repoUrl, branch = 'main', username = null, password = null, files = null, transportRequest = null) {
    logger.info('Starting pull operation', { repoUrl, branch, username: !!username, files, transportRequest });

    try {
      const csrfToken = await this.http.fetchCsrfToken();

      const data = {
        url: repoUrl,
        branch: branch,
        username: username || this.config.gitUsername,
        password: password || this.config.gitPassword
      };

      if (files && files.length > 0) {
        data.files = files;
      }

      if (transportRequest) {
        data.transport_request = transportRequest;
      }

      const result = await this.http.post('/sap/bc/z_abapgit_agent/pull', data, { csrfToken });

      // Return the result directly from ABAP (handle uppercase keys from /UI2/CL_JSON)
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        job_id: result.JOB_ID || result.job_id,
        message: result.MESSAGE || result.message,
        error_detail: result.ERROR_DETAIL || result.error_detail || null,
        activated_count: result.ACTIVATED_COUNT || result.activated_count || 0,
        failed_count: result.FAILED_COUNT || result.failed_count || 0,
        activated_objects: result.ACTIVATED_OBJECTS || result.activated_objects || [],
        failed_objects: result.FAILED_OBJECTS || result.failed_objects || []
      };

    } catch (error) {
      logger.error('Pull failed', { error: error.message });
      throw new Error(`Pull failed: ${error.message}`);
    }
  }

  /**
   * Health check
   * @returns {object} Health status
   */
  async healthCheck() {
    try {
      const result = await this.http.get('/sap/bc/z_abapgit_agent/health');
      return {
        status: 'healthy',
        abap: 'connected',
        version: result.version || '1.4.0'
      };
    } catch (error) {
      return {
        status: 'unhealthy',
        abap: 'disconnected',
        error: error.message
      };
    }
  }

}

module.exports = {
  ABAPGitAgent
};
