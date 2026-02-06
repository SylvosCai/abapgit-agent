/**
 * ABAP Git Agent - Main agent class
 */

const { getClient } = require('./abap-client');
const logger = require('./logger');

class ABAPGitAgent {
  constructor() {
    this.abap = getClient();
  }

  /**
   * Pull repository and activate objects
   * @param {string} repoUrl - Git repository URL
   * @param {string} branch - Branch name (default: main)
   * @param {string} username - Git username (optional)
   * @param {string} password - Git password/token (optional)
   * @returns {object} Pull result with success, job_id, message, error_detail
   */
  async pull(repoUrl, branch = 'main', username = null, password = null) {
    logger.info('Starting pull operation', { repoUrl, branch, username: !!username });

    try {
      const result = await this.abap.pull(repoUrl, branch, username, password);

      // Return the result directly from ABAP
      return {
        success: result.success === 'X' || result.success === true,
        job_id: result.job_id,
        message: result.message,
        error_detail: result.error_detail || null
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
      const result = await this.abap.healthCheck();
      return {
        status: 'healthy',
        abap: 'connected',
        version: result.version || '1.0.0'
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
