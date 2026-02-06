/**
 * ABAP Git Agent - Main agent class
 */

const { getClient } = require('./abap-client');
const { getAgentConfig } = require('./config');
const logger = require('./logger');

class ABAPGitAgent {
  constructor() {
    this.abap = getClient();
    this.agentConfig = getAgentConfig();
    this.jobs = new Map();  // Store job statuses in memory
  }

  /**
   * Pull repository and activate objects
   * @param {string} repoUrl - Git repository URL
   * @param {string} branch - Branch name (default: main)
   * @param {string} username - Git username (optional)
   * @param {string} password - Git password/token (optional)
   * @returns {object} Job information
   */
  async pull(repoUrl, branch = 'main', username = null, password = null) {
    logger.info('Starting pull operation', { repoUrl, branch, username: !!username });

    try {
      // Call REST API to start background job
      const result = await this.abap.pull(repoUrl, branch, username, password);

      // Handle OData response format
      const responseData = result.d || result;

      const jobId = responseData.JobId || responseData.jobId;

      // Store job info
      const jobInfo = {
        id: jobId,
        repoUrl,
        branch,
        status: responseData.Status || 'RUNNING',
        startedAt: new Date().toISOString(),
        message: responseData.Message || responseData.message
      };

      this.jobs.set(jobId, jobInfo);

      logger.info('Pull job started', { jobId });

      return {
        success: true,
        jobId,
        status: jobInfo.status,
        message: jobInfo.message,
        pollUrl: `/api/jobs/${jobId}`
      };

    } catch (error) {
      logger.error('Failed to start pull job', { error: error.message });
      throw new Error(`Failed to start pull: ${error.message}`);
    }
  }

  /**
   * Get job status and results
   * @param {string} jobId - Job ID
   * @returns {object} Job status and results
   */
  async getJobStatus(jobId) {
    logger.debug('Getting job status', { jobId });

    try {
      // Get status from OData
      const result = await this.abap.getJobStatus(jobId);
      const statusData = result.d || result;

      // Get error log from OData
      const logResult = await this.abap.getActivationLog(jobId);
      const errorLog = (logResult.d?.results || logResult).map(entry =>
        entry.Message || entry.message
      );

      const jobInfo = this.jobs.get(jobId) || {};

      return {
        jobId,
        status: statusData.Status || statusData.status,
        success: statusData.Success || statusData.success,
        message: statusData.Message || statusData.message,
        activatedCount: parseInt(statusData.ActivatedCount || statusData.activatedCount || '0', 10),
        failedCount: parseInt(statusData.FailedCount || statusData.failedCount || '0', 10),
        errorLog,
        startedAt: jobInfo.startedAt,
        finishedAt: new Date().toISOString()
      };

    } catch (error) {
      logger.error('Failed to get job status', { jobId, error: error.message });
      throw new Error(`Failed to get job status: ${error.message}`);
    }
  }

  /**
   * Poll for job completion
   * @param {string} jobId - Job ID
   * @param {number} maxWait - Maximum wait time in ms
   * @returns {object} Final job result
   */
  async waitForJob(jobId, maxWait = 300000) {
    const pollInterval = this.agentConfig.pollInterval || 5000;
    const startTime = Date.now();

    while (Date.now() - startTime < maxWait) {
      const status = await this.getJobStatus(jobId);

      if (status.status !== 'RUNNING') {
        return status;
      }

      logger.debug('Job still running, waiting...', { jobId });
      await this.sleep(pollInterval);
    }

    throw new Error(`Timeout waiting for job ${jobId}`);
  }

  /**
   * Health check
   */
  async healthCheck() {
    try {
      const result = await this.abap.healthCheck();
      const healthData = result.d || result;
      return {
        status: healthData.Status === 'OK' ? 'healthy' : 'unhealthy',
        abap: 'connected',
        version: healthData.Version || healthData.version
      };
    } catch (error) {
      return {
        status: 'unhealthy',
        abap: 'disconnected',
        error: error.message
      };
    }
  }

  /**
   * Sleep utility
   */
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Get list of all jobs
   */
  getAllJobs() {
    return Array.from(this.jobs.values());
  }

  /**
   * Clean up old jobs
   */
  cleanupOldJobs(maxAge = 3600000) { // 1 hour default
    const now = Date.now();
    for (const [jobId, job] of this.jobs) {
      if (now - new Date(job.startedAt).getTime() > maxAge) {
        this.jobs.delete(jobId);
      }
    }
  }
}

module.exports = {
  ABAPGitAgent
};
