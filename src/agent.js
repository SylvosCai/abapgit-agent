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
   * @param {Array} files - Specific files to pull (optional)
   * @returns {object} Pull result with success, job_id, message, error_detail
   */
  async pull(repoUrl, branch = 'main', username = null, password = null, files = null) {
    logger.info('Starting pull operation', { repoUrl, branch, username: !!username, files });

    try {
      const result = await this.abap.pull(repoUrl, branch, username, password, files);

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

  /**
   * Check syntax of an ABAP object
   * @param {string} objectType - ABAP object type (e.g., 'CLAS', 'PROG', 'INTF')
   * @param {string} objectName - ABAP object name
   * @returns {object} Syntax check result with errors (if any)
   */
  async syntaxCheck(objectType, objectName) {
    logger.info('Starting syntax check', { objectType, objectName });

    try {
      const result = await this.abap.syntaxCheck(objectType, objectName);
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        object_type: result.OBJECT_TYPE || result.object_type,
        object_name: result.OBJECT_NAME || result.object_name,
        error_count: result.ERROR_COUNT || result.error_count || 0,
        errors: result.ERRORS || result.errors || []
      };
    } catch (error) {
      logger.error('Syntax check failed', { error: error.message });
      throw new Error(`Syntax check failed: ${error.message}`);
    }
  }

  /**
   * Run unit tests for package or objects
   * @param {string} packageName - Package name to run tests for (optional)
   * @param {Array} objects - Array of {object_type, object_name} objects (optional)
   * @returns {object} Unit test results with test_count, passed_count, failed_count, results
   */
  async unitCheck(packageName = null, objects = []) {
    logger.info('Starting unit tests', { package: packageName, objects });

    try {
      const result = await this.abap.unitTest(packageName, objects);
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        test_count: result.TEST_COUNT || result.test_count || 0,
        passed_count: result.PASSED_COUNT || result.passed_count || 0,
        failed_count: result.FAILED_COUNT || result.failed_count || 0,
        message: result.MESSAGE || result.message || '',
        errors: result.ERRORS || result.errors || []
      };
    } catch (error) {
      logger.error('Unit tests failed', { error: error.message });
      throw new Error(`Unit tests failed: ${error.message}`);
    }
  }

  /**
   * Create a new online repository
   * @param {string} repoUrl - Git repository URL
   * @param {string} packageName - ABAP package name
   * @param {string} branch - Branch name (default: 'main')
   * @param {string} displayName - Display name for the repository (optional)
   * @param {string} name - Repository name (optional)
   * @param {string} folderLogic - Folder logic: 'PREFIX' or 'FULL' (default: 'PREFIX')
   * @returns {object} Create result with success, repo_key, repo_name, message
   */
  async create(repoUrl, packageName, branch = 'main', displayName = null, name = null, folderLogic = 'PREFIX') {
    logger.info('Creating repository', { repoUrl, packageName, branch });

    try {
      const result = await this.abap.create(repoUrl, packageName, branch, displayName, name, folderLogic);
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        repo_key: result.REPO_KEY || result.repo_key,
        repo_name: result.REPO_NAME || result.repo_name,
        message: result.MESSAGE || result.message || '',
        error: result.ERROR || result.error || null
      };
    } catch (error) {
      logger.error('Create failed', { error: error.message });
      throw new Error(`Create failed: ${error.message}`);
    }
  }

  /**
   * Import existing objects from package to git repository
   * @param {string} repoUrl - Git repository URL
   * @param {string} message - Commit message (optional)
   * @returns {object} Import result with success, files_staged, commit_sha
   */
  async import(repoUrl, message = null) {
    logger.info('Starting import operation', { repoUrl, message });

    try {
      const result = await this.abap.import(repoUrl, message);
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        files_staged: result.FILES_STAGED || result.files_staged || 0,
        commit_sha: result.COMMIT_SHA || result.commit_sha || '',
        message: result.MESSAGE || result.message || '',
        error: result.ERROR || result.error || null
      };
    } catch (error) {
      logger.error('Import failed', { error: error.message });
      throw new Error(`Import failed: ${error.message}`);
    }
  }

  /**
   * Get package hierarchy tree
   * @param {string} packageName - ABAP package name
   * @param {number} depth - Maximum depth to traverse (default: 3)
   * @param {boolean} includeObjects - Include object counts breakdown
   * @returns {object} Tree result with hierarchy, summary, and metadata
   */
  async tree(packageName, depth = 3, includeObjects = false) {
    logger.info('Getting package tree', { package: packageName, depth, includeObjects });

    try {
      const result = await this.abap.tree(packageName, depth, includeObjects);
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        command: result.COMMAND || result.command || 'TREE',
        package: result.PACKAGE || result.package,
        message: result.MESSAGE || result.message || '',
        hierarchy: result.HIERARCHY || result.hierarchy || null,
        summary: result.SUMMARY || result.summary || null,
        error: result.ERROR || result.error || null
      };
    } catch (error) {
      logger.error('Tree command failed', { error: error.message });
      throw new Error(`Tree command failed: ${error.message}`);
    }
  }

  async preview(objects, type = null, limit = 10) {
    logger.info('Previewing data', { objects, type, limit });

    try {
      const result = await this.abap.preview(objects, type, limit);
      return {
        success: result.SUCCESS === 'X' || result.success === 'X' || result.success === true,
        command: result.COMMAND || result.command || 'PREVIEW',
        message: result.MESSAGE || result.message || '',
        objects: result.OBJECTS || result.objects || [],
        summary: result.SUMMARY || result.summary || null,
        error: result.ERROR || result.error || null
      };
    } catch (error) {
      logger.error('Preview command failed', { error: error.message });
      throw new Error(`Preview command failed: ${error.message}`);
    }
  }
}

module.exports = {
  ABAPGitAgent
};
