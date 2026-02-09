/**
 * ABAP Git Agent - Package Entry Point
 *
 * Exports functions for programmatic use:
 *   const { pull, healthCheck } = require('abapgit-agent');
 */

const { ABAPGitAgent } = require('./agent');

/**
 * Pull repository and activate objects
 * @param {string} repoUrl - Git repository URL
 * @param {string} branch - Branch name (default: main)
 * @param {string} username - Git username (optional)
 * @param {string} password - Git password/token (optional)
 * @returns {object} Pull result with success, job_id, message, error_detail
 */
async function pull(repoUrl, branch = 'main', username = null, password = null) {
  const agent = new ABAPGitAgent();
  return await agent.pull(repoUrl, branch, username, password);
}

/**
 * Check agent health
 * @returns {object} Health status
 */
async function healthCheck() {
  const agent = new ABAPGitAgent();
  return await agent.healthCheck();
}

/**
 * Check if integration is configured for current directory
 * @returns {boolean} True if .abapGitAgent exists
 */
function isConfigured() {
  const fs = require('fs');
  const path = require('path');
  const repoConfigPath = path.join(process.cwd(), '.abapGitAgent');
  return fs.existsSync(repoConfigPath);
}

module.exports = {
  pull,
  healthCheck,
  isConfigured,
  ABAPGitAgent
};
