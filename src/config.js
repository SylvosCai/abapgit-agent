/**
 * Configuration loader
 * Loads config from .abapGitAgent or environment variables
 */

const fs = require('fs');
const path = require('path');

let config = null;

function loadConfig() {
  if (config) return config;

  // First check current working directory (repo root) - for system-level integration
  const repoConfigPath = path.join(process.cwd(), '.abapGitAgent');
  if (fs.existsSync(repoConfigPath)) {
    config = JSON.parse(fs.readFileSync(repoConfigPath, 'utf8'));
    return config;
  }

  // Fallback to package directory (for legacy/compatibility)
  const configPath = process.env.CONFIG_PATH || path.join(__dirname, '..', '.abapGitAgent');

  if (fs.existsSync(configPath)) {
    config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
  } else {
    // Load from environment variables only when no config file exists
    config = {
      host: process.env.ABAP_HOST,
      sapport: parseInt(process.env.ABAP_PORT, 10) || 443,
      client: process.env.ABAP_CLIENT || '100',
      user: process.env.ABAP_USER,
      password: process.env.ABAP_PASSWORD,
      language: process.env.ABAP_LANGUAGE || 'EN',
      gitUsername: process.env.GIT_USERNAME,
      gitPassword: process.env.GIT_PASSWORD,
      transport: process.env.ABAP_TRANSPORT
    };
  }

  return config;
}

function getAbapConfig() {
  const cfg = loadConfig();
  return {
    host: cfg.host,
    sapport: cfg.sapport || 443,
    client: cfg.client,
    user: cfg.user,
    password: cfg.password,
    language: cfg.language || 'EN',
    gitUsername: cfg.gitUsername || process.env.GIT_USERNAME,
    gitPassword: cfg.gitPassword || process.env.GIT_PASSWORD
  };
}

function getAgentConfig() {
  const cfg = loadConfig();
  return cfg.agent;
}

function getTransport() {
  const cfg = loadConfig();
  return cfg.transport;
}

/**
 * Check if ABAP integration is configured for this repo
 * @returns {boolean} True if .abapGitAgent file exists
 */
function isAbapIntegrationEnabled() {
  const repoConfigPath = path.join(process.cwd(), '.abapGitAgent');
  return fs.existsSync(repoConfigPath);
}

/**
 * Get workflow configuration
 * @returns {Object} Workflow config with mode ('branch' or 'trunk') and optional defaultBranch
 */
function getWorkflowConfig() {
  const cfg = loadConfig();

  // Default to trunk mode if not specified (backward compatible)
  return {
    mode: cfg.workflow?.mode || 'trunk',
    defaultBranch: cfg.workflow?.defaultBranch || null  // null means auto-detect
  };
}

/**
 * Load project-level configuration (.abapgit-agent.json)
 * This file is checked into version control and contains project settings
 * @returns {Object|null} Project config or null if not found
 */
function loadProjectConfig() {
  const projectConfigPath = path.join(process.cwd(), '.abapgit-agent.json');

  if (fs.existsSync(projectConfigPath)) {
    try {
      return JSON.parse(fs.readFileSync(projectConfigPath, 'utf8'));
    } catch (error) {
      console.warn(`⚠️  Warning: Failed to parse .abapgit-agent.json: ${error.message}`);
      return null;
    }
  }

  return null;
}

/**
 * Get safeguard configuration from project-level config
 * Project-level safeguards CANNOT be overridden by user config
 * @returns {Object} Safeguards config with requireFilesForPull, disablePull, and reason
 */
function getSafeguards() {
  const projectConfig = loadProjectConfig();

  if (projectConfig?.safeguards) {
    return {
      requireFilesForPull: projectConfig.safeguards.requireFilesForPull === true,
      disablePull: projectConfig.safeguards.disablePull === true,
      reason: projectConfig.safeguards.reason || null
    };
  }

  // Default: no safeguards (backward compatible)
  return {
    requireFilesForPull: false,
    disablePull: false,
    reason: null
  };
}

/**
 * Get project information from project-level config
 * @returns {Object|null} Project info (name, description) or null
 */
function getProjectInfo() {
  const projectConfig = loadProjectConfig();
  return projectConfig?.project || null;
}

module.exports = {
  loadConfig,
  getAbapConfig,
  getAgentConfig,
  getTransport,
  isAbapIntegrationEnabled,
  getWorkflowConfig,
  getSafeguards,
  getProjectInfo,
  loadProjectConfig
};
