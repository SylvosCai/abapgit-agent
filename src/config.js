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
      transport: process.env.ABAP_TRANSPORT,
      protocol: process.env.ABAP_PROTOCOL || 'https'
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
    gitPassword: cfg.gitPassword || process.env.GIT_PASSWORD,
    protocol: cfg.protocol || process.env.ABAP_PROTOCOL || 'https'
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
    // importAllowedUsers: accept string (single) or array; normalize to uppercase array
    const rawAllowed = projectConfig.safeguards.importAllowedUsers;
    let importAllowedUsers = null;
    if (rawAllowed) {
      const arr = Array.isArray(rawAllowed) ? rawAllowed : [rawAllowed];
      importAllowedUsers = arr.map(u => String(u).toUpperCase());
    }

    return {
      requireFilesForPull: projectConfig.safeguards.requireFilesForPull === true,
      disablePull: projectConfig.safeguards.disablePull === true,
      disableRun: projectConfig.safeguards.disableRun === true,
      disableImport: projectConfig.safeguards.disableImport === true,
      requireImportMessage: projectConfig.safeguards.requireImportMessage === true,
      importAllowedUsers,
      disableProbeClasses: projectConfig.safeguards.disableProbeClasses === true,
      reason: projectConfig.safeguards.reason || null
    };
  }

  // Default: no safeguards (backward compatible)
  return {
    requireFilesForPull: false,
    disablePull: false,
    disableRun: false,
    disableImport: false,
    requireImportMessage: false,
    importAllowedUsers: null,
    disableProbeClasses: false,
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

/**
 * Get conflict detection configuration from project-level config
 * Precedence: CLI flag > project config > default ('abort')
 * @returns {Object} Conflict detection config with mode and reason
 */
function getConflictSettings() {
  const projectConfig = loadProjectConfig();

  if (projectConfig?.conflictDetection) {
    const validModes = ['ignore', 'abort'];
    const mode = projectConfig.conflictDetection.mode;
    if (mode && !validModes.includes(mode)) {
      console.warn(`⚠️  Warning: Invalid conflictDetection.mode '${mode}' in .abapgit-agent.json. Must be one of: ${validModes.join(', ')}. Falling back to 'abort'.`);
      return { mode: 'abort', reason: null };
    }
    return {
      mode: mode || 'abort',
      reason: projectConfig.conflictDetection.reason || null
    };
  }

  // Default: abort (conflict detection on by default)
  return { mode: 'abort', reason: null };
}

/**
 * Get transport hook configuration from project-level config
 * @returns {{ hook: string|null, description: string|null }}
 */
function getTransportHookConfig() {
  const projectConfig = loadProjectConfig();
  if (projectConfig?.transports?.hook) {
    return {
      hook: projectConfig.transports.hook.path || null,
      description: projectConfig.transports.hook.description || null
    };
  }
  return { hook: null, description: null };
}

/**
 * Get transport operation settings from project-level config
 * @returns {{ allowCreate: boolean, allowRelease: boolean, reason: string|null }}
 */
function getTransportSettings() {
  const projectConfig = loadProjectConfig();
  if (projectConfig?.transports) {
    return {
      allowCreate:  projectConfig.transports.allowCreate  !== false,
      allowRelease: projectConfig.transports.allowRelease !== false,
      reason:       projectConfig.transports.reason || null
    };
  }
  return { allowCreate: true, allowRelease: true, reason: null };
}

/**
 * Get scratch workspace configuration from personal config (.abapGitAgent)
 * Used by AI to create probe/throwaway ABAP classes outside the current project
 * @returns {{ path: string, classPrefix: string, programPrefix: string }|null}
 */
function getScratchWorkspace() {
  const cfg = loadConfig();
  if (!cfg.scratchWorkspace) return null;
  const user = (cfg.user || 'PROBE').toUpperCase();
  return {
    path: cfg.scratchWorkspace.path || null,
    classPrefix: cfg.scratchWorkspace.classPrefix || `ZCL_${user}_`,
    programPrefix: cfg.scratchWorkspace.programPrefix || `Z${user}_`
  };
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
  getConflictSettings,
  loadProjectConfig,
  getTransportHookConfig,
  getTransportSettings,
  getScratchWorkspace
};
