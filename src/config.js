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
    // Load from environment variables
    config = {
      host: process.env.ABAP_HOST,
      sapport: parseInt(process.env.ABAP_PORT, 10) || 443,
      client: process.env.ABAP_CLIENT || '100',
      user: process.env.ABAP_USER,
      password: process.env.ABAP_PASSWORD,
      language: process.env.ABAP_LANGUAGE || 'EN',
      gitUsername: process.env.GIT_USERNAME,
      gitPassword: process.env.GIT_PASSWORD
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

module.exports = {
  loadConfig,
  getAbapConfig,
  getAgentConfig
};
