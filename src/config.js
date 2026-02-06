/**
 * Configuration loader
 * Loads config from .abapGitAgent or environment variables
 */

const fs = require('fs');
const path = require('path');

let config = null;

function loadConfig() {
  if (config) return config;

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
      agent: {
        port: parseInt(process.env.AGENT_PORT, 10) || 3000,
        pollInterval: parseInt(process.env.POLL_INTERVAL, 10) || 5000,
        maxRetries: parseInt(process.env.MAX_RETRIES, 10) || 3,
        timeout: parseInt(process.env.TIMEOUT, 10) || 300000
      }
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
