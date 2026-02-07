/**
 * Claude Integration - Shell script for Claude Code
 *
 * Usage in Claude:
 * 1. Run: node scripts/claude-integration.js pull --url <git-url> [--branch <branch>]
 * 2. Parse response for activation result
 * 3. Display errors if any
 */

const http = require('http');
const https = require('https');
const path = require('path');
const fs = require('fs');

const COOKIE_FILE = path.join(__dirname, '..', '.abapgit_agent_cookies.txt');

/**
 * Check if ABAP AI integration is configured for this repo
 * Looks for .abapGitAgent in current working directory
 */
function isAbapIntegrationEnabled() {
  // Check in current working directory (repo root)
  const repoConfigPath = path.join(process.cwd(), '.abapGitAgent');
  if (fs.existsSync(repoConfigPath)) {
    return true;
  }
  // Also check if repo has abap/ folder with ABAP objects
  const abapFolder = path.join(process.cwd(), 'abap');
  if (fs.existsSync(abapFolder)) {
    const files = fs.readdirSync(abapFolder);
    return files.some(f => f.endsWith('.abap') || f.endsWith('.clas.abap') || f.endsWith('.fugr.abap'));
  }
  return false;
}

/**
 * Load configuration from .abapGitAgent
 */
function loadConfig() {
  // First check current working directory (repo root)
  const repoConfigPath = path.join(process.cwd(), '.abapGitAgent');

  if (fs.existsSync(repoConfigPath)) {
    console.log(`📁 Found .abapGitAgent in: ${repoConfigPath}`);
    return JSON.parse(fs.readFileSync(repoConfigPath, 'utf8'));
  }

  // Fallback to abap-ai-bridge parent directory
  const bridgeConfigPath = path.join(__dirname, '..', '.abapGitAgent');
  if (fs.existsSync(bridgeConfigPath)) {
    return JSON.parse(fs.readFileSync(bridgeConfigPath, 'utf8'));
  }

  // Fallback to environment variables
  return {
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

/**
 * Read cookies from Netscape format cookie file
 */
function readNetscapeCookies() {
  if (!fs.existsSync(COOKIE_FILE)) return '';

  const content = fs.readFileSync(COOKIE_FILE, 'utf8');
  const lines = content.split('\n');
  const cookies = [];

  for (const line of lines) {
    const trimmed = line.trim();
    // Skip empty lines and header comments but NOT HttpOnly cookies
    if (!trimmed || (trimmed.startsWith('#') && !trimmed.startsWith('#HttpOnly'))) continue;

    const parts = trimmed.split('\t');
    if (parts.length >= 7) {
      cookies.push(`${parts[5]}=${parts[6]}`);
    }
  }

  return cookies.join('; ');
}

/**
 * Fetch CSRF token using GET /pull with X-CSRF-Token: fetch
 */
async function fetchCsrfToken(config) {
  const url = new URL(`/sap/bc/z_abapgit_agent/pull`, `https://${config.host}:${config.sapport}`);

  return new Promise((resolve, reject) => {
    const cookieHeader = readNetscapeCookies();

    const options = {
      hostname: url.hostname,
      port: url.port,
      path: url.pathname,
      method: 'GET',
      headers: {
        'Authorization': `Basic ${Buffer.from(`${config.user}:${config.password}`).toString('base64')}`,
        'sap-client': config.client,
        'sap-language': config.language || 'EN',
        'X-CSRF-Token': 'fetch',
        'Content-Type': 'application/json',
        ...(cookieHeader && { 'Cookie': cookieHeader })
      },
      agent: new https.Agent({ rejectUnauthorized: false })
    };

    const req = https.request(options, (res) => {
      const csrfToken = res.headers['x-csrf-token'];

      // Save new cookies from response - the CSRF token is tied to this new session!
      const setCookie = res.headers['set-cookie'];
      if (setCookie) {
        const cookies = Array.isArray(setCookie)
          ? setCookie.map(c => c.split(';')[0]).join('; ')
          : setCookie.split(';')[0];
        fs.writeFileSync(COOKIE_FILE, cookies);
      }

      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        resolve(csrfToken);
      });
    });

    req.on('error', reject);
    req.end();
  });
}

/**
 * Make HTTP request to ABAP REST endpoint
 */
function request(method, path, data = null, options = {}) {
  return new Promise((resolve, reject) => {
    const config = loadConfig();
    const url = new URL(path, `https://${config.host}:${config.sapport}`);

    const headers = {
      'Content-Type': 'application/json',
      'sap-client': config.client,
      'sap-language': config.language || 'EN',
      ...options.headers
    };

    // Add authorization
    headers['Authorization'] = `Basic ${Buffer.from(`${config.user}:${config.password}`).toString('base64')}`;

    // Add CSRF token for POST
    if (method === 'POST' && options.csrfToken) {
      headers['X-CSRF-Token'] = options.csrfToken;
    }

    // Add cookies if available
    const cookieHeader = readNetscapeCookies();
    if (cookieHeader) {
      headers['Cookie'] = cookieHeader;
    }

    const reqOptions = {
      hostname: url.hostname,
      port: url.port,
      path: url.pathname,
      method,
      headers,
      agent: new https.Agent({ rejectUnauthorized: false })
    };

    const req = (url.protocol === 'https:' ? https : http).request(reqOptions, (res) => {
      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        try {
          resolve(JSON.parse(body));
        } catch (e) {
          resolve(body);
        }
      });
    });

    req.on('error', reject);

    if (data) {
      req.write(JSON.stringify(data));
    }
    req.end();
  });
}

/**
 * Pull and activate repository
 */
async function pull(gitUrl, branch = 'main') {
  console.log(`\n🚀 Starting pull for: ${gitUrl}`);
  console.log(`   Branch: ${branch}`);

  try {
    const config = loadConfig();

    // Fetch CSRF token first
    const csrfToken = await fetchCsrfToken(config);

    // Prepare request data with git credentials
    const data = {
      url: gitUrl,
      branch: branch,
      username: config.gitUsername,
      password: config.gitPassword
    };

    const result = await request('POST', '/sap/bc/z_abapgit_agent/pull', data, { csrfToken });

    console.log('\n');

    if (result.success === 'X' || result.success === true) {
      console.log(`✅ Pull completed successfully!`);
      console.log(`   Job ID: ${result.job_id}`);
      console.log(`   Message: ${result.message}`);
    } else {
      console.log(`❌ Pull completed with errors!`);
      console.log(`   Job ID: ${result.job_id}`);
      console.log(`   Message: ${result.message}`);

      if (result.error_detail) {
        console.log(`\n📋 Error Details:`);
        console.log(result.error_detail);
      }
    }

    return result;
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Check agent health
 */
async function healthCheck() {
  try {
    const result = await request('GET', '/sap/bc/z_abapgit_agent/health');
    return result;
  } catch (error) {
    return { status: 'unreachable', error: error.message };
  }
}

/**
 * Main CLI
 */
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  // Check if ABAP integration is enabled for this repo
  if (!isAbapIntegrationEnabled()) {
    console.log(`
⚠️  ABAP AI Integration not configured for this repository.

To enable integration:
1. Create a .abapGitAgent file in the repo root with ABAP connection details:
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN"
}

2. Or set environment variables:
   - ABAP_HOST, ABAP_PORT, ABAP_CLIENT, ABAP_USER, ABAP_PASSWORD
`);
    if (command !== 'help' && command !== '--help' && command !== '-h') {
      process.exit(1);
    }
  }

  try {
    switch (command) {
      case 'pull':
        const urlIndex = args.indexOf('--url') !== -1 ? args.indexOf('--url') + 1 : 1;
        const branchIndex = args.indexOf('--branch');

        if (!args[urlIndex]) {
          console.error('Error: --url is required');
          console.error('Usage: node scripts/claude-integration.js pull --url <git-url> [--branch <branch>]');
          process.exit(1);
        }

        await pull(args[urlIndex], branchIndex !== -1 ? args[branchIndex + 1] : 'main');
        break;

      case 'health':
        const health = await healthCheck();
        console.log(JSON.stringify(health, null, 2));
        break;

      case 'status':
        if (isAbapIntegrationEnabled()) {
          console.log('✅ ABAP AI Integration is ENABLED');
          console.log('   Config location:', path.join(process.cwd(), '.abapGitAgent'));
        } else {
          console.log('❌ ABAP AI Integration is NOT configured');
        }
        break;

      default:
        console.log(`
ABAP AI Bridge - Claude Integration

Usage:
  node scripts/claude-integration.js <command> [options]

Commands:
  pull --url <git-url> [--branch <branch>]
    Pull and activate a repository in ABAP system

  health
    Check if ABAP REST API is healthy

  status
    Check if ABAP integration is configured for this repo

Examples:
  node scripts/claude-integration.js pull --url https://github.tools.sap/user/repo --branch main
  node scripts/claude-integration.js health
`);
    }
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
