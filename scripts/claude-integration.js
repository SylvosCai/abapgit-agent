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

/**
 * Load configuration from .abapGitAgent
 */
function loadConfig() {
  const configPath = path.join(__dirname, '..', '.abapGitAgent');

  if (fs.existsSync(configPath)) {
    return JSON.parse(fs.readFileSync(configPath, 'utf8'));
  }

  // Fallback to environment variables
  return {
    host: process.env.ABAP_HOST,
    sapport: parseInt(process.env.ABAP_PORT, 10) || 443,
    client: process.env.ABAP_CLIENT || '100',
    user: process.env.ABAP_USER,
    password: process.env.ABAP_PASSWORD,
    language: process.env.ABAP_LANGUAGE || 'EN'
  };
}

/**
 * Make HTTP request to ABAP REST endpoint
 */
function request(method, path, data = null) {
  return new Promise((resolve, reject) => {
    const config = loadConfig();
    const url = new URL(path, `https://${config.host}:${config.sapport}`);

    const options = {
      hostname: url.hostname,
      port: url.port,
      path: url.pathname,
      method,
      headers: {
        'Content-Type': 'application/json',
        'sap-client': config.client,
        'sap-language': config.language || 'EN'
      },
      auth: `${config.user}:${config.password}`
    };

    const req = (url.protocol === 'https:' ? https : http).request(options, (res) => {
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
    const result = await request('POST', '/sap/bc/z_abapgit_agent/pull', {
      url: gitUrl,
      branch: branch
    });

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

      default:
        console.log(`
ABAP AI Bridge - Claude Integration

Usage:
  node scripts/claude-integration.js <command> [options]

Commands:
  pull --url <git-url> [--branch <branch>]
    Pull and activate a repository

  health
    Check if ABAP REST API is healthy

Examples:
  node scripts/claude-integration.js pull --url https://github.com/example/repo --branch main
  node scripts/claude-integration.js health
`);
    }
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
