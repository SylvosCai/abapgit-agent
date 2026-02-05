/**
 * Claude Integration - Shell script for Claude Code
 *
 * Usage in Claude:
 * 1. Run: node scripts/claude-integration.js --pull <git-url> [--branch <branch>]
 * 2. Poll for results until completion
 * 3. Parse and display errors if any
 */

const { execSync, spawn } = require('child_process');
const http = require('http');
const path = require('path');

const AGENT_URL = process.env.AGENT_URL || 'http://localhost:3000';

/**
 * Make HTTP request to agent
 */
function request(method, path, data = null) {
  return new Promise((resolve, reject) => {
    const url = new URL(path, AGENT_URL);
    const options = {
      hostname: url.hostname,
      port: url.port,
      path: url.pathname,
      method,
      headers: {
        'Content-Type': 'application/json'
      }
    };

    const req = http.request(options, (res) => {
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
 * Poll for job completion
 */
async function waitForCompletion(jobId, maxWait = 300000, interval = 5000) {
  const start = Date.now();

  while (Date.now() - start < maxWait) {
    const result = await request('GET', `/api/jobs/${jobId}`);

    if (result.status !== 'RUNNING') {
      return result;
    }

    process.stdout.write('.');
    await new Promise(r => setTimeout(r, interval));
  }

  throw new Error('Timeout waiting for job completion');
}

/**
 * Pull and activate repository
 */
async function pull(gitUrl, branch = 'main') {
  console.log(`\n🚀 Starting pull for: ${gitUrl}`);
  console.log(`   Branch: ${branch}`);

  // Start pull
  const startResult = await request('POST', '/api/pull', {
    url: gitUrl,
    branch
  });

  if (!startResult.success) {
    console.error(`\n❌ Failed to start pull: ${startResult.error}`);
    process.exit(1);
  }

  console.log(`   Job ID: ${startResult.jobId}`);
  console.log(`   Status: ${startResult.status}`);
  console.log(`\n⏳ Waiting for completion...`);

  // Wait for completion
  const result = await waitForCompletion(startResult.jobId);

  console.log('\n');

  // Display results
  if (result.success) {
    console.log(`✅ Activation completed successfully!`);
    console.log(`   Activated: ${result.activatedCount} objects`);
    console.log(`   Failed: ${result.failedCount} objects`);
  } else {
    console.log(`❌ Activation failed!`);
    console.log(`   Message: ${result.message}`);
  }

  // Show errors
  if (result.errorLog && result.errorLog.length > 0) {
    console.log(`\n📋 Error Log:`);
    result.errorLog.forEach((err, i) => {
      console.log(`   ${i + 1}. ${err}`);
    });
  }

  return result;
}

/**
 * Check agent health
 */
async function healthCheck() {
  try {
    const result = await request('GET', '/api/health');
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
          console.error('Usage: node claude-integration.js pull --url <git-url> [--branch <branch>]');
          process.exit(1);
        }

        await pull(args[urlIndex], branchIndex !== -1 ? args[branchIndex + 1] : 'main');
        break;

      case 'status':
        const jobId = args[1];
        if (!jobId) {
          console.error('Error: Job ID required');
          process.exit(1);
        }
        const status = await request('GET', `/api/jobs/${jobId}`);
        console.log(JSON.stringify(status, null, 2));
        break;

      case 'health':
        const health = await healthCheck();
        console.log(JSON.stringify(health, null, 2));
        break;

      case 'wait':
        const waitJobId = args[1];
        if (!waitJobId) {
          console.error('Error: Job ID required');
          process.exit(1);
        }
        const waitResult = await waitForCompletion(waitJobId);
        console.log(JSON.stringify(waitResult, null, 2));
        break;

      default:
        console.log(`
ABAP AI Bridge - Claude Integration

Usage:
  node claude-integration.js <command> [options]

Commands:
  pull --url <git-url> [--branch <branch>]
    Pull and activate a repository

  status <job-id>
    Get status of a specific job

  wait <job-id>
    Wait for job completion and return results

  health
    Check if agent is healthy

Examples:
  node claude-integration.js pull --url https://github.com/example/repo --branch main
  node claude-integration.js status JOB123
  node claude-integration.js wait JOB123
`);
    }
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
