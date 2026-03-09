/**
 * Transport selector utility
 * Resolves a transport request number for the pull command when none is configured.
 *
 * Strategy:
 *   - Non-interactive (AI mode, CI): run a project-configured Node.js hook
 *   - Interactive (TTY): show a numbered picker (list, scope-switch, create, skip)
 */

const path = require('path');

/**
 * Returns true when running non-interactively (no TTY, CI, AI coding tool).
 * Can be forced with NO_TTY=1 env var.
 */
function isNonInteractive() {
  if (process.env.NO_TTY === '1') return true;
  return !process.stdout.isTTY || !process.stdin.isTTY;
}

/**
 * Load and execute a hook module.
 * The hook must export an async function({ config, http }) → string|null.
 *
 * @param {string} hookPath - Absolute path to the hook module
 * @param {object} context  - { config, http }
 * @returns {Promise<string|null>}
 */
async function runHook(hookPath, context) {
  const hookFn = require(hookPath);
  const result = await hookFn(context);
  return typeof result === 'string' ? result : null;
}

/**
 * Fetch open transport requests from ABAP.
 * Returns normalised lowercase-key objects.
 *
 * @param {object} http
 * @param {string} scope - 'mine' | 'task' | 'all'
 * @returns {Promise<Array>}
 */
async function fetchTransports(http, scope = 'mine') {
  try {
    const result = await http.get(`/sap/bc/z_abapgit_agent/transport?scope=${scope}`);
    const raw = result.TRANSPORTS || result.transports || [];
    return raw.map(t => ({
      number:      t.NUMBER      || t.number      || '',
      description: t.DESCRIPTION || t.description || '',
      owner:       t.OWNER       || t.owner       || '',
      date:        t.DATE        || t.date        || ''
    }));
  } catch {
    return [];
  }
}

/**
 * Create a new transport request.
 * @param {object} http
 * @param {string} description
 * @returns {Promise<string|null>} transport number or null
 */
async function createTransport(http, description) {
  try {
    const result = await http.post(
      '/sap/bc/z_abapgit_agent/transport',
      { action: 'CREATE', description: description || '' },
      {}
    );
    return result.NUMBER || result.number || null;
  } catch {
    return null;
  }
}

/**
 * Interactive picker — shown in TTY mode.
 * Presents a numbered menu; supports scope switching, create, and skip.
 *
 * @param {object} http
 * @returns {Promise<string|null>}
 */
async function interactivePicker(http) {
  const readline = require('readline');

  let scope = 'mine';
  let transports = [];
  let fetchError = false;

  const rl = readline.createInterface({ input: process.stdin, output: process.stderr });
  const ask = (q) => new Promise(resolve => rl.question(q, resolve));

  const fetchAndDisplay = async () => {
    transports = await fetchTransports(http, scope);
    fetchError = transports.length === 0;

    const scopeLabel = { mine: 'my transports', task: 'transports where I have a task', all: 'all open transports' }[scope];
    process.stderr.write(`\nSelect a transport request (showing: ${scopeLabel}):\n\n`);

    if (transports.length > 0) {
      transports.forEach((t, i) => {
        process.stderr.write(`  ${i + 1}. ${t.number}  ${t.description}  (${t.owner}, ${t.date})\n`);
      });
      process.stderr.write('  ' + '─'.repeat(61) + '\n');
    } else {
      if (fetchError) {
        process.stderr.write('  ⚠️  Could not fetch transports from ABAP system.\n');
      } else {
        process.stderr.write('  No open transport requests found.\n');
      }
      process.stderr.write('  ' + '─'.repeat(61) + '\n');
    }

    if (scope !== 'task') process.stderr.write('  s. Show transports where I have a task\n');
    if (scope !== 'all') process.stderr.write('  a. Show all open transports\n');
    process.stderr.write('  c. Create new transport request\n');
    process.stderr.write('  0. Skip (no transport request)\n\n');
  };

  await fetchAndDisplay();

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const answer = (await ask('Enter number or option: ')).trim().toLowerCase();

    if (answer === '0') {
      rl.close();
      return null;
    }

    if (answer === 's') {
      scope = 'task';
      await fetchAndDisplay();
      continue;
    }

    if (answer === 'a') {
      scope = 'all';
      await fetchAndDisplay();
      continue;
    }

    if (answer === 'c') {
      const desc = (await ask('Description: ')).trim();
      rl.close();
      const number = await createTransport(http, desc);
      if (number) {
        process.stderr.write(`\n✅ Created transport ${number}\n`);
        return number;
      } else {
        process.stderr.write('\n❌ Could not create transport request.\n');
        return null;
      }
    }

    const idx = parseInt(answer, 10);
    if (!isNaN(idx) && idx >= 1 && idx <= transports.length) {
      rl.close();
      return transports[idx - 1].number;
    }

    process.stderr.write(`Invalid selection '${answer}'. Enter a number, s, a, c, or 0.\n`);
  }
}

/**
 * Main export — selects a transport request for use in the pull command.
 * Returns the transport number, or null to proceed without one.
 *
 * @param {object} config - Loaded ABAP config
 * @param {object} http   - Pre-built AbapHttp instance
 * @returns {Promise<string|null>}
 */
async function selectTransport(config, http) {
  if (isNonInteractive()) {
    // AI mode: look for project-configured hook
    const hookConfig = module.exports._getTransportHookConfig();
    if (!hookConfig || !hookConfig.hook) return null;

    // Resolve hook path relative to cwd
    const hookPath = path.resolve(process.cwd(), hookConfig.hook);
    try {
      return await module.exports.runHook(hookPath, { config, http });
    } catch {
      return null;
    }
  }

  // Manual mode: interactive picker
  return interactivePicker(http);
}

/**
 * Read transportRequest hook config from .abapgit-agent.json
 * (mirrors getConflictSettings pattern in config.js)
 */
function _getTransportHookConfig() {
  const fs = require('fs');
  const projectConfigPath = path.join(process.cwd(), '.abapgit-agent.json');

  if (!fs.existsSync(projectConfigPath)) return null;

  try {
    const projectConfig = JSON.parse(fs.readFileSync(projectConfigPath, 'utf8'));
    if (projectConfig && projectConfig.transportRequest) {
      return {
        hook: projectConfig.transportRequest.hook || null,
        description: projectConfig.transportRequest.description || null
      };
    }
  } catch {
    // ignore parse errors
  }

  return null;
}

module.exports = {
  isNonInteractive,
  runHook,
  fetchTransports,
  createTransport,
  interactivePicker,
  selectTransport,
  _getTransportHookConfig
};
