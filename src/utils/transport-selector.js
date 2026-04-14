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
 * @param {string} [type] - 'workbench' (default) | 'customizing'
 * @returns {Promise<Array>}
 */
async function fetchTransports(http, scope = 'mine', type = 'workbench') {
  try {
    const typeParam = type === 'customizing' ? '&type=customizing' : '';
    const result = await http.get(`/sap/bc/z_abapgit_agent/transport?scope=${scope}${typeParam}`);
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
 * @param {string} [type] - 'workbench' (default) | 'customizing'
 * @returns {Promise<string|null>} transport number or null
 */
async function createTransport(http, description, type = 'workbench') {
  try {
    const result = await http.post(
      '/sap/bc/z_abapgit_agent/transport',
      { action: 'CREATE', description: description || '', type },
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
 * @param {string} [type] - 'workbench' (default) | 'customizing'
 * @returns {Promise<string|null>}
 */
async function interactivePicker(http, type = 'workbench') {
  const readline = require('readline');

  let scope = 'mine';
  let transports = [];
  let fetchError = false;

  const rl = readline.createInterface({ input: process.stdin, output: process.stderr });
  const ask = (q) => new Promise(resolve => rl.question(q, resolve));

  const fetchAndDisplay = async () => {
    transports = await fetchTransports(http, scope, type);
    fetchError = transports.length === 0;

    const scopeLabel = { mine: 'my transports', task: 'transports where I have a task', all: 'all open transports' }[scope];
    const typeLabel = type === 'customizing' ? ' (customizing)' : '';
    process.stderr.write(`\nSelect a transport request (showing: ${scopeLabel}${typeLabel}):\n\n`);

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
      const number = await createTransport(http, desc, type);
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
 * Build the `run` helper that is passed to hooks in AI mode.
 *
 * run(command) — accepts a full CLI command string, e.g.:
 *   run('transport list --scope task')
 *   run('transport create --description "My transport"')
 *
 * Splits on whitespace, forces --json, captures output, returns parsed JSON.
 *
 * When `type` is set, `transport list` calls automatically get `--type <type>`
 * injected (unless the hook already passes --type explicitly). This means hooks
 * written generically as `run('transport list ...')` automatically see only the
 * right kind of transport (workbench or customizing) without any hook changes.
 *
 * @param {object}   config               - Loaded ABAP config
 * @param {object}   http                 - Pre-built AbapHttp instance
 * @param {Function} loadConfig           - Config factory (from pull context)
 * @param {Function} AbapHttp             - AbapHttp constructor (from pull context)
 * @param {Function} getTransportSettings - Transport settings getter (from pull context)
 * @param {string}   [type]               - 'workbench' (default) | 'customizing'
 * @returns {Function|undefined}          - The run helper, or undefined if factories are missing
 */
function buildRun(config, http, loadConfig, AbapHttp, getTransportSettings, type = 'workbench') {
  if (!loadConfig || !AbapHttp) return undefined;

  return async function run(command) {
    let [commandName, ...args] = command.trim().split(/\s+/);

    // Auto-inject --type for "transport list" so the hook gets the right request type
    // without needing to know about the transport type explicitly.
    if (commandName === 'transport' && args[0] === 'list' && !args.includes('--type') && type) {
      args = [...args, '--type', type];
    }
    const cmdModule = require(`../commands/${commandName}`);

    // Always force --json so output is parseable
    const runArgs = args.includes('--json') ? args : [...args, '--json'];

    // Reuse the already-authenticated http instance
    const MockAbapHttp = function MockAbapHttp() { return http; };

    const runContext = {
      loadConfig: () => config,
      AbapHttp: MockAbapHttp,
      getTransportSettings: getTransportSettings || (() => ({ allowCreate: true, allowRelease: true, reason: null }))
    };

    // Capture console.log output; override process.exit to throw instead of exit
    const captured = [];
    const origLog = console.log;
    const origExit = process.exit;
    console.log = (...a) => captured.push(a.map(String).join(' '));
    process.exit = (code) => { throw new Error(`process.exit(${code})`); };

    try {
      await cmdModule.execute(runArgs, runContext);
    } finally {
      console.log = origLog;
      process.exit = origExit;
    }

    const output = captured.join('');
    if (!output) throw new Error(`run("${command}") produced no output`);
    return JSON.parse(output);
  };
}

/**
 * Main export — selects a transport request for use in the pull/customize command.
 * Returns the transport number, or null to proceed without one.
 *
 * @param {object}   config               - Loaded ABAP config
 * @param {object}   http                 - Pre-built AbapHttp instance
 * @param {Function} [loadConfig]         - Config factory (enables run helper in hook context)
 * @param {Function} [AbapHttp]           - AbapHttp constructor (enables run helper in hook context)
 * @param {Function} [getTransportSettings] - Transport settings getter
 * @param {string}   [type]               - 'workbench' (default) | 'customizing'
 *                                          Passed to hook as context.type and auto-injected into
 *                                          run('transport list ...') calls so hooks work correctly
 *                                          for both ABAP objects and customizing entries without
 *                                          needing to handle type explicitly.
 * @returns {Promise<string|null>}
 */
async function selectTransport(config, http, loadConfig, AbapHttp, getTransportSettings, type = 'workbench') {
  // Hook takes precedence over the interactive picker — runs in both TTY and non-TTY mode
  const hookConfig = module.exports._getTransportHookConfig();
  if (hookConfig && hookConfig.hook) {
    const hookPath = path.resolve(process.cwd(), hookConfig.hook);
    const run = buildRun(config, http, loadConfig, AbapHttp, getTransportSettings, type);
    try {
      return await module.exports.runHook(hookPath, { config, http, run, type });
    } catch {
      return null;
    }
  }

  // No hook configured — fall back based on context
  if (isNonInteractive()) {
    return null;  // AI/CI mode: proceed without transport
  }

  // Manual mode: interactive picker
  return interactivePicker(http, type);
}

/**
 * Read transport hook config from .abapgit-agent.json
 * (mirrors getConflictSettings pattern in config.js)
 */
function _getTransportHookConfig() {
  const fs = require('fs');
  const projectConfigPath = path.join(process.cwd(), '.abapgit-agent.json');

  if (!fs.existsSync(projectConfigPath)) return null;

  try {
    const projectConfig = JSON.parse(fs.readFileSync(projectConfigPath, 'utf8'));
    if (projectConfig && projectConfig.transports?.hook) {
      return {
        hook: projectConfig.transports.hook.path || null,
        description: projectConfig.transports.hook.description || null
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
  buildRun,
  _getTransportHookConfig
};
