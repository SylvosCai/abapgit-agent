/**
 * Transport command - List and manage SAP transport requests
 */

const VALID_SCOPES = ['mine', 'task', 'all'];
const VALID_SUBCOMMANDS = ['list', 'create', 'check', 'release'];
const VALID_TYPES = ['workbench', 'customizing'];
const { printHttpError } = require('../utils/format-error');

module.exports = {
  name: 'transport',
  description: 'List and manage SAP transport requests',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { loadConfig, AbapHttp, getTransportSettings } = context;

    const jsonOutput = args.includes('--json');
    const verbose = args.includes('--verbose');

    // Determine subcommand (first positional arg, default to 'list')
    const subcommand = args[0] && !args[0].startsWith('-') ? args[0] : 'list';

    if (!VALID_SUBCOMMANDS.includes(subcommand)) {
      console.error(`❌ Error: Unknown subcommand '${subcommand}'. Use: ${VALID_SUBCOMMANDS.join(', ')}`);
      process.exit(1);
    }

    // Check project-level transport settings
    const transportSettings = getTransportSettings();

    if (subcommand === 'create' && !transportSettings.allowCreate) {
      console.error(`❌ transport create is disabled for this project.`);
      if (transportSettings.reason) console.error(`   Reason: ${transportSettings.reason}`);
      console.error(`   This safeguard is configured in .abapgit-agent.json`);
      const err = new Error('transport create disabled');
      err._isTransportError = true;
      throw err;
    }

    if (subcommand === 'release' && !transportSettings.allowRelease) {
      console.error(`❌ transport release is disabled for this project.`);
      if (transportSettings.reason) console.error(`   Reason: ${transportSettings.reason}`);
      console.error(`   This safeguard is configured in .abapgit-agent.json`);
      const err = new Error('transport release disabled');
      err._isTransportError = true;
      throw err;
    }

    const config = loadConfig();
    const http = new AbapHttp(config);

    try {
      switch (subcommand) {
        case 'list':
          await this._list(args, http, jsonOutput);
          break;
        case 'create':
          await this._create(args, http, jsonOutput);
          break;
        case 'check':
          await this._check(args, http, jsonOutput);
          break;
        case 'release':
          await this._release(args, http, jsonOutput);
          break;
      }
    } catch (error) {
      printHttpError(error, { verbose });
      process.exit(1);
    }
  },

  async _list(args, http, jsonOutput) {
    const scopeIdx = args.indexOf('--scope');
    const scope = scopeIdx !== -1 ? args[scopeIdx + 1] : 'mine';

    if (!VALID_SCOPES.includes(scope)) {
      console.error(`❌ Error: Invalid scope '${scope}'. Valid values: ${VALID_SCOPES.join(', ')}`);
      process.exit(1);
    }

    const typeIdx = args.indexOf('--type');
    const type = typeIdx !== -1 ? args[typeIdx + 1] : null;

    if (type && !VALID_TYPES.includes(type)) {
      console.error(`❌ Error: Invalid type '${type}'. Valid values: ${VALID_TYPES.join(', ')}`);
      process.exit(1);
    }

    const typeParam = type ? `&type=${type}` : '';
    const result = await http.get(`/sap/bc/z_abapgit_agent/transport?scope=${scope}${typeParam}`);

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
      return;
    }

    const transports = result.TRANSPORTS || result.transports || [];
    const scopeLabel = { mine: 'mine', task: 'task — where I own or have a task', all: 'all' }[scope];
    const typeLabel = type ? ` (${type})` : '';

    console.log(`\n📋 Open Transport Requests (${scopeLabel}${typeLabel})\n`);

    if (transports.length === 0) {
      console.log('  No open transport requests found.');
      console.log('');
      console.log('  To create one: abapgit-agent transport create');
      console.log('  To see more:   abapgit-agent transport list --scope task');
      return;
    }

    // Table header
    const numW = 2;
    const trW = 12;
    const descW = 33;
    const ownerW = 12;
    const dateW = 10;

    const pad = (s, w) => String(s || '').substring(0, w).padEnd(w);
    const header = `  ${'#'.padEnd(numW)}  ${pad('Number', trW)}  ${pad('Description', descW)}  ${pad('Owner', ownerW)}  ${'Date'}`;
    const divider = `  ${'─'.repeat(numW)}  ${'─'.repeat(trW)}  ${'─'.repeat(descW)}  ${'─'.repeat(ownerW)}  ${'─'.repeat(dateW)}`;

    console.log(header);
    console.log(divider);

    transports.forEach((t, i) => {
      const num = t.NUMBER || t.number || '';
      const desc = t.DESCRIPTION || t.description || '';
      const owner = t.OWNER || t.owner || '';
      const date = t.DATE || t.date || '';
      console.log(`  ${String(i + 1).padEnd(numW)}  ${pad(num, trW)}  ${pad(desc, descW)}  ${pad(owner, ownerW)}  ${date}`);
    });

    console.log('');
    console.log(`  ${transports.length} transport(s) found.`);
    console.log('');
    console.log(`  To use one: abapgit-agent pull --transport ${(transports[0].NUMBER || transports[0].number || 'DEVK900001')}`);

    if (scope === 'mine') {
      console.log('  To switch scope:');
      console.log('    transport list --scope task   transports where I have a task');
      console.log('    transport list --scope all     all open transports');
    }
  },

  async _create(args, http, jsonOutput) {
    const descIdx = args.indexOf('--description');
    let description = descIdx !== -1 ? args[descIdx + 1] : null;

    const typeIdx = args.indexOf('--type');
    const type = typeIdx !== -1 ? args[typeIdx + 1] : 'workbench';

    if (!VALID_TYPES.includes(type)) {
      console.error(`❌ Error: Invalid type '${type}'. Valid values: ${VALID_TYPES.join(', ')}`);
      process.exit(1);
    }

    // Prompt for description if not provided and running in TTY
    if (!description && !jsonOutput && process.stdin.isTTY) {
      description = await this._prompt('Description: ');
    }

    const csrfToken = await http.fetchCsrfToken();
    const result = await http.post(
      '/sap/bc/z_abapgit_agent/transport',
      { action: 'CREATE', description: description || '', type },
      { csrfToken }
    );

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
      return;
    }

    const number = result.NUMBER || result.number;
    const success = result.SUCCESS === true || result.success === true ||
                    result.SUCCESS === 'X' || result.success === 'X';
    const typeLabel = type === 'customizing' ? 'Customizing' : 'Workbench';

    if (success && number) {
      console.log(`\n✅ Transport ${number} created (${typeLabel} request).\n`);
      console.log(`   To use it now: abapgit-agent pull --transport ${number}`);
    } else {
      const error = result.ERROR || result.error || result.MESSAGE || result.message || 'Could not create transport request';
      console.error(`❌ Error: ${error}`);
      process.exit(1);
    }
  },

  async _check(args, http, jsonOutput) {
    const numIdx = args.indexOf('--number');
    if (numIdx === -1 || !args[numIdx + 1]) {
      console.error('❌ Error: --number is required for transport check');
      process.exit(1);
    }
    const number = args[numIdx + 1];

    const csrfToken = await http.fetchCsrfToken();
    const result = await http.post(
      '/sap/bc/z_abapgit_agent/transport',
      { action: 'CHECK', number },
      { csrfToken }
    );

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
      return;
    }

    const passed = result.PASSED === true || result.passed === true ||
                   result.PASSED === 'X' || result.passed === 'X';
    const issues = result.ISSUES || result.issues || [];
    const desc = result.DESCRIPTION || result.description || '';
    const owner = result.OWNER || result.owner || '';
    const date = result.DATE || result.date || '';

    console.log(`\n🔍 Checking transport ${number}...`);
    if (desc) console.log(`\n   Description: ${desc}`);
    if (owner) console.log(`   Owner: ${owner}`);
    if (date) console.log(`   Date: ${date}`);

    if (passed || issues.length === 0) {
      console.log(`\n✅ Transport check passed — no issues found.`);
      console.log(`   Ready to release: abapgit-agent transport release --number ${number}`);
    } else {
      console.log(`\n⚠️  Transport check completed with warnings/errors:\n`);

      const typeW = 6;
      const objW = 21;
      const msgW = 44;
      const pad = (s, w) => String(s || '').substring(0, w).padEnd(w);

      console.log(`  ${'Type'.padEnd(typeW)}  ${'Object'.padEnd(objW)}  Message`);
      console.log(`  ${'─'.repeat(typeW)}  ${'─'.repeat(objW)}  ${'─'.repeat(msgW)}`);

      for (const issue of issues) {
        const type = issue.TYPE || issue.type || '';
        const objType = issue.OBJ_TYPE || issue.obj_type || '';
        const objName = issue.OBJ_NAME || issue.obj_name || '';
        const text = issue.TEXT || issue.text || '';
        const icon = type === 'E' ? '❌' : type === 'W' ? '⚠️ ' : 'ℹ️ ';
        const obj = objType && objName ? `${objType} ${objName}` : objType || objName;
        console.log(`  ${icon.padEnd(typeW)}  ${pad(obj, objW)}  ${text}`);
      }

      console.log(`\n  ${issues.length} issue(s) found. Fix before releasing.`);
    }
  },

  async _release(args, http, jsonOutput) {
    const numIdx = args.indexOf('--number');
    if (numIdx === -1 || !args[numIdx + 1]) {
      console.error('❌ Error: --number is required for transport release');
      process.exit(1);
    }
    const number = args[numIdx + 1];

    const csrfToken = await http.fetchCsrfToken();
    const result = await http.post(
      '/sap/bc/z_abapgit_agent/transport',
      { action: 'RELEASE', number },
      { csrfToken }
    );

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
      return;
    }

    const success = result.SUCCESS === true || result.success === true ||
                    result.SUCCESS === 'X' || result.success === 'X';
    const message = result.MESSAGE || result.message || '';
    const error = result.ERROR || result.error || '';
    const desc = result.DESCRIPTION || result.description || '';

    if (success) {
      console.log(`\n🚀 Releasing transport ${number}...`);
      if (desc) console.log(`\n   Description: ${desc}`);
      console.log(`\n✅ Transport ${number} released successfully.`);
    } else {
      console.error(`\n❌ Could not release transport ${number}.`);
      if (error) {
        console.error(`\n   Error: ${error}`);
      } else if (message) {
        console.error(`\n   Error: ${message}`);
      }
    }
  },

  _prompt(question) {
    const readline = require('readline');
    const rl = readline.createInterface({ input: process.stdin, output: process.stderr });
    return new Promise((resolve) => {
      rl.question(question, (answer) => {
        rl.close();
        resolve(answer.trim());
      });
    });
  }
};
