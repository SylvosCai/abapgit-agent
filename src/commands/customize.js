/**
 * Customize command — write a single row to a SAP customizing table (delivery class C/E).
 *
 * Usage:
 *   abapgit-agent customize <table> --set <field=value> [<field=value>...]
 *                                   [--transport <TRKORR>] [--no-transport] [--json]
 */

const { printHttpError } = require('../utils/format-error');

module.exports = {
  name: 'customize',
  description: 'Write a row to a SAP customizing table (delivery class C/E)',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { loadConfig, AbapHttp, getTransport, getTransportSettings } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent customize <table> --set <field=value> [<field=value>...]
                          [--transport <TRKORR>] [--no-transport] [--json]

Description:
  Write a single row to a SAP customizing table (delivery class C or E).
  The row is identified by the key fields you provide via --set.

Parameters:
  <table>               Name of the customizing table (e.g. ZTABLE_CONFIG).
  --set <field=value>   One or more field=value pairs (space-separated after --set).
  --transport <TRKORR>  Record the change in this transport request.
  --no-transport        Write without a transport request (local change).
  --json                Output result as JSON.

Examples:
  abapgit-agent customize ZTABLE_CONFIG --set KEY=APP VALUE=active
  abapgit-agent customize ZTABLE_CONFIG --set KEY=APP VALUE=active --transport DEVK900001
  abapgit-agent customize ZTABLE_CONFIG --set KEY=APP VALUE=active --no-transport
`);
      return;
    }

    const jsonOutput = args.includes('--json');
    const verbose    = args.includes('--verbose');
    const noTransport = args.includes('--no-transport');

    // First positional arg (not starting with '-' and not following --set/--transport) = table name
    const skipNext = new Set();
    for (let i = 0; i < args.length; i++) {
      if (args[i] === '--transport' || args[i] === '-t') {
        if (i + 1 < args.length) skipNext.add(i + 1);
      } else if (args[i] === '--set') {
        let j = i + 1;
        while (j < args.length && !args[j].startsWith('-')) {
          skipNext.add(j);
          j++;
        }
      }
    }
    const tableName = args.find((a, idx) => !a.startsWith('-') && !skipNext.has(idx));
    if (!tableName) {
      console.error('❌ Error: table name is required');
      console.error('   Usage: abapgit-agent customize <table> --set <field=value> [...]');
      process.exit(1);
    }

    // Collect all --set values (one or more field=value pairs after the flag)
    const fieldValues = [];
    for (let i = 0; i < args.length; i++) {
      if (args[i] === '--set') {
        // Consume all following positional tokens as field=value pairs
        let j = i + 1;
        while (j < args.length && !args[j].startsWith('-')) {
          const pair = args[j];
          const eqIdx = pair.indexOf('=');
          if (eqIdx === -1) {
            console.error(`❌ Error: --set value '${pair}' must be in field=value format`);
            process.exit(1);
          }
          fieldValues.push({
            field: pair.substring(0, eqIdx).toUpperCase(),
            value: pair.substring(eqIdx + 1)
          });
          j++;
        }
      }
    }

    if (fieldValues.length === 0) {
      console.error('❌ Error: at least one --set field=value pair is required');
      console.error('   Example: abapgit-agent customize ZTABLE_CONFIG --set KEY=APP VALUE=active');
      process.exit(1);
    }

    // Transport resolution: CLI --transport > config/env > auto-selection
    let transportRequest = null;
    const transportArgIndex = args.indexOf('--transport');
    if (transportArgIndex !== -1 && transportArgIndex + 1 < args.length) {
      transportRequest = args[transportArgIndex + 1];
    } else if (!noTransport) {
      transportRequest = getTransport ? getTransport() : null;
    }

    // When no transport is determined yet and we are not in JSON mode, use the
    // selector (hook / interactive picker) with type='customizing'
    if (!transportRequest && !noTransport && !jsonOutput) {
      const { selectTransport, isNonInteractive, _getTransportHookConfig } = require('../utils/transport-selector');

      const config = loadConfig();
      const http = new AbapHttp(config);
      transportRequest = await selectTransport(config, http, loadConfig, AbapHttp, getTransportSettings, 'customizing');

      if (transportRequest === null) {
        const hookConfig = _getTransportHookConfig();
        if (hookConfig && hookConfig.hook) {
          if (isNonInteractive()) {
            console.error('❌ Error: transport hook returned no transport request.');
            console.error(`   Hook: ${hookConfig.hook}`);
            if (hookConfig.description) console.error(`   ${hookConfig.description}`);
            process.exit(1);
          } else {
            process.stderr.write(`⚠️  Transport hook returned no transport request (${hookConfig.hook}).\n`);
            process.stderr.write('   Please select one manually:\n');
            const { interactivePicker } = require('../utils/transport-selector');
            transportRequest = await interactivePicker(http, 'customizing');
          }
        }
      }
    }

    const config = loadConfig();
    const http = new AbapHttp(config);

    try {
      const csrfToken = await http.fetchCsrfToken();
      const payload = {
        table_name:   tableName.toUpperCase(),
        field_values: fieldValues,
        transport:    transportRequest || '',
        no_transport: noTransport ? 'X' : ''
      };

      const result = await http.post(
        '/sap/bc/z_abapgit_agent/customize',
        payload,
        { csrfToken }
      );

      if (jsonOutput) {
        console.log(JSON.stringify(result, null, 2));
        return;
      }

      const success       = result.SUCCESS === true || result.success === true ||
                            result.SUCCESS === 'X'  || result.success === 'X';
      const error         = result.ERROR         || result.error         || '';
      const message       = result.MESSAGE       || result.message       || '';
      const action        = result.ACTION        || result.action        || '';
      const transport     = result.TRANSPORT     || result.transport     || '';
      const deliveryCls   = result.DELIVERY_CLASS || result.delivery_class || '';

      if (!success) {
        console.error(`❌ Error: ${error || message || 'customize command failed'}`);
        process.exit(1);
      }

      console.log(`\n✅ Customizing entry written to ${tableName.toUpperCase()}`);
      if (action)      console.log(`   Action:    ${action}`);
      if (transport)   console.log(`   Transport: ${transport} (recorded)`);
      else if (deliveryCls) console.log(`   Transport: none (delivery class ${deliveryCls})`);
      else             console.log(`   Transport: none`);
      if (message)     console.log(`   ${message}`);
      console.log('');

    } catch (error) {
      printHttpError(error, { verbose });
      process.exit(1);
    }
  }
};
