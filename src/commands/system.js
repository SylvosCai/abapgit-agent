'use strict';

const schema = require('../utils/central-catalog');
const { options, output } = require('../utils/catalog-command');
const usage = `Usage:
  abapgit-agent system list [--json]
  abapgit-agent system create <name> --host <hostname> [--port 443] [--language EN] [--json]

Create/list personal central endpoints. No SAP, vault or credential checks are made.`;
module.exports = {
  name: 'system', description: 'Create and list central SAP systems', requiresAbapConfig: false,
  async execute(args) {
    if (!args.length || args.includes('--help') || args.includes('-h')) { console.log(usage); return; }
    const action = args[0];
    if (!['list', 'create'].includes(action)) throw new Error(usage);
    const opts = options(args.slice(1), action === 'list' ? ['json'] : ['host', 'port', 'language', 'json'], action === 'create' ? 1 : 0);
    let created;
    if (action === 'create') {
      schema.name(opts.positionals[0]);
      if (opts.port !== undefined && !/^\d+$/.test(opts.port)) throw new Error('Invalid SAP port');
      created = { protocol: 'https', host: opts.host, sapport: opts.port === undefined ? 443 : Number(opts.port), language: opts.language ?? 'EN', clients: {} };
      schema.endpoint(created);
    }
    const state = schema.readCatalog(action === 'create');
    if (!schema.object(state.value.systems)) throw new Error('Catalog requires a systems object');
    if (action === 'list') {
      const systems = Object.keys(state.value.systems).sort().map(name => ({ name, ...schema.endpoint(state.value.systems[name]) }));
      output(opts, { systems }, systems.length ? `Central SAP systems\n\nSystem  Protocol  Host  Port  Language\n${systems.map(s => `${s.name}  ${s.protocol}  ${s.host}  ${s.sapport}  ${s.language}`).join('\n')}` : 'No central SAP systems configured.');
    } else {
      const name = opts.positionals[0];
      if (schema.own(state.value.systems, name)) throw new Error('Central system already exists');
      state.value.systems[name] = created;
      schema.saveCatalog(state);
      output(opts, { system: name, created: true }, `Created central SAP system ${name}. Add a client with logon create.`);
    }
  }
};
