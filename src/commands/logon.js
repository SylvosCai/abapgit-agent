'use strict';

const path = require('node:path');
const schema = require('../utils/central-catalog');
const { centralBinding, selection } = require('../utils/connection-context');
const { readPrivate, writePrivate } = require('../utils/private-json');
const { options, provider, providerOptions, output } = require('../utils/catalog-command');
const usage = `Usage:
  abapgit-agent logon list [--json]
  abapgit-agent logon current [--json]
  abapgit-agent logon use <client> [--json]
  abapgit-agent logon create <client> [--system <name>] --provider <file|bitwarden|cli>
    [--user <username> | --item-id <UUID> | --adapter <name> [--arg <lookup-arg>]] [--json]

Global --logon-client overrides one invocation. No SAP/vault checks are made.
File creation saves an empty draft password to fill manually in the private catalog.`;
module.exports = {
  name: 'logon', description: 'Manage client logons within a central SAP system', requiresAbapConfig: false,
  async execute(args, context) {
    if (!args.length || args.includes('--help') || args.includes('-h')) { console.log(usage); return; }
    const action = args[0];
    if (!['list', 'current', 'use', 'create'].includes(action)) throw new Error(usage);
    const opts = options(args.slice(1), action === 'create' ? ['system', ...providerOptions] : ['json'], ['use', 'create'].includes(action) ? 1 : 0);
    if (opts.positionals.length) schema.client(opts.positionals[0]);
    let raw, previous, repoFile;
    if (action === 'use') {
      repoFile = path.join(process.cwd(), '.abapGitAgent');
      previous = readPrivate(repoFile);
      raw = previous.value;
    } else raw = action === 'create' && opts.system !== undefined ? { system: opts.system } : context.loadConfig();
    centralBinding(raw);
    const alias = schema.name(raw.system);
    const state = schema.readCatalog();
    const system = schema.system(state.value, alias);
    if (action === 'create') {
      const id = opts.positionals[0];
      if (schema.own(system.clients, id)) throw new Error('Central client already exists');
      const credentials = provider(opts, state.value);
      system.clients[id] = { credentials };
      schema.saveCatalog(state);
      const passwordRequired = credentials.provider === 'file';
      output(opts, { system: alias, client: id, created: true, passwordRequired }, `Created logon ${alias}/${id}.${passwordRequired ? ' Fill in its password manually in the private catalog before use.' : ''}`);
      return;
    }
    // Validate metadata, never provider readiness or unrelated Git profiles.
    for (const entry of Object.values(system.clients)) {
      if (!schema.object(entry) || Object.keys(entry).some(key => key !== 'credentials')) throw new Error('Invalid client logon metadata');
      schema.credentials(entry.credentials, state.value);
    }
    const selected = action === 'use' ? { client: opts.positionals[0] } : selection(raw, context.logonClient);
    if (!schema.own(system.clients, selected.client)) throw new Error('Unknown central SAP client');
    if (action === 'use') {
      const changed = raw.client !== selected.client;
      if (changed) writePrivate(repoFile, { ...raw, client: selected.client }, previous, false);
      output(opts, { system: alias, savedClient: selected.client, changed }, `Saved logon client ${selected.client} for ${alias}.`);
    } else if (action === 'current') {
      output(opts, { system: alias, ...selected }, `System: ${alias}\nClient: ${selected.client}\nSource: ${selected.source}`);
    } else {
      const clients = Object.entries(system.clients).sort(([a], [b]) => a.localeCompare(b)).map(([client, entry]) => ({ client, provider: entry.credentials.provider,
        ...(entry.credentials.provider === 'cli' ? { adapter: entry.credentials.adapter } : {}), selected: client === selected.client }));
      output(opts, { system: alias, ...selected, clients }, `SAP clients for ${alias}\n\nClient  Provider  Adapter  Selected\n${clients.map(c => `${c.client}  ${c.provider}  ${c.adapter || '-'}  ${c.selected ? '*' : ''}`).join('\n')}\n\nSelected client: ${selected.client} (${selected.source})`);
    }
  }
};
