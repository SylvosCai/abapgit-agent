'use strict';

const schema = require('../utils/central-catalog');
const { options, provider, providerOptions, output } = require('../utils/catalog-command');
const usage = `Usage:
  abapgit-agent credential adapters [--json]
  abapgit-agent credential register <name> --executable <absolute-path> [--arg <prefix-arg>] [--json]
  abapgit-agent credential create <git-profile> --origin <https-origin> --provider <file|bitwarden|cli>
    [--user <username> | --item-id <UUID> | --adapter <name> [--arg <lookup-arg>]] [--json]

File creation saves an empty password draft; fill it manually in the private catalog.
No credentials, environment, executable paths or lookup arguments are printed.`;
module.exports = {
  name: 'credential', description: 'Manage central Git profiles and credential adapters', requiresAbapConfig: false,
  async execute(args) {
    if (!args.length || args.includes('--help') || args.includes('-h')) { console.log(usage); return; }
    const action = args[0];
    if (!['adapters', 'register', 'create'].includes(action)) throw new Error(usage);
    const allowed = action === 'adapters' ? ['json'] : action === 'register' ? ['executable', 'arg', 'json'] : ['origin', ...providerOptions];
    const opts = options(args.slice(1), allowed, action === 'adapters' ? 0 : 1);
    const name = action === 'adapters' ? undefined : schema.name(opts.positionals[0]);
    let entry;
    if (action === 'register') entry = schema.registration(name, { executable: opts.executable, args: opts.arg });
    if (action === 'create') schema.origin(opts.origin);
    const state = schema.readCatalog(action !== 'adapters' || !process.env.ABAP_SYSTEMS_PATH);
    if (action === 'adapters') {
      const adapters = Object.entries(state.value.cliAdapters || {}).sort(([a], [b]) => a.localeCompare(b)).map(([name, value]) => ({ name, source: 'custom', available: schema.available(name, value) }));
      output(opts, { adapters }, adapters.length ? `CLI credential adapters\n\nName  Source  Available\n${adapters.map(a => `${a.name}  custom  ${a.available ? 'yes' : 'no'}`).join('\n')}\n\nAvailability does not imply authentication.` : 'No CLI adapters registered.');
      return;
    }
    const section = action === 'register' ? 'cliAdapters' : 'gitProfiles';
    state.value[section] ??= {};
    if (schema.own(state.value[section], name)) throw new Error('Central entry already exists');
    if (action === 'create') entry = { origin: schema.origin(opts.origin), credentials: provider(opts, state.value) };
    state.value[section][name] = entry;
    schema.saveCatalog(state);
    if (action === 'register') output(opts, { adapter: name, registered: true }, `Registered credential adapter ${name}.`);
    else {
      const passwordRequired = entry.credentials.provider === 'file';
      output(opts, { gitProfile: name, created: true, passwordRequired }, `Created Git credential profile ${name}.${passwordRequired ? ' Fill in its password/token manually in the private catalog before use.' : ''}`);
    }
  }
};
