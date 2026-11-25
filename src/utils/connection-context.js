'use strict';

const schema = require('./central-catalog');
const { createProviderResolver } = require('./credential-provider');

function freeze(value) {
  if (schema.object(value) || Array.isArray(value)) { Object.values(value).forEach(freeze); Object.freeze(value); }
  return value;
}
function centralBinding(raw) {
  if (!schema.object(raw)) throw new Error('Repository configuration must be a JSON object');
  if (process.platform === 'win32' && (schema.own(raw, 'system') || schema.own(raw, 'gitProfile'))) throw new Error('Central credentials require a supported POSIX platform');
  if (schema.own(raw, 'system')) {
    schema.name(raw.system);
    if (['host', 'sapport', 'protocol', 'language', 'user', 'password', 'transport', 'customizingClient', 'customizingLogon'].some(key => schema.own(raw, key))) throw new Error('Central SAP binding conflicts with flat endpoint, credential or transport settings');
  }
  if (schema.own(raw, 'gitProfile')) {
    schema.name(raw.gitProfile);
    if (['gitUsername', 'gitPassword'].some(key => schema.own(raw, key))) throw new Error('Central Git profile conflicts with flat Git credentials');
  }
}
function selection(raw, override) {
  centralBinding(raw);
  if (!schema.own(raw, 'system')) throw new Error('Client selection requires a central SAP system binding');
  const source = override !== undefined ? 'cli' : process.env.ABAP_LOGON_CLIENT !== undefined ? 'environment' : 'repository';
  return { client: schema.client(override ?? process.env.ABAP_LOGON_CLIENT ?? raw.client), source };
}
function parseGlobal(args) {
  const remaining = [];
  let override;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--logon-client' || args[i].startsWith('--logon-client=')) {
      if (override !== undefined) throw new Error('Repeated --logon-client override');
      override = schema.client(args[i] === '--logon-client' ? args[++i] : args[i].slice('--logon-client='.length));
    } else remaining.push(args[i]);
  }
  return { args: remaining, override };
}
async function createConnectionContext(raw, override) {
  raw = freeze(JSON.parse(JSON.stringify(raw)));
  centralBinding(raw);
  if (override !== undefined && !schema.own(raw, 'system')) throw new Error('--logon-client requires a central SAP system binding');
  let catalog, provider, selected;
  const load = () => {
    if (!catalog) { catalog = freeze(schema.readCatalog().value); provider = createProviderResolver(catalog); }
    return catalog;
  };
  const materializeSap = async target => {
    const entry = schema.system(load(), raw.system);
    const id = schema.client(target);
    if (!schema.own(entry.clients, id)) throw new Error('Unknown central SAP client');
    const logon = entry.clients[id];
    if (!schema.object(logon) || Object.keys(logon).some(key => key !== 'credentials')) throw new Error('Client logons contain credentials only');
    const settings = raw.clientSettings?.[id] ?? {};
    if (!schema.object(settings) || Object.keys(settings).some(key => !['transport', 'customizingClient'].includes(key))) throw new Error('Invalid client project settings');
    if (settings.transport !== undefined && typeof settings.transport !== 'string') throw new Error('Invalid client transport');
    if (settings.customizingClient !== undefined) schema.client(settings.customizingClient);
    const credentials = await provider(logon.credentials);
    return freeze({ ...JSON.parse(JSON.stringify(raw)), ...schema.endpoint(entry), client: id, ...credentials,
      transport: settings.transport, customizingClient: settings.customizingClient ?? id, verifiedTls: true });
  };
  const sapCache = new Map();
  const resolveSap = target => {
    const id = schema.client(target);
    if (!sapCache.has(id)) sapCache.set(id, materializeSap(id));
    return sapCache.get(id);
  };
  let config;
  if (schema.own(raw, 'system')) {
    selected = selection(raw, override);
    config = await resolveSap(selected.client);
  } else {
    config = { ...raw };
    if (schema.own(raw, 'gitProfile')) {
      if ((raw.protocol || 'https') !== 'https') throw new Error('Central Git credentials require a verified HTTPS SAP recipient');
      config.verifiedTls = true;
    }
    config = freeze(config);
  }
  const gitCache = new Map();
  const resolveGitCredentials = async url => {
    if (!schema.own(raw, 'gitProfile')) return { username: raw.gitUsername, password: raw.gitPassword };
    const cat = load();
    if (!schema.own(cat.gitProfiles || {}, raw.gitProfile)) throw new Error('Unknown central Git credential profile');
    const profile = cat.gitProfiles[raw.gitProfile];
    if (!schema.object(profile) || schema.origin(profile.origin) !== schema.destination(url)) throw new Error('Git destination does not match the credential profile origin');
    if (!gitCache.has(profile.origin)) gitCache.set(profile.origin, provider(profile.credentials).then(value => freeze({ username: value.user, password: value.password })));
    return gitCache.get(profile.origin);
  };
  return { config, loadConfig: () => config, resolveGitCredentials,
    resolveSapClient: schema.own(raw, 'system') ? async target => target === config.client ? config : resolveSap(target) : undefined };
}
module.exports = { createConnectionContext, centralBinding, selection, parseGlobal };
