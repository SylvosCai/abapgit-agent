'use strict';

const path = require('node:path');
const os = require('node:os');
const net = require('node:net');
const fs = require('node:fs');
const { readPrivate, writePrivate } = require('./private-json');

const own = (object, key) => Object.hasOwn(object, key);
function object(value) { return value !== null && typeof value === 'object' && !Array.isArray(value); }
function name(value) {
  if (typeof value !== 'string' || !value.trim() || ['__proto__', 'constructor', 'prototype'].includes(value)) throw new Error('Invalid configuration name');
  return value;
}
function client(value) {
  if (typeof value !== 'string' || !/^\d{3}$/.test(value)) throw new Error('Client must be exactly three digits');
  return value;
}
function host(value) {
  if (typeof value !== 'string' || (!net.isIP(value) && (value.length > 253 || !value.split('.').every(label => /^(?=.{1,63}$)[a-z\d](?:[a-z\d-]*[a-z\d])?$/i.test(label))))) throw new Error('Host must be a hostname or IP, not a URL');
  return value;
}
function origin(value) {
  let url;
  try { url = new URL(value); } catch { throw new Error('Invalid HTTPS origin'); }
  if (url.protocol !== 'https:' || url.username || url.password || url.pathname !== '/' || url.search || url.hash) throw new Error('Expected an HTTPS origin without path, userinfo, query or fragment');
  return url.origin;
}
function destination(value) {
  let url;
  try { url = new URL(value); } catch { throw new Error('Invalid Git destination'); }
  if (url.protocol !== 'https:' || url.username || url.password) throw new Error('Git credentials require an HTTPS URL without embedded credentials');
  return url.origin;
}
function strings(value) {
  if (value === undefined) return [];
  if (!Array.isArray(value) || value.some(arg => typeof arg !== 'string')) throw new Error('Helper arguments must be strings');
  return value;
}
function registration(key, value) {
  if (!/^[a-z][a-z0-9-]*$/.test(key) || ['file', 'bitwarden', 'cli'].includes(key)) throw new Error('Invalid or reserved adapter name');
  if (!object(value) || typeof value.executable !== 'string' || !path.isAbsolute(value.executable) || Object.keys(value).some(k => !['executable', 'args'].includes(k))) throw new Error('Adapter requires an absolute executable and optional arguments');
  strings(value.args);
  // Interpreters require an explicit script, not cwd-relative or inline code.
  if (/^(node(?:\d+)?|python(?:\d+(?:\.\d+)?)?|ruby|perl)(?:\.exe)?$/i.test(path.basename(value.executable))) {
    if (!value.args?.length || !path.isAbsolute(value.args[0])) throw new Error('Interpreter requires an absolute script path as its first argument');
  }
  return value;
}
function available(key, value) {
  registration(key, value);
  try {
    if (process.platform === 'win32' || /\.(cmd|bat|ps1)$/i.test(value.executable)) return false;
    fs.accessSync(value.executable, fs.constants.X_OK);
    if (!fs.statSync(value.executable).isFile()) return false;
    if (/^(node(?:\d+)?|python(?:\d+(?:\.\d+)?)?|ruby|perl)$/i.test(path.basename(value.executable))) {
      fs.accessSync(value.args[0], fs.constants.R_OK);
      if (!fs.statSync(value.args[0]).isFile()) return false;
    }
    return true;
  } catch { return false; }
}
const uuid = value => typeof value === 'string' && /^[a-f\d]{8}-(?:[a-f\d]{4}-){3}[a-f\d]{12}$/i.test(value);
function credentials(value, catalog) {
  if (!object(value)) throw new Error('Missing credential provider');
  const fields = { file: ['provider', 'user', 'password'], bitwarden: ['provider', 'itemId'], cli: ['provider', 'adapter', 'args'] }[value.provider];
  if (!fields || Object.keys(value).some(key => !fields.includes(key))) throw new Error('Invalid or conflicting credential provider fields');
  if (value.provider === 'file' && (typeof value.user !== 'string' || !value.user.trim() || typeof value.password !== 'string')) throw new Error('File credentials require a username and string password');
  if (value.provider === 'bitwarden' && !uuid(value.itemId)) throw new Error('Bitwarden requires an exact Login UUID');
  if (value.provider === 'cli') {
    if (typeof value.adapter !== 'string' || !own(catalog.cliAdapters || {}, value.adapter)) throw new Error('Unknown credential adapter');
    registration(value.adapter, catalog.cliAdapters[value.adapter]);
    strings(value.args);
  }
  return value;
}
function endpoint(value) {
  if (!object(value) || value.protocol !== 'https') throw new Error('Central SAP endpoints require HTTPS');
  host(value.host);
  const port = value.sapport ?? 443;
  if (!Number.isInteger(port) || port < 1 || port > 65535) throw new Error('Invalid SAP port');
  if (value.language !== undefined && (typeof value.language !== 'string' || !/^[a-z\d]{2}$/i.test(value.language))) throw new Error('Invalid SAP language');
  return { protocol: 'https', host: value.host, sapport: port, language: value.language ?? 'EN' };
}
function validate(value) {
  if (!object(value) || value.version !== 1) throw new Error('Unsupported or invalid central catalog');
  for (const key of ['systems', 'gitProfiles', 'cliAdapters']) {
    if (value[key] !== undefined && !object(value[key])) throw new Error('Invalid central catalog section');
  }
  return value;
}
function catalogPath() { return process.env.ABAP_SYSTEMS_PATH || path.join(os.homedir(), '.config', 'abapgit-agent', 'systems.json'); }
function readCatalog(missing = false) {
  const file = catalogPath();
  const previous = readPrivate(file, missing);
  if (previous) {
    const dir = fs.statSync(path.dirname(path.resolve(file)));
    if (dir.uid !== process.getuid() || (dir.mode & 0o077)) throw new Error('Central catalog directory must be owner-only (0700)');
    validate(previous.value);
  }
  return { file, previous, value: previous?.value ?? { version: 1, systems: {} } };
}
function saveCatalog(state) { validate(state.value); writePrivate(state.file, state.value, state.previous); }
function system(catalog, alias) {
  name(alias);
  if (!own(catalog.systems || {}, alias)) throw new Error('Unknown central SAP system');
  const entry = catalog.systems[alias];
  endpoint(entry);
  if (!object(entry.clients)) throw new Error('Invalid system clients');
  for (const key of Object.keys(entry.clients)) client(key);
  return entry;
}
module.exports = { own, object, name, client, host, origin, destination, strings, registration, available, credentials, endpoint, uuid, validate, catalogPath, readCatalog, saveCatalog, system };
