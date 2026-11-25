'use strict';

const crypto = require('node:crypto');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { safePath, readPrivate, writePrivate } = require('./private-json');
function key(config) {
  return crypto.createHash('sha256').update(JSON.stringify([config.protocol || 'https', config.host.toLowerCase(), config.sapport ?? 443, String(config.client), config.user])).digest('hex').slice(0, 32);
}
function stateFile(config, prefix, extension = '.json') {
  let dir = os.tmpdir();
  if (config.verifiedTls) {
    dir = path.join(os.homedir(), '.cache', 'abapgit-agent', 'connections');
    safePath(dir);
    fs.mkdirSync(dir, { recursive: true, mode: 0o700 });
    const stat = fs.statSync(dir);
    if (!stat.isDirectory() || stat.uid !== process.getuid() || (stat.mode & 0o077)) throw new Error('Unsafe connection-state directory');
  }
  return path.join(dir, `${extension === '.sock' ? 'd' : prefix}-v2-${extension === '.sock' ? key(config).slice(0, 16) : key(config)}${extension}`);
}
function readState(file, config) {
  return config.verifiedTls ? readPrivate(file, true)?.value ?? null : JSON.parse(fs.readFileSync(file, 'utf8'));
}
function writeState(file, value, config) {
  if (config.verifiedTls) writePrivate(file, value, readPrivate(file, true));
  else fs.writeFileSync(file, JSON.stringify(value));
}
module.exports = { key, stateFile, readState, writeState };
