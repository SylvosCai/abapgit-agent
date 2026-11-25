'use strict';

const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

function safePath(file) {
  if (process.platform === 'win32') throw new Error('Private central configuration is not supported on Windows yet');
  const absolute = path.resolve(file);
  let current = path.parse(absolute).root;
  for (const part of absolute.slice(current.length).split(path.sep)) {
    current = path.join(current, part);
    try {
      if (fs.lstatSync(current).isSymbolicLink()) throw new Error('Unsafe configuration path');
    } catch (error) {
      if (error.code !== 'ENOENT') throw error;
    }
  }
  return absolute;
}

function readPrivate(file, missing = false) {
  file = safePath(file);
  try {
    const fd = fs.openSync(file, fs.constants.O_RDONLY | fs.constants.O_NOFOLLOW | fs.constants.O_NONBLOCK);
    try {
      const stat = fs.fstatSync(fd);
      if (!stat.isFile() || stat.uid !== process.getuid() || (stat.mode & 0o077) || stat.nlink !== 1) {
        throw new Error('Configuration must be an owner-only regular file (0600)');
      }
      const text = fs.readFileSync(fd, 'utf8');
      let value;
      try { value = JSON.parse(text); } catch { throw new Error('Invalid configuration JSON'); }
      return { value, text, identity: `${stat.dev}:${stat.ino}` };
    } finally { fs.closeSync(fd); }
  } catch (error) {
    if (missing && error.code === 'ENOENT') return null;
    if (error.code) throw new Error('Cannot read private configuration');
    throw error;
  }
}

function writePrivate(file, value, previous, privateDirectory = true) {
  file = safePath(file);
  const dir = path.dirname(file);
  fs.mkdirSync(dir, { recursive: true, mode: 0o700 });
  safePath(file);
  const stat = fs.statSync(dir);
  if (stat.uid !== process.getuid() || (privateDirectory && (stat.mode & 0o077))) {
    throw new Error('Configuration directory must be owner-only (0700)');
  }
  const lock = `${file}.lock`;
  let lockFd, temporary;
  try {
    lockFd = fs.openSync(lock, 'wx', 0o600);
    const latest = readPrivate(file, true);
    if ((previous === null && latest !== null) || (previous && (!latest || latest.text !== previous.text || latest.identity !== previous.identity))) {
      throw new Error('Configuration changed concurrently; retry');
    }
    temporary = path.join(dir, `.abgagt-${crypto.randomBytes(12).toString('hex')}.tmp`);
    const fd = fs.openSync(temporary, 'wx', 0o600);
    try { fs.writeFileSync(fd, `${JSON.stringify(value, null, 2)}\n`); fs.fsyncSync(fd); } finally { fs.closeSync(fd); }
    // Recheck manual edits before committing; the lock serializes CLI writers.
    const beforeRename = readPrivate(file, true);
    if (latest?.text !== beforeRename?.text || latest?.identity !== beforeRename?.identity) throw new Error('Configuration changed concurrently; retry');
    fs.renameSync(temporary, file);
    temporary = null;
  } catch (error) {
    if (error.code) throw new Error(error.code === 'EEXIST' ? 'Configuration is being edited; retry' : 'Cannot safely write private configuration');
    throw error;
  } finally {
    if (temporary) fs.unlinkSync(temporary);
    if (lockFd !== undefined) { fs.closeSync(lockFd); fs.unlinkSync(lock); }
  }
}

module.exports = { safePath, readPrivate, writePrivate };
