const fs = require('fs');
const path = require('path');
const { execFileSync } = require('child_process');
const { createHash, randomBytes } = require('crypto');
const { TextDecoder } = require('util');

function validateLines(lines) {
  if (!Array.isArray(lines) || !lines.length || !lines.some((line) => typeof line?.tdline === 'string' && line.tdline.trim())) {
    throw new Error('Empty text is not supported; saving it could delete a text');
  }
  for (const line of lines) {
    if (!line || typeof line.tdformat !== 'string' || typeof line.tdline !== 'string' ||
        Object.keys(line).some((key) => !['tdformat', 'tdline'].includes(key)) ||
        line.tdformat.length > 2 || line.tdline.length > 132 || /[\r\n\0]/.test(line.tdformat + line.tdline)) {
      throw new Error('Invalid ITF line: tdformat <= 2 and tdline <= 132 characters, without embedded line delimiters');
    }
  }
}

function validateKey(key) {
  if (!key || Array.isArray(key) || Object.keys(key).some((field) => !['object', 'name', 'id', 'language'].includes(field)) ||
      key.object !== 'TEXT' || typeof key.name !== 'string' || !key.name.trim() || key.name.length > 70 || key.name.endsWith(' ') || /[\x00-\x1f\x7f*]/.test(key.name) ||
      typeof key.id !== 'string' || key.id.length < 1 || key.id.length > 4 || /[^A-Z0-9_]/.test(key.id) ||
      typeof key.language !== 'string' || key.language.length !== 1 || !/^[A-Z0-9]$/.test(key.language)) {
    throw new Error('Invalid managed SO10 key; use TEXT, an exact Text name, Text ID and canonical SAP language');
  }
}

function keyTuple(key) {
  validateKey(key);
  return JSON.stringify([key.object, key.name, key.id, key.language]);
}

function validatePayload(payload, expectedKey) {
  if (!payload || Array.isArray(payload) || payload.version !== 1 ||
      Object.keys(payload).some((field) => !['version', 'key', 'lines'].includes(field))) {
    throw new Error('Unsupported SO10 ITF payload; expected version 1, key and lines');
  }
  validateKey(payload.key);
  validateLines(payload.lines);
  if (expectedKey && keyTuple(payload.key) !== keyTuple(expectedKey)) {
    throw new Error('SO10 payload key does not match the requested text');
  }
  return payload;
}

function filename(key) {
  const digest = createHash('sha256').update(keyTuple(key), 'utf8').digest('hex');
  const slug = key.name.replace(/[^A-Za-z0-9_-]/g, '_').slice(0, 24) || 'text';
  return `${slug}.${key.id}.${key.language}.${digest}.itf.json`;
}

function safePath(repository, relativePath) {
  if (typeof relativePath !== 'string' || !relativePath.trim() || path.isAbsolute(relativePath) ||
      relativePath.split(/[\\/]/).includes('..') || relativePath.includes('\0')) {
    throw new Error('SO10 paths must stay inside the repository');
  }
  const target = path.resolve(repository.root, relativePath);
  const relative = path.relative(repository.root, target);
  if (relative === '..' || relative.startsWith(`..${path.sep}`) || path.isAbsolute(relative)) {
    throw new Error('SO10 paths must stay inside the repository');
  }
  let current = repository.root;
  for (const part of relative.split(path.sep).filter(Boolean)) {
    current = path.join(current, part);
    try {
      if (fs.lstatSync(current).isSymbolicLink()) throw new Error('SO10 managed paths cannot contain symlinks');
    } catch (error) {
      if (error.code !== 'ENOENT') throw error;
    }
  }
  return target;
}

function readJson(file) {
  try {
    return JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(fs.readFileSync(file)));
  } catch (_) {
    throw new Error('SO10 repository file must be readable, valid UTF-8 JSON');
  }
}

function getRepository(cwd = process.cwd(), { optional = false } = {}) {
  let root;
  try {
    root = fs.realpathSync(execFileSync('git', ['rev-parse', '--show-toplevel'], { cwd, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim());
  } catch (error) {
    if (optional && error.status === 128 && /not a git repository/i.test(String(error.stderr))) return null;
    throw new Error('SO10 managed operations require a Git repository');
  }
  const configFile = safePath({ root }, '.abapgit-agent.json');
  if (optional && !fs.existsSync(configFile)) return null;
  const settings = readJson(configFile).so10;
  if (optional && settings === undefined) return null;
  try {
    execFileSync('git', ['ls-files', '--error-unmatch', '--', '.abapgit-agent.json'], { cwd: root, stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (_) {
    throw new Error('Track .abapgit-agent.json before using managed SO10 operations');
  }
  if (!settings || Array.isArray(settings) || typeof settings.repoId !== 'string' || settings.repoId.length > 80 ||
      !/^[a-z0-9]/.test(settings.repoId) || /[^a-z0-9._-]/.test(settings.repoId)) {
    throw new Error('Set tracked so10.repoId to a stable lowercase repository identity');
  }
  const repository = { root, repoId: settings.repoId };
  repository.folder = safePath(repository, settings.folder ?? 'texts/so10');
  return repository;
}

function readManagedFiles(repository) {
  const files = new Map();
  if (!fs.existsSync(repository.folder)) return files;
  for (const entry of fs.readdirSync(repository.folder, { withFileTypes: true })) {
    if (!entry.name.endsWith('.itf.json')) continue;
    const file = safePath(repository, path.relative(repository.root, path.join(repository.folder, entry.name)));
    if (!entry.isFile()) throw new Error('SO10 managed payload must be a regular file');
    const payload = validatePayload(readJson(file));
    const tuple = keyTuple(payload.key);
    if (files.has(tuple)) throw new Error('Duplicate managed SO10 text key');
    files.set(tuple, { file, payload });
  }
  return files;
}

function readManaged(repository, key) {
  const entry = readManagedFiles(repository).get(keyTuple(key));
  if (!entry) throw new Error('SO10 text is not imported; use LIST/READ and explicit IMPORT first');
  return entry;
}

function readCommitted(repository, key, expectedHead) {
  const entry = readManaged(repository, key);
  const git = (args) => execFileSync('git', ['--literal-pathspecs', ...args], {
    cwd: repository.root, stdio: ['ignore', 'pipe', 'pipe']
  });
  try {
    const head = git(['rev-parse', '--verify', 'HEAD^{commit}']).toString().trim();
    if (expectedHead && head !== expectedHead) throw new Error('HEAD changed');
    const configFile = safePath(repository, '.abapgit-agent.json');
    let payload;
    for (const file of [configFile, entry.file]) {
      const relative = path.relative(repository.root, file).split(path.sep).join('/');
      if (git(['status', '--porcelain', '--untracked-files=all', '--', relative]).toString().trim()) throw new Error('Uncommitted work');
      const tree = git(['ls-tree', head, '--', relative]).toString();
      if (!/^100(644|755) blob /.test(tree)) throw new Error('Not a committed regular file');
      const committed = git(['cat-file', 'blob', `${head}:${relative}`]);
      if (!committed.equals(fs.readFileSync(file))) throw new Error('Payload changed');
      const data = JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(committed));
      if (file === configFile) {
        if (data.so10?.repoId !== repository.repoId || safePath(repository, data.so10.folder ?? 'texts/so10') !== repository.folder) {
          throw new Error('Repository configuration changed');
        }
      } else payload = validatePayload(data, key);
    }
    if (git(['rev-parse', '--verify', 'HEAD^{commit}']).toString().trim() !== head) throw new Error('HEAD changed');
    return { ...entry, payload, head };
  } catch (_) {
    throw new Error('SO10 drop requires a clean committed payload/configuration at unchanged HEAD; staged or untracked work is not recoverable Git history');
  }
}

function writeManaged(repository, payload, { replaceClean = false } = {}) {
  validatePayload(payload);
  const entry = readManagedFiles(repository).get(keyTuple(payload.key));
  const file = safePath(repository, path.relative(repository.root, entry?.file ?? path.join(repository.folder, filename(payload.key))));
  const signature = (data) => JSON.stringify([keyTuple(data.key), data.lines.map(({ tdformat, tdline }) => [tdformat, tdline])]);
  const previous = entry ? fs.readFileSync(file) : null;
  if (previous) {
    let current;
    try { current = validatePayload(JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(previous)), payload.key); }
    catch (_) { throw new Error('Managed SO10 file changed or became invalid before preparation'); }
    if (signature(current) === signature(payload)) return { file, fileWritten: false };
    if (!replaceClean) throw new Error('Managed SO10 payload differs; use explicit system adoption');
  }
  const assertUnchanged = () => {
    safePath(repository, path.relative(repository.root, file));
    if (previous) {
      const relative = path.relative(repository.root, file).split(path.sep).join('/');
      try { execFileSync('git', ['--literal-pathspecs', 'ls-files', '--error-unmatch', '--', relative], { cwd: repository.root, stdio: ['ignore', 'pipe', 'pipe'] }); }
      catch (_) { throw new Error('Refusing to replace untracked SO10 work'); }
      const status = execFileSync('git', ['--literal-pathspecs', 'status', '--porcelain', '--', relative], { cwd: repository.root, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
      // Status can hide local edits; filtered index bytes also preserve clean checkout line endings.
      const indexed = execFileSync('git', ['cat-file', '--filters', `:${relative}`], { cwd: repository.root, maxBuffer: Math.max(previous.length, 1024 * 1024), stdio: ['ignore', 'pipe', 'pipe'] });
      if (status.trim() || !indexed.equals(previous) || !fs.readFileSync(file).equals(previous)) throw new Error('Refusing to replace changed or uncommitted SO10 work');
    } else if (fs.existsSync(file)) throw new Error('SO10 payload was created concurrently');
  };
  assertUnchanged();
  fs.mkdirSync(repository.folder, { recursive: true });
  safePath(repository, path.relative(repository.root, repository.folder));
  const temporary = safePath(repository, path.relative(repository.root, path.join(repository.folder, `.so10-${randomBytes(8).toString('hex')}.tmp`)));
  let fd, fileWritten = false;
  try {
    fd = fs.openSync(temporary, 'wx', 0o600);
    fs.writeFileSync(fd, `${JSON.stringify(payload, null, 2)}\n`, 'utf8');
    fs.fsyncSync(fd);
    fs.closeSync(fd); fd = undefined;
    // Recheck local work before the atomic replacement; no Git staging or commits are performed.
    assertUnchanged();
    if (previous) fs.renameSync(temporary, file);
    else fs.linkSync(temporary, file);
    fileWritten = true;
    return { file, fileWritten };
  } catch (error) {
    error.fileWritten = fileWritten;
    throw error;
  } finally {
    try {
      if (fd !== undefined) fs.closeSync(fd);
      if (fs.existsSync(temporary)) fs.unlinkSync(temporary);
    } catch (error) {
      error.fileWritten = fileWritten;
      throw error;
    }
  }
}

module.exports = { validateLines, validateKey, validatePayload, keyTuple, filename, safePath, getRepository, readManagedFiles, readManaged, readCommitted, writeManaged };
