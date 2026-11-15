'use strict';

const fs = require('fs');
const os = require('os');
const path = require('path');
const cp = require('child_process');
const crypto = require('crypto');
const files = require('../../src/utils/so10-files');
const { runSo10Tests } = require('../integration/so10-runner');

const key = { object: 'TEXT', name: 'ZABGAGT_SO10_TEST', id: 'ST', language: 'E' };
const copyKey = { ...key, name: 'EXISTING_COPY_FIXTURE' };
const payload = text => ({ version: 1, key, lines: [{ tdformat: '*', tdline: text }, { tdformat: '', tdline: '  café — 日本語' }] });
const hash = lines => crypto.createHash('sha256').update(JSON.stringify(lines)).digest('hex');
let root, config, repository, printers, states, calls, sessions, spawn;
const tuple = (client, k) => `${client}:${files.keyTuple(k)}`;
const state = (client, k = key) => {
  const id = tuple(client, k);
  if (!states.has(id)) states.set(id, { lines: null, baseline: null });
  return states.get(id);
};
const git = args => cp.execFileSync('git', ['-c', 'core.hooksPath=/dev/null', '-c', 'commit.gpgsign=false', ...args], { cwd: repository, stdio: 'pipe' });

function simulate(_node, argv, options) {
  const args = argv.slice(2);
  const action = args[0];
  const option = name => args[args.indexOf(name) + 1];
  const k = { ...key, name: action === 'import' ? option('--name') : args[1] };
  if (action !== 'import') {
    expect(args[1]).toBe(k.name);
    expect(args[1]).not.toMatch(/^--/);
    expect(args).not.toContain('--name');
  }
  expect(args).not.toContain('--file');
  const client = args.includes('--source-client') ? option('--source-client') : '123';
  calls.push({ action, args, cwd: options.cwd });
  sessions.push(options.env.TMPDIR);
  expect(fs.statSync(path.join(options.cwd, '.abapGitAgent')).mode & 0o777).toBe(0o600);
  expect(options.shell).toBeUndefined();
  let success = true;
  let result = { success: true, ...k, client: '123', data_client: client, saved: false, baseline_recorded: false };
  const current = state('123', k);
  if (action === 'read') {
    const s = state(client, k);
    success = !!s.lines;
    result = { ...result, lines: s.lines || [], source_sha: s.lines ? hash(s.lines) : '',
      base_found: !!s.baseline, base_repo_id: s.baseline?.owner || '', base_sha: s.baseline?.sha || '',
      ...(success ? {} : { error_kind: 'NOT_FOUND' }) };
  } else if (action === 'create' || action === 'update') {
    const repo = files.getRepository(options.cwd, { optional: true });
    const desired = args.includes('--itf-file') ? JSON.parse(fs.readFileSync(option('--itf-file'))) : files.readManaged(repo, k).payload;
    if (repo && action === 'update' && !args.includes('--conflict-mode') && current.baseline.sha !== hash(current.lines)) {
      success = false;
      result.conflict_kind = 'LOCAL_EDIT';
    } else {
      const changed = !current.lines || hash(current.lines) !== hash(desired.lines);
      if (repo && args.includes('--itf-file')) files.writeManaged(repo, desired, { replaceClean: true });
      current.lines = desired.lines;
      if (repo) current.baseline = { owner: repo.repoId, sha: hash(desired.lines) };
      result = { ...result, saved: changed, baseline_recorded: !!repo };
    }
  } else if (action === 'import') {
    const repo = files.getRepository(options.cwd);
    const s = state(client, k);
    files.writeManaged(repo, { version: 1, key: k, lines: s.lines }, { replaceClean: args.includes('--adopt-system') });
    s.baseline = { owner: repo.repoId, sha: hash(s.lines) };
    result = { ...result, files: [{ baseline_recorded: true }] };
  } else if (action === 'drop') {
    try {
      const repo = files.getRepository(options.cwd);
      const committed = files.readCommitted(repo, k);
      if (!current.baseline || current.baseline.owner !== repo.repoId || current.baseline.sha !== hash(committed.payload.lines)) throw new Error('Unknown baseline');
      current.lines = null;
      current.baseline = null;
      result = { ...result, data_client: '123', deleted: true, baseline_removed: true };
    } catch {
      success = false;
      result = { ...result, deleted: false, baseline_removed: false };
    }
  } else if (action === 'copy') {
    expect(option('--target-client')).toBe('123');
    expect(args).not.toContain('--overwrite');
    current.lines = state(client, k).lines;
    result = { ...result, data_client: '123', source_client: client, saved: true };
  } else throw new Error(`Unexpected action: ${action}`);
  result.success = success;
  return { status: success ? 0 : 1, stdout: JSON.stringify(result), stderr: '', signal: null };
}

beforeEach(() => {
  root = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-runner-unit-'));
  repository = path.join(root, 'fixtures');
  fs.mkdirSync(repository);
  git(['init', '--initial-branch=main']);
  fs.writeFileSync(path.join(repository, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'abgagt.so10.test' } }));
  fs.mkdirSync(path.join(repository, 'texts/so10'), { recursive: true });
  fs.writeFileSync(path.join(repository, 'texts/so10', files.filename(key)), JSON.stringify(payload('baseline')));
  fs.mkdirSync(path.join(repository, 'fixtures'));
  fs.writeFileSync(path.join(repository, 'fixtures/update.itf.json'), JSON.stringify(payload('updated')));
  fs.writeFileSync(path.join(repository, 'fixtures/system-edit.itf.json'), JSON.stringify(payload('system edit')));
  git(['add', '--', '.']);
  git(['-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', 'commit', '-m', 'fixtures']);
  config = { host: 'example.invalid', client: '123', user: 'TEST', password: 'FAKE', testRepos: { so10: repository } };
  fs.writeFileSync(path.join(root, '.abapGitAgent'), JSON.stringify(config));
  printers = Object.fromEntries(['printSubHeader', 'printInfo', 'printSuccess', 'printError'].map(name => [name, jest.fn()]));
  states = new Map(); calls = []; sessions = [];
  spawn = jest.spyOn(cp, 'spawnSync').mockImplementation(simulate);
});

afterEach(() => {
  jest.restoreAllMocks();
  for (const message of printers.printInfo.mock.calls.flat()) {
    const match = String(message).match(/artifacts retained at (.*?);/);
    if (match) fs.rmSync(match[1], { recursive: true, force: true });
  }
  fs.rmSync(root, { recursive: true, force: true });
});

test('development-only lifecycle succeeds, retains source Git, isolates sessions and performs no CTS or other-client write', () => {
  const head = git(['rev-parse', 'HEAD']).toString();
  const result = runSo10Tests(root, printers);
  expect(result.results.filter(step => !step.passed)).toEqual([]);
  expect(result.success).toBe(true);
  expect(result.totalCount).toBe(10);
  expect(state('123').lines).toBeNull();
  expect(state('123').baseline).toBeNull();
  expect(git(['rev-parse', 'HEAD']).toString()).toBe(head);
  expect(calls.some(call => ['copy', 'transport'].includes(call.action))).toBe(false);
  expect(printers.printInfo).toHaveBeenCalledWith(expect.stringContaining('Live COPY not run'));
  expect(new Set(sessions).size).toBe(sessions.length);
  expect(sessions.every(dir => !fs.existsSync(dir))).toBe(true);
});

test.each(['text', 'baseline'])('preexisting %s stops before any mutation', existing => {
  state('123')[existing === 'text' ? 'lines' : 'baseline'] = existing === 'text' ? payload('existing').lines : { owner: 'other', sha: 'a'.repeat(64) };
  expect(runSo10Tests(root, printers).success).toBe(false);
  expect(calls.every(call => call.action === 'read')).toBe(true);
});

test('unexpected HTTP logon client stops in read-only preflight before any write', () => {
  spawn.mockImplementation((node, argv, options) => {
    const result = simulate(node, argv, options);
    return { ...result, stdout: JSON.stringify({ ...JSON.parse(result.stdout), client: '124' }) };
  });
  expect(runSo10Tests(root, printers).success).toBe(false);
  expect(calls.every(call => call.action === 'read')).toBe(true);
  expect(state('123').lines).toBeNull();
});

test('post-save failure stops without automatic DROP and removes temporary credentials', () => {
  spawn.mockImplementation((node, argv, options) => {
    const result = simulate(node, argv, options);
    if (argv[2] === 'create') return { ...result, status: 1, stdout: JSON.stringify({ success: false, saved: true, baseline_recorded: false }) };
    return result;
  });
  expect(runSo10Tests(root, printers).success).toBe(false);
  expect(state('123').lines).not.toBeNull();
  expect(calls.some(call => call.action === 'drop')).toBe(false);
  const retained = printers.printInfo.mock.calls.flat().join('').match(/artifacts retained at (.*?);/)[1];
  expect(fs.existsSync(path.join(retained, 'repository/.abapGitAgent'))).toBe(false);
  expect(fs.existsSync(path.join(retained, 'standalone/.abapGitAgent'))).toBe(false);
});

test('optional existing source is read-only; COPY and cleanup write only development', () => {
  config.testSo10 = { copySource: { client: '000', key: copyKey } };
  fs.writeFileSync(path.join(root, '.abapGitAgent'), JSON.stringify(config));
  const original = { lines: payload('existing source').lines, baseline: { owner: 'SOURCE_OWNER', sha: hash(payload('existing source').lines) } };
  states.set(tuple('000', copyKey), structuredClone(original));
  const result = runSo10Tests(root, printers);
  expect(result.results.filter(step => !step.passed)).toEqual([]);
  expect(result.success).toBe(true);
  expect(result.totalCount).toBe(15);
  expect(state('000', copyKey)).toEqual(original);
  expect(state('123', copyKey)).toEqual({ lines: null, baseline: null });
  expect(calls.filter(call => call.args.includes('000')).every(call => ['read', 'copy'].includes(call.action))).toBe(true);
});

test('COPY target preexistence aborts without COPY or overwrite', () => {
  config.testSo10 = { copySource: { client: '000', key: copyKey } };
  fs.writeFileSync(path.join(root, '.abapGitAgent'), JSON.stringify(config));
  state('000', copyKey).lines = payload('source').lines;
  state('123', copyKey).lines = payload('do not touch').lines;
  expect(runSo10Tests(root, printers).success).toBe(false);
  expect(calls.some(call => call.action === 'copy')).toBe(false);
  expect(state('123', copyKey).lines).toEqual(payload('do not touch').lines);
});

test('invalid JSON and arbitrary subprocess failures never count as NOT_FOUND', () => {
  spawn.mockReturnValue({ status: 1, stdout: 'SECRET raw output', stderr: 'SECRET', signal: null });
  expect(runSo10Tests(root, printers).success).toBe(false);
  expect(printers.printError.mock.calls.flat().join('')).not.toContain('SECRET');
});

test('existing lock is not removed and blocks all CLI calls', () => {
  const lock = path.join(os.tmpdir(), `abgagt-so10-${crypto.createHash('sha256').update(JSON.stringify([config.host, '123'])).digest('hex')}.lock`);
  fs.writeFileSync(lock, 'owned test lock', { flag: 'wx' });
  try {
    expect(runSo10Tests(root, printers).success).toBe(false);
    expect(spawn).not.toHaveBeenCalled();
    expect(fs.readFileSync(lock, 'utf8')).toBe('owned test lock');
  } finally { fs.unlinkSync(lock); }
});
