'use strict';

const assert = require('assert');
const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const cp = require('child_process');
const files = require('../../src/utils/so10-files');
const { getTestRepoUrl } = require('./test-repos');

const KEY = { object: 'TEXT', name: 'ZABGAGT_SO10_TEST', id: 'ST', language: 'E' };
const OWNER = 'abgagt.so10.test';
const field = (result, name) => Object.prototype.hasOwnProperty.call(result, name)
  ? result[name] : result[name.toUpperCase()];

// Explicit --cmd --command=so10 only: no global setup or unattended CI enrollment.
function runSo10Tests(repoRoot, printers) {
  const start = Date.now();
  const results = [];
  let work, lock, lockPath;
  const step = (name, fn) => {
    try {
      fn();
      results.push({ command: 'so10', name, passed: true, output: '' });
      printers.printSuccess(name);
    } catch (error) {
      results.push({ command: 'so10', name, passed: false, output: error.message });
      throw error;
    }
  };
  printers.printSubHeader('SO10 named-text lifecycle (no CTS)');
  try {
    let config;
    try { config = JSON.parse(fs.readFileSync(path.join(repoRoot, '.abapGitAgent'), 'utf8')); }
    catch { throw new Error('SO10 lifecycle requires a valid .abapGitAgent configuration'); }
    const source = String(config.client || '');
    assert(/^\d{3}$/.test(source) && source.length === 3, 'Configured development client must have three digits');
    const copySource = config.testSo10?.copySource;
    if (copySource) {
      files.validateKey(copySource.key);
      assert(typeof copySource.client === 'string' && /^\d{3}$/.test(copySource.client) && copySource.client.length === 3 && copySource.client !== source,
        'testSo10.copySource.client must be a distinct read-only source client');
      assert(!/[+?#%]/.test(copySource.key.name), 'COPY fixture name must support literal import discovery');
    }
    const url = config.testRepos?.so10 || getTestRepoUrl('so10');
    assert(typeof url === 'string' && url.length > 0, 'Set testRepos.so10 to the clone-only fixture repository URL');
    if (/^https?:/i.test(url)) {
      const parsed = new URL(url);
      assert(!parsed.username && !parsed.password, 'Fixture repository URL must not contain credentials');
    }
    lockPath = path.join(os.tmpdir(), `abgagt-so10-${crypto.createHash('sha256')
      .update(JSON.stringify([config.host, source])).digest('hex')}.lock`);
    try { lock = fs.openSync(lockPath, 'wx', 0o600); }
    catch { throw new Error('SO10 fixture is locked by another run; inspect the lock before retrying'); }
    fs.writeFileSync(lock, String(process.pid));
    work = fs.mkdtempSync(path.join(os.tmpdir(), 'abgagt-so10-'));
    const clone = path.join(work, 'repository');
    const standalone = path.join(work, 'standalone');
    const git = (args, cwd = clone) => cp.execFileSync('git', args, { cwd, encoding: 'utf8', stdio: 'pipe', timeout: 60000 });
    try { git(['clone', '--', url, clone], work); }
    catch { throw new Error('Could not clone SO10 fixture repository; check URL and Git authentication'); }
    const repository = files.getRepository(clone);
    assert.equal(repository.repoId, OWNER, 'Unexpected SO10 fixture repository identity');
    const entries = files.readManagedFiles(repository);
    assert.equal(entries.size, 1, 'Fixture repository must enroll exactly one text');
    const entry = files.readCommitted(repository, KEY);
    const relativePath = path.relative(repository.root, entry.file);
    const baseline = entry.payload;
    const updated = files.validatePayload(JSON.parse(fs.readFileSync(path.join(clone, 'fixtures/update.itf.json'), 'utf8')), KEY);
    const systemEdit = files.validatePayload(JSON.parse(fs.readFileSync(path.join(clone, 'fixtures/system-edit.itf.json'), 'utf8')), KEY);
    fs.mkdirSync(standalone);
    // Credentials exist only in owned 0700 temporary directories and 0600 ignored files.
    for (const cwd of [clone, standalone]) {
      fs.writeFileSync(path.join(cwd, '.abapGitAgent'), JSON.stringify(config), { mode: 0o600, flag: 'wx' });
    }
    const hooks = path.join(work, 'empty-hooks');
    fs.mkdirSync(hooks);
    const commitPayload = () => {
      git(['add', '--', relativePath]);
      if (git(['diff', '--cached', '--name-only']).trim()) {
        git(['-c', `core.hooksPath=${hooks}`, '-c', 'commit.gpgsign=false',
          '-c', 'user.name=SO10 Integration Test', '-c', 'user.email=so10-test@example.invalid',
          'commit', '-m', 'test: commit selected SO10 lifecycle payload']);
      }
    };
    const execute = (args, cwd = clone, expectedSuccess = true) => {
      const session = fs.mkdtempSync(path.join(work, 'session-'));
      let execution;
      try {
        execution = cp.spawnSync(process.execPath, [path.join(repoRoot, 'bin/abapgit-agent'), 'so10', ...args, '--json'], {
          cwd, env: { ...process.env, TMPDIR: session }, encoding: 'utf8', timeout: 120000,
          maxBuffer: 1024 * 1024
        });
      } finally { fs.rmSync(session, { recursive: true, force: true }); }
      assert(!execution.error && !execution.signal && Number.isInteger(execution.status), 'SO10 subprocess did not finish normally');
      let result;
      try { result = JSON.parse(execution.stdout); }
      catch { throw new Error('SO10 subprocess did not return valid JSON; raw output is intentionally not logged'); }
      assert.equal(field(result, 'success'), expectedSuccess, 'Unexpected SO10 semantic outcome');
      assert.equal(execution.status, expectedSuccess ? 0 : 1, 'Unexpected SO10 exit status');
      return result;
    };
    const argumentsFor = key => [key.name, '--id', key.id, '--language', key.language];
    const importArguments = key => ['import', '--prefix', key.name, '--name', key.name, '--id', key.id, '--language', key.language];
    const keyArgs = argumentsFor(KEY);
    const verifyKey = (result, client, key = KEY) => {
      for (const [name, value] of Object.entries(key)) assert.equal(field(result, name), value, `Unexpected returned ${name}`);
      assert.equal(field(result, 'data_client'), client, 'Unexpected returned data client');
      assert.equal(field(result, 'client'), source, 'Unexpected HTTP logon client; no cross-client write is authorized');
    };
    const read = (client, payload, key = KEY) => {
      const result = execute(['read', ...argumentsFor(key), '--source-client', client], clone, !!payload);
      verifyKey(result, client, key);
      if (payload) assert.deepStrictEqual(field(result, 'lines'), payload.lines, 'Fresh-session ITF readback differs');
      else assert.equal(field(result, 'error_kind'), 'NOT_FOUND', 'Missing text must be a native NOT_FOUND, not another failure');
      return result;
    };
    step('preflight: development fixture has no preexisting text or baseline', () => {
      const result = read(source, null);
      assert.strictEqual(field(result, 'base_found'), false, 'Preexisting baseline requires explicit recovery, not automatic cleanup');
    });
    const write = (action, payload) => {
      const args = [action, ...keyArgs];
      if (payload) args.push('--itf-file', path.join(clone, 'fixtures/update.itf.json'));
      const result = execute(args);
      verifyKey(result, source);
      assert.strictEqual(field(result, 'saved'), true, 'Text save was not verified');
      assert.strictEqual(field(result, 'baseline_recorded'), true, 'Baseline was not verified');
    };
    step('create committed baseline and verify fresh-session readback', () => { write('create'); read(source, baseline); });
    step('explicit import preserves payload and confirms baseline', () => {
      const result = execute(importArguments(KEY));
      assert.strictEqual(field(result, 'saved'), false);
      assert.equal(field(result, 'files')?.length, 1, 'Import must contain exactly the named fixture');
      assert.strictEqual(field(field(result, 'files')[0], 'baseline_recorded'), true);
      read(source, baseline);
    });
    step('managed update and committed payload', () => { write('update', updated); read(source, updated); commitPayload(); });
    step('simulate a same-account system edit without advancing baseline', () => {
      const input = path.join(standalone, 'system-edit.itf.json');
      fs.writeFileSync(input, JSON.stringify(systemEdit));
      const result = execute(['update', ...keyArgs, '--itf-file', input, '--conflict-mode', 'ignore'], standalone);
      assert.strictEqual(field(result, 'saved'), true);
      assert.strictEqual(field(result, 'baseline_recorded'), false);
      read(source, systemEdit);
    });
    step('managed UPDATE rejects system edit without changing text', () => {
      const result = execute(['update', ...keyArgs], clone, false);
      assert.equal(field(result, 'conflict_kind'), 'LOCAL_EDIT');
      assert.strictEqual(field(result, 'saved'), false);
      read(source, systemEdit);
    });
    step('explicit managed ignore repairs the named development fixture', () => {
      const result = execute(['update', ...keyArgs, '--conflict-mode', 'ignore']);
      assert.strictEqual(field(result, 'saved'), true);
      assert.strictEqual(field(result, 'baseline_recorded'), true);
      read(source, updated);
    });
    step('identical managed UPDATE verifies baseline without another save', () => {
      const result = execute(['update', ...keyArgs]);
      assert.strictEqual(field(result, 'saved'), false);
      assert.strictEqual(field(result, 'baseline_recorded'), true);
      read(source, updated);
    });
    step('DROP refuses dirty Git payload', () => {
      fs.writeFileSync(entry.file, JSON.stringify({ ...updated, lines: [...updated.lines, { tdformat: '*', tdline: 'Dirty local fixture' }] }));
      const result = execute(['drop', ...keyArgs], clone, false);
      assert.strictEqual(field(result, 'deleted'), false);
      read(source, updated);
      git(['restore', '--', relativePath]);
    });
    for (const client of [source]) {
      step(`Git-protected DROP and fresh absence verification (${client})`, () => {
        const result = execute(['drop', ...keyArgs, '--target-client', client]);
        verifyKey(result, client);
        assert.strictEqual(field(result, 'saved'), false);
        assert.strictEqual(field(result, 'deleted'), true);
        assert.strictEqual(field(result, 'baseline_removed'), true);
        const absent = read(client, null);
        assert.strictEqual(field(absent, 'base_found'), false);
        files.readCommitted(repository, KEY);
      });
    }
    if (copySource) {
      const key = copySource.key;
      const copyArgs = argumentsFor(key);
      let observed;
      let document;
      step('COPY preflight: existing read-only source and absent development target', () => {
        const result = execute(['read', ...copyArgs, '--source-client', copySource.client]);
        verifyKey(result, copySource.client, key);
        document = files.validatePayload({ version: 1, key, lines: field(result, 'lines') }, key);
        observed = result;
        const absent = read(source, null, key);
        assert.strictEqual(field(absent, 'base_found'), false, 'COPY target must not have a preexisting baseline');
      });
      step('COPY existing source into development only, without metadata advance', () => {
        const result = execute(['copy', ...copyArgs, '--source-client', copySource.client, '--target-client', source]);
        verifyKey(result, source, key);
        assert.equal(field(result, 'source_client'), copySource.client);
        assert.strictEqual(field(result, 'saved'), true);
        assert.strictEqual(field(result, 'baseline_recorded'), false);
        read(source, document, key);
      });
      step('copied text cannot DROP before explicit adoption', () => {
        const result = execute(['drop', ...copyArgs], clone, false);
        assert.strictEqual(field(result, 'deleted'), false);
        read(source, document, key);
      });
      step('import copied development text and commit its local cleanup proof', () => {
        const result = execute([...importArguments(key), '--adopt-system']);
        assert.strictEqual(field(result, 'saved'), false);
        assert.equal(field(result, 'files')?.length, 1);
        assert.strictEqual(field(field(result, 'files')[0], 'baseline_recorded'), true);
        const copied = files.readManaged(repository, key);
        const relative = path.relative(repository.root, copied.file);
        git(['add', '--', relative]);
        if (git(['diff', '--cached', '--name-only']).trim()) git(['-c', `core.hooksPath=${hooks}`, '-c', 'commit.gpgsign=false',
          '-c', 'user.name=SO10 Integration Test', '-c', 'user.email=so10-test@example.invalid', 'commit', '-m', 'test: retain local COPY cleanup proof']);
        files.readCommitted(repository, key);
      });
      step('DROP copied development text; source text and baseline remain unchanged', () => {
        const result = execute(['drop', ...copyArgs]);
        verifyKey(result, source, key);
        assert.strictEqual(field(result, 'saved'), false);
        assert.strictEqual(field(result, 'deleted'), true);
        assert.strictEqual(field(result, 'baseline_removed'), true);
        const absent = read(source, null, key);
        assert.strictEqual(field(absent, 'base_found'), false);
        const unchanged = read(copySource.client, document, key);
        for (const name of ['source_sha', 'base_found', 'base_repo_id', 'base_sha']) assert.deepStrictEqual(field(unchanged, name), field(observed, name), 'Source metadata changed');
        files.readCommitted(repository, key);
      });
    } else printers.printInfo('Live COPY not run: configure an approved existing testSo10.copySource. No other-client fixture is created.');
  } catch (error) {
    if (!results.some(result => !result.passed)) results.push({ command: 'so10', name: 'SO10 setup', passed: false, output: error.message });
    printers.printError(error.message);
  } finally {
    if (work) {
      for (const cwd of ['repository', 'standalone']) fs.rmSync(path.join(work, cwd, '.abapGitAgent'), { force: true });
      if (results.length && results.every(result => result.passed)) fs.rmSync(work, { recursive: true, force: true });
      else printers.printInfo(`Owned fixture artifacts retained at ${work}; credentials removed. No automatic SAP cleanup attempted.`);
    }
    if (lock !== undefined) { fs.closeSync(lock); fs.unlinkSync(lockPath); }
  }
  const passedCount = results.filter(result => result.passed).length;
  return { success: results.length > 0 && passedCount === results.length, results,
    duration: ((Date.now() - start) / 1000).toFixed(1), passedCount, totalCount: results.length };
}

module.exports = { runSo10Tests };
