const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');
const command = require('../../src/commands/so10');
const { filename } = require('../../src/utils/so10-files');

describe('SO10 managed/default and external writes', () => {
  let root, originalCwd, key, document, http, context, log;
  const file = () => path.join(root, 'texts/so10', filename(key));
  const prepare = () => { fs.mkdirSync(path.dirname(file()), { recursive: true }); fs.writeFileSync(file(), JSON.stringify(document)); };
  const commit = () => {
    execFileSync('git', ['add', '.'], { cwd: root });
    execFileSync('git', ['-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid', '-c', 'commit.gpgsign=false', '-c', 'core.hooksPath=/dev/null', 'commit', '-qm', 'owned fixture'], { cwd: root });
  };
  const response = (body) => ({ success: true, action: body.action, object: body.object, name: body.name, id: body.id,
    language: body.language, data_client: '123', saved: true, baseline_recorded: Boolean(body.repo_id),
    source_sha: 'a'.repeat(64), base_sha: 'a'.repeat(64), base_repo_id: body.repo_id });
  const execute = async (...extra) => {
    await command.execute(['update', key.name, '--language', key.language, '--json', ...extra], context);
    return JSON.parse(log.mock.calls.at(-1)[0]);
  };
  beforeEach(() => {
    originalCwd = process.cwd(); root = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-write-owned-'));
    execFileSync('git', ['init', '-q', root]);
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'owned' } }));
    execFileSync('git', ['add', '.abapgit-agent.json'], { cwd: root }); process.chdir(root);
    key = { object: 'TEXT', name: 'ZAPP_TEST', id: 'ST', language: 'E' };
    document = { version: 1, key, lines: [{ tdformat: '*', tdline: '  Title' }, { tdformat: '', tdline: 'A  B' }] };
    http = { fetchCsrfToken: jest.fn().mockResolvedValue('mock-token'), post: jest.fn(async (_, body) => response(body)) };
    context = { loadConfig: jest.fn(() => ({ host: 'mock.invalid', client: '123' })), AbapHttp: jest.fn(() => http) };
    log = jest.spyOn(console, 'log').mockImplementation(() => {}); jest.spyOn(console, 'error').mockImplementation(() => {});
    process.exitCode = 0;
  });
  afterEach(() => { process.chdir(originalCwd); fs.rmSync(root, { recursive: true, force: true }); jest.restoreAllMocks(); process.exitCode = 0; });

  test('default UPDATE uses the owned full-key payload without rewriting/staging or hidden import', async () => {
    prepare(); const original = fs.readFileSync(file());
    const result = await execute();
    expect(result).toMatchObject({ success: true, git_managed: true, file_written: false, baseline_recorded: true });
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['UPDATE']);
    expect(http.post.mock.calls[0][1]).toEqual({ action: 'UPDATE', ...key, repo_id: 'owned', lines: document.lines });
    expect(fs.readFileSync(file())).toEqual(original);
    expect(execFileSync('git', ['ls-files', '--', 'texts/so10'], { encoding: 'utf8' })).toBe('');
  });
  test('ISO CREATE resolves the SAP language through readonly NOT_FOUND before matching the file', async () => {
    prepare(); http.post.mockImplementation(async (_, body) => body.action === 'READ' ? {
      success: false, error_kind: 'NOT_FOUND', ...key, data_client: '123'
    } : response(body));
    await command.execute(['create', key.name, '--language', 'EN', '--json'], context);
    expect(JSON.parse(log.mock.calls.at(-1)[0]).success).toBe(true);
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['READ', 'CREATE']);
    expect(http.post.mock.calls[1][1]).toMatchObject({ ...key, repo_id: 'owned', lines: document.lines });
  });
  test('absent default payload instructs explicit IMPORT without saving or creating files', async () => {
    const result = await execute();
    expect(result).toMatchObject({ success: false, saved: false, file_written: false });
    expect(result.error).toMatch(/explicit IMPORT/); expect(http.post).not.toHaveBeenCalled();
    expect(fs.existsSync(path.join(root, 'texts'))).toBe(false);
  });
  test('invalid external payload reports known-false write/baseline/file outcomes before connection', async () => {
    const external = path.join(root, 'invalid.json'); fs.writeFileSync(external, '[]');
    expect(await execute('--itf-file', external)).toMatchObject({ success: false, saved: false, baseline_recorded: false,
      file_written: false, git_managed: true, stage: 'VALIDATION' });
    expect(context.AbapHttp).not.toHaveBeenCalled(); expect(fs.existsSync(file())).toBe(false);
  });
  test('duplicate full keys fail before connection', async () => {
    prepare(); fs.writeFileSync(path.join(path.dirname(file()), 'duplicate.itf.json'), JSON.stringify(document));
    expect((await execute()).error).toMatch(/Duplicate/); expect(context.AbapHttp).not.toHaveBeenCalled();
  });
  test('plain external override prepares clean tracked desired content before SAP and preserves its original', async () => {
    prepare(); commit(); const external = path.join(root, 'external.txt'); fs.writeFileSync(external, 'new\r\n');
    http.post.mockImplementation(async (_, body) => { expect(JSON.parse(fs.readFileSync(file())).lines).toEqual(body.lines); return response(body); });
    const result = await execute('--file', external);
    expect(result).toMatchObject({ success: true, file_written: true, git_managed: true });
    expect(fs.readFileSync(external, 'utf8')).toBe('new\r\n');
    expect(execFileSync('git', ['diff', '--cached', '--name-only'], { encoding: 'utf8' })).toBe('');
  });
  test('differing dirty desired work is never overwritten or sent', async () => {
    prepare(); commit(); fs.writeFileSync(file(), JSON.stringify({ ...document, lines: [{ tdformat: '*', tdline: 'local edit' }] }));
    const before = fs.readFileSync(file()), external = path.join(root, 'external.txt'); fs.writeFileSync(external, 'different');
    const result = await execute('--file', external);
    expect(result.success).toBe(false); expect(fs.readFileSync(file())).toEqual(before); expect(http.post).not.toHaveBeenCalled();
  });
  test.each(['--assume-unchanged', '--skip-worktree'])('external override preserves hidden work with %s before any save', async (flag) => {
    prepare(); commit();
    execFileSync('git', ['update-index', flag, path.relative(root, file())], { cwd: root });
    fs.writeFileSync(file(), JSON.stringify({ ...document, lines: [{ tdformat: '*', tdline: 'hidden local edit' }] }));
    const before = fs.readFileSync(file()), external = path.join(root, 'external.txt'); fs.writeFileSync(external, 'different');
    const result = await execute('--file', external);
    expect(result).toMatchObject({ success: false, saved: false, baseline_recorded: false, file_written: false });
    expect(result.error).toMatch(/uncommitted/);
    expect(fs.readFileSync(file())).toEqual(before);
    expect(fs.readFileSync(external, 'utf8')).toBe('different');
    expect(http.post).not.toHaveBeenCalled();
  });
  test('same-content dirty external override is a no-op and may still attempt conflict-safe UPDATE', async () => {
    prepare(); const external = path.join(root, 'external.json'); fs.writeFileSync(external, JSON.stringify(document));
    const before = fs.readFileSync(file()); expect((await execute('--itf-file', external)).file_written).toBe(false);
    expect(fs.readFileSync(file())).toEqual(before); expect(http.post.mock.calls[0][1].repo_id).toBe('owned');
  });
  test('wrapper key mismatch fails before preparation or save', async () => {
    const external = path.join(root, 'external.json'); fs.writeFileSync(external, JSON.stringify({ ...document, key: { ...key, name: 'OTHER' } }));
    const result = await execute('--itf-file', external);
    expect(result.error).toMatch(/does not match/); expect(http.post).not.toHaveBeenCalled(); expect(fs.existsSync(file())).toBe(false);
  });
  test('standalone external input requires explicit ignore and never creates managed files', async () => {
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), '{}');
    const external = path.join(root, 'external.json'); fs.writeFileSync(external, JSON.stringify(document.lines));
    expect((await execute('--itf-file', external)).error).toMatch(/explicit --conflict-mode ignore/);
    expect(http.post).not.toHaveBeenCalled();
    expect(await execute('--itf-file', external, '--conflict-mode', 'ignore')).toMatchObject({ success: true, git_managed: false, baseline_recorded: false });
    expect(http.post.mock.calls[0][1]).toMatchObject({ conflict_mode: 'ignore', lines: document.lines });
    expect(http.post.mock.calls[0][1]).not.toHaveProperty('repo_id'); expect(fs.existsSync(file())).toBe(false);
  });
  test('managed ignore retains ownership context rather than downgrading to standalone', async () => {
    prepare(); await execute('--conflict-mode', 'ignore');
    expect(http.post.mock.calls[0][1]).toMatchObject({ repo_id: 'owned', conflict_mode: 'ignore' });
  });
  test('post-save metadata failure preserves saved=true and prepared working content without retry', async () => {
    const external = path.join(root, 'external.txt'); fs.writeFileSync(external, 'new');
    http.post.mockResolvedValue({ success: false, saved: true, baseline_recorded: false, stage: 'METADATA_RECORD', error: 'record failed' });
    expect(await execute('--file', external)).toMatchObject({ success: false, saved: true, baseline_recorded: false, file_written: true, stage: 'METADATA_RECORD' });
    expect(JSON.parse(fs.readFileSync(file())).lines[0].tdline).toBe('new'); expect(http.post).toHaveBeenCalledTimes(1);
  });
  test('lost save response reports unknown save/baseline and retains the new desired file', async () => {
    const external = path.join(root, 'external.txt'); fs.writeFileSync(external, 'new'); http.post.mockRejectedValue(new Error('secret credentials'));
    const result = await execute('--file', external);
    expect(result).toMatchObject({ success: false, saved: null, baseline_recorded: null, file_written: true });
    expect(result.error).not.toMatch(/secret/); expect(fs.existsSync(file())).toBe(true); expect(http.post).toHaveBeenCalledTimes(1);
  });
  test.each(['owner', 'client', 'hash'])('invalid success acknowledgement %s is unknown, not verified success', async (kind) => {
    prepare(); http.post.mockImplementation(async (_, body) => ({ ...response(body), ...(kind === 'owner' ? { base_repo_id: 'other' } :
      kind === 'client' ? { data_client: '124' } : { source_sha: 'short', base_sha: 'short' }) }));
    expect(await execute()).toMatchObject({ success: false, saved: null, baseline_recorded: null });
    expect(http.post).toHaveBeenCalledTimes(1);
  });
  test('untrusted ISO normalization never causes file preparation or save', async () => {
    http.post.mockResolvedValue({ success: false, error_kind: 'NOT_FOUND', ...key, data_client: '124' });
    await command.execute(['create', key.name, '--language', 'EN', '--json'], context);
    expect(JSON.parse(log.mock.calls.at(-1)[0])).toMatchObject({ success: false, saved: false, file_written: false });
    expect(http.post).toHaveBeenCalledTimes(1); expect(fs.existsSync(file())).toBe(false);
  });
});
