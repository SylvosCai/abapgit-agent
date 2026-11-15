const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');
const command = require('../../src/commands/so10');
const { filename } = require('../../src/utils/so10-files');

describe('so10 explicit import', () => {
  let root, originalCwd, http, context, log, key, sha, sourceLines;
  const execute = async (...extra) => {
    await command.execute(['import', '--prefix', 'ZAPP_', '--json', ...extra], context);
    return JSON.parse(log.mock.calls.at(-1)[0]);
  };
  const file = () => path.join(root, 'texts/so10', filename(key));
  const commitFixture = () => {
    execFileSync('git', ['add', '.'], { cwd: root });
    execFileSync('git', ['-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid', '-c', 'commit.gpgsign=false', '-c', 'core.hooksPath=/dev/null', 'commit', '-qm', 'owned fixture'], { cwd: root });
  };
  beforeEach(() => {
    originalCwd = process.cwd();
    root = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-import-owned-'));
    execFileSync('git', ['init', '-q', root]);
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'owned' } }));
    execFileSync('git', ['add', '.abapgit-agent.json'], { cwd: root });
    process.chdir(root);
    key = { object: 'TEXT', name: 'ZAPP_TEST', id: 'ST', language: 'E' };
    sha = 'a'.repeat(64);
    sourceLines = [{ tdformat: '*', tdline: 'hello' }];
    http = { fetchCsrfToken: jest.fn().mockResolvedValue('mock-token'), post: jest.fn(async (_, body) => {
      if (body.action === 'LIST') return { success: true, keys: [key], has_more: false, data_client: '123' };
      if (body.action === 'READ') return { success: true, ...key, lines: sourceLines, data_client: '123', source_sha: sha, base_found: false };
      expect(body.action).toBe('ADOPT');
      expect(fs.existsSync(file())).toBe(true);
      return { success: true, baseline_recorded: true, data_client: '123', base_repo_id: 'owned', base_sha: body.source_sha };
    }) };
    context = { loadConfig: jest.fn(() => ({ host: 'mock.invalid', client: '123', user: 'mock' })), AbapHttp: jest.fn(() => http) };
    log = jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
    process.exitCode = 0;
  });
  afterEach(() => {
    process.chdir(originalCwd);
    fs.rmSync(root, { recursive: true, force: true });
    jest.restoreAllMocks();
    process.exitCode = 0;
  });
  test('writes self-describing ITF then registers exact observed source without Git staging', async () => {
    const result = await execute();
    expect(result).toMatchObject({ success: true, imported: 1, saved: false, file_written: true });
    expect(JSON.parse(fs.readFileSync(file(), 'utf8'))).toEqual({ version: 1, key, lines: sourceLines });
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['LIST', 'READ', 'ADOPT']);
    expect(http.post.mock.calls[2][1]).toMatchObject({ ...key, repo_id: 'owned', source_sha: sha, client: '123' });
    expect(context.AbapHttp).toHaveBeenCalledTimes(1);
    expect(execFileSync('git', ['ls-files', '--', 'texts/so10'], { cwd: root, encoding: 'utf8' })).toBe('');
  });
  test('missing prefix fails before HTTP', async () => {
    await command.execute(['import', '--json'], context);
    expect(http.post).not.toHaveBeenCalled();
    expect(JSON.parse(log.mock.calls.at(-1)[0]).success).toBe(false);
  });
  test('dry-run lists intended files without reading bodies or creating folders', async () => {
    const result = await execute('--dry-run');
    expect(result).toMatchObject({ success: true, selected: 1, imported: 0, file_written: false });
    expect(http.post).toHaveBeenCalledTimes(1);
    expect(fs.existsSync(path.join(root, 'texts'))).toBe(false);
  });
  test('overflow fails before bodies/files', async () => {
    http.post.mockResolvedValue({ success: true, keys: [key], has_more: true, data_client: '123' });
    const result = await execute();
    expect(result.success).toBe(false);
    expect(http.post).toHaveBeenCalledTimes(1);
    expect(fs.existsSync(file())).toBe(false);
  });
  test('duplicate full keys fail before bodies/files', async () => {
    http.post.mockResolvedValue({ success: true, keys: [key, key], data_client: '123' });
    expect((await execute()).success).toBe(false);
    expect(http.post).toHaveBeenCalledTimes(1);
  });
  test('mismatched source identity fails before local preparation', async () => {
    http.post.mockResolvedValueOnce({ success: true, keys: [key], data_client: '123' })
      .mockResolvedValueOnce({ success: true, ...key, name: 'WRONG', lines: sourceLines, source_sha: sha, data_client: '123' });
    expect((await execute()).success).toBe(false);
    expect(http.post).toHaveBeenCalledTimes(2);
    expect(fs.existsSync(file())).toBe(false);
  });
  test('foreign baseline ownership fails before file or metadata write', async () => {
    http.post.mockResolvedValueOnce({ success: true, keys: [key], data_client: '123' })
      .mockResolvedValueOnce({ success: true, ...key, lines: sourceLines, source_sha: sha, data_client: '123', base_found: true, base_repo_id: 'other' });
    expect((await execute()).success).toBe(false);
    expect(http.post).toHaveBeenCalledTimes(2);
    expect(fs.existsSync(file())).toBe(false);
  });
  test('stale post-file adoption reports file written but baseline false; no auto retry', async () => {
    http.post.mockResolvedValueOnce({ success: true, keys: [key], data_client: '123' })
      .mockResolvedValueOnce({ success: true, ...key, lines: sourceLines, source_sha: sha, data_client: '123' })
      .mockResolvedValueOnce({ success: false, baseline_recorded: false, error: 'Source changed' });
    const result = await execute();
    expect(result).toMatchObject({ success: false, saved: false, file_written: true });
    expect(result.files[0].baseline_recorded).toBe(false);
    expect(fs.existsSync(file())).toBe(true);
    expect(http.post).toHaveBeenCalledTimes(3);
  });
  test('lost adoption response is unknown metadata, never unknown text save or leaked transport error', async () => {
    http.post.mockResolvedValueOnce({ success: true, keys: [key], data_client: '123' })
      .mockResolvedValueOnce({ success: true, ...key, lines: sourceLines, source_sha: sha, data_client: '123' })
      .mockRejectedValueOnce(new Error('secret mock credential'));
    const result = await execute();
    expect(result).toMatchObject({ success: false, saved: false, file_written: true });
    expect(result.files[0].baseline_recorded).toBeNull();
    expect(JSON.stringify(result)).not.toContain('secret mock credential');
  });
  test('wrong adoption acknowledgment cannot report success', async () => {
    http.post.mockResolvedValueOnce({ success: true, keys: [key], data_client: '123' })
      .mockResolvedValueOnce({ success: true, ...key, lines: sourceLines, source_sha: sha, data_client: '123' })
      .mockResolvedValueOnce({ success: true, baseline_recorded: true, data_client: '999', base_repo_id: 'owned', base_sha: sha });
    const result = await execute();
    expect(result.success).toBe(false);
    expect(result.files[0].baseline_recorded).toBeNull();
  });
  test('explicit system adoption never overwrites differing dirty tracked work', async () => {
    await execute();
    commitFixture();
    const local = { version: 1, key, lines: [{ tdformat: '*', tdline: 'local work' }] };
    fs.writeFileSync(file(), JSON.stringify(local));
    http.post.mockClear();
    expect((await execute('--adopt-system')).success).toBe(false);
    expect(JSON.parse(fs.readFileSync(file(), 'utf8'))).toEqual(local);
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['LIST', 'READ']);
  });
  test.each(['--assume-unchanged', '--skip-worktree'])('system adoption preserves hidden work with %s before baseline registration', async (flag) => {
    await execute(); commitFixture();
    execFileSync('git', ['update-index', flag, path.relative(root, file())], { cwd: root });
    const local = { version: 1, key, lines: [{ tdformat: '*', tdline: 'hidden local work' }] };
    fs.writeFileSync(file(), JSON.stringify(local));
    const before = fs.readFileSync(file());
    sourceLines = [{ tdformat: '*', tdline: 'SAP correction' }]; sha = 'b'.repeat(64);
    http.post.mockClear();
    const result = await execute('--adopt-system');
    expect(result).toMatchObject({ success: false, saved: false, file_written: false, imported: 0 });
    expect(result.files[0].baseline_recorded).toBe(false);
    expect(result.error).toMatch(/uncommitted/);
    expect(fs.readFileSync(file())).toEqual(before);
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['LIST', 'READ']);
  });
  test('explicit clean adoption changes file without creating a Git commit', async () => {
    await execute();
    commitFixture();
    const before = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' });
    sourceLines = [{ tdformat: '*', tdline: 'SAP correction' }];
    sha = 'b'.repeat(64);
    expect((await execute('--adopt-system')).success).toBe(true);
    expect(JSON.parse(fs.readFileSync(file(), 'utf8')).lines).toEqual(sourceLines);
    expect(execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' })).toBe(before);
  });
  test('local edit after preparation is preserved and prevents baseline registration', async () => {
    const link = fs.linkSync;
    jest.spyOn(fs, 'linkSync').mockImplementation((from, to) => {
      link(from, to);
      fs.writeFileSync(to, JSON.stringify({ version: 1, key, lines: [{ tdformat: '*', tdline: 'concurrent local edit' }] }));
    });
    const result = await execute();
    expect(result).toMatchObject({ success: false, file_written: true });
    expect(result.files[0].baseline_recorded).toBe(false);
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['LIST', 'READ']);
    expect(JSON.parse(fs.readFileSync(file(), 'utf8')).lines[0].tdline).toBe('concurrent local edit');
  });
  test('partial import reports first verified file and stops on second source failure', async () => {
    const other = { ...key, name: 'ZAPP_OTHER' };
    http.post.mockResolvedValueOnce({ success: true, keys: [key, other], data_client: '123' })
      .mockResolvedValueOnce({ success: true, ...key, lines: sourceLines, source_sha: sha, data_client: '123' })
      .mockResolvedValueOnce({ success: true, baseline_recorded: true, data_client: '123', base_repo_id: 'owned', base_sha: sha })
      .mockResolvedValueOnce({ success: false, error: 'Source missing' });
    const result = await execute();
    expect(result).toMatchObject({ success: false, selected: 2, imported: 1, file_written: true, saved: false });
    expect(result.files.map((item) => item.baseline_recorded)).toEqual([true, false]);
    expect(fs.existsSync(file())).toBe(true);
    expect(http.post).toHaveBeenCalledTimes(4);
  });
  test('other-client import uses data parameters, not a cloned HTTP logon', async () => {
    http.post.mockResolvedValueOnce({ success: true, keys: [key], data_client: '000' })
      .mockResolvedValueOnce({ success: true, ...key, lines: sourceLines, source_sha: sha, data_client: '000' })
      .mockResolvedValueOnce({ success: true, baseline_recorded: true, data_client: '000', base_repo_id: 'owned', base_sha: sha });
    expect((await execute('--source-client', '000')).success).toBe(true);
    expect(context.AbapHttp).toHaveBeenCalledWith(expect.objectContaining({ client: '123', user: 'mock' }));
    expect(http.post.mock.calls.map(([, body]) => body.client)).toEqual(['000', '000', '000']);
  });
  test('same-content dirty payload stays byte-identical while baseline is verified', async () => {
    await execute();
    const bytes = JSON.stringify({ lines: sourceLines, key, version: 1 });
    fs.writeFileSync(file(), bytes);
    expect((await execute()).files[0].file_written).toBe(false);
    expect(fs.readFileSync(file(), 'utf8')).toBe(bytes);
  });
});
