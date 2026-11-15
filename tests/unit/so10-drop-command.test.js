const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');
const command = require('../../src/commands/so10');
const { filename, getRepository, readCommitted } = require('../../src/utils/so10-files');

describe('SO10 Git-protected DROP', () => {
  let root, cwd, key, document, file, http, context, log;
  const git = (...args) => execFileSync('git', args, { cwd: root, encoding: 'utf8' });
  const commit = () => { git('add', '.'); git('-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
    '-c', 'commit.gpgsign=false', '-c', 'core.hooksPath=/dev/null', 'commit', '-qm', 'owned fixture'); };
  const execute = async (...args) => {
    await command.execute(['drop', key.name, '--language', 'E', '--json', ...args], context);
    return JSON.parse(log.mock.calls.at(-1)[0]);
  };
  beforeEach(() => {
    cwd = process.cwd(); root = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-drop-owned-'));
    git('init', '-q'); process.chdir(root);
    key = { object: 'TEXT', name: 'ZAPP_TEST', id: 'ST', language: 'E' };
    document = { version: 1, key, lines: [{ tdformat: '*', tdline: '  recoverable text' }] };
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'owned' } }));
    file = path.join(root, 'texts/so10', filename(key)); fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, JSON.stringify(document)); commit();
    http = { fetchCsrfToken: jest.fn().mockResolvedValue('mock'), post: jest.fn(async (_, body) => ({
      success: true, ...key, data_client: body.target_client, saved: false, deleted: true, baseline_removed: true
    })) };
    context = { loadConfig: jest.fn(() => ({ host: 'mock.invalid', client: '123', customizingClient: '124' })),
      AbapHttp: jest.fn(() => http) };
    log = jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {}); process.exitCode = 0;
  });
  afterEach(() => { process.chdir(cwd); fs.rmSync(root, { recursive: true, force: true }); jest.restoreAllMocks(); process.exitCode = 0; });

  test('committed exact payload is sent once, configured target/HTTP retained, Git untouched', async () => {
    const bytes = fs.readFileSync(file), head = git('rev-parse', 'HEAD').trim();
    expect(await execute()).toMatchObject({ success: true, saved: false, deleted: true, baseline_removed: true, git_commit: head });
    expect(http.post.mock.calls.map(([, body]) => body)).toEqual([{ action: 'DROP', ...key, repo_id: 'owned',
      target_client: '123', lines: document.lines }]);
    expect(context.AbapHttp).toHaveBeenCalledWith(expect.objectContaining({ client: '123' }));
    expect(fs.readFileSync(file)).toEqual(bytes); expect(git('status', '--porcelain')).toBe('');
  });
  test('explicit target is data only and does not clone credentials', async () => {
    expect((await execute('--target-client', '124')).success).toBe(true);
    expect(http.post.mock.calls[0][1].target_client).toBe('124');
    expect(context.AbapHttp.mock.calls[0][0].client).toBe('123');
  });
  test.each(['untracked', 'staged', 'dirty', 'staged-edit', 'config-edit', 'missing', 'duplicate'])('%s work cannot be dropped', async (kind) => {
    if (kind === 'untracked' || kind === 'staged') {
      git('rm', path.relative(root, file)); commit();
      // Git removes the now-empty directory along with its final tracked file.
      fs.mkdirSync(path.dirname(file), { recursive: true });
      fs.writeFileSync(file, JSON.stringify(document));
      if (kind === 'staged') git('add', path.relative(root, file));
    } else if (kind === 'dirty' || kind === 'staged-edit') {
      fs.writeFileSync(file, JSON.stringify({ ...document, lines: [{ tdformat: '*', tdline: 'uncommitted' }] }));
      if (kind === 'staged-edit') git('add', path.relative(root, file));
    } else if (kind === 'config-edit') fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'other' } }));
    else if (kind === 'missing') fs.unlinkSync(file);
    else fs.writeFileSync(path.join(path.dirname(file), 'duplicate.itf.json'), JSON.stringify(document));
    expect(await execute()).toMatchObject({ success: false, saved: false, deleted: false, baseline_removed: false });
    expect(http.post).not.toHaveBeenCalled();
  });
  test('pinned HEAD/local recheck rejects changes, including assume-unchanged files', () => {
    const repository = getRepository(), entry = readCommitted(repository, key);
    git('update-index', '--assume-unchanged', path.relative(root, file));
    fs.writeFileSync(file, JSON.stringify({ ...document, lines: [{ tdformat: '*', tdline: 'hidden change' }] }));
    expect(() => readCommitted(repository, key, entry.head)).toThrow(/clean committed/);
    fs.writeFileSync(file, JSON.stringify(document)); git('update-index', '--no-assume-unchanged', path.relative(root, file));
    fs.writeFileSync(path.join(root, 'unrelated'), 'change'); commit();
    expect(() => readCommitted(repository, key, entry.head)).toThrow(/unchanged HEAD/);
  });
  test('a previously loaded repository identity cannot outlive a committed config change', () => {
    const repository = getRepository();
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'other' } }));
    commit();
    expect(() => readCommitted(repository, key)).toThrow(/clean committed/);
  });
  test.each([['--conflict-mode', 'ignore'], ['--file', 'anything'], ['--overwrite'], ['--prefix', 'Z'], ['--source-client', '124'], ['--target-client', '124\n']])('rejects bypass/inapplicable option %j', async (...args) => {
    expect(await execute(...args)).toMatchObject({ success: false, deleted: false, baseline_removed: false });
    expect(context.AbapHttp).not.toHaveBeenCalled();
  });
  test('delete alias and standalone operation do not exist', async () => {
    await command.execute(['delete', key.name, '--language', 'E', '--json'], context);
    expect(JSON.parse(log.mock.calls.at(-1)[0]).success).toBe(false);
    fs.unlinkSync(path.join(root, '.abapgit-agent.json'));
    expect((await execute()).success).toBe(false); expect(http.post).not.toHaveBeenCalled();
  });
  test('post-delete metadata failure is preserved without retries', async () => {
    http.post.mockResolvedValue({ success: false, saved: false, deleted: true, baseline_removed: false, stage: 'METADATA_REMOVE', error: 'metadata failed' });
    expect(await execute()).toMatchObject({ success: false, deleted: true, baseline_removed: false });
    expect(http.post).toHaveBeenCalledTimes(1); expect(fs.existsSync(file)).toBe(true);
  });
  test.each(['lost', 'bad-client', 'bad-key', 'bad-saved', 'missing-flags'])('%s acknowledgement is unknown, no automatic retry', async (kind) => {
    if (kind === 'lost') http.post.mockRejectedValue(new Error('secret'));
    else http.post.mockResolvedValue({ success: true, ...key, saved: kind === 'bad-saved', data_client: kind === 'bad-client' ? '124' : '123',
      name: kind === 'bad-key' ? 'OTHER' : key.name, ...(kind === 'missing-flags' ? {} : { deleted: true, baseline_removed: true }) });
    const result = await execute();
    expect(result).toMatchObject({ success: false, saved: false, deleted: null, baseline_removed: null });
    expect(result.error).not.toMatch(/secret/); expect(http.post).toHaveBeenCalledTimes(1);
  });
  test('ISO canonicalization is read-only before committed key selection', async () => {
    http.post.mockImplementation(async (_, body) => body.action === 'READ' ? { success: false, error_kind: 'NOT_FOUND', ...key, data_client: '124' } :
      { success: true, ...key, data_client: '124', saved: false, deleted: true, baseline_removed: true });
    await command.execute(['drop', key.name, '--language', 'EN', '--target-client', '124', '--json'], context);
    expect(JSON.parse(log.mock.calls.at(-1)[0]).success).toBe(true);
    expect(http.post.mock.calls.map(([, body]) => body.action)).toEqual(['READ', 'DROP']);
  });
});
