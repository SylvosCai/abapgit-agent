const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');
const files = require('../../src/utils/so10-files');

const key = { object: 'TEXT', name: 'ZAPP_FOOTER', id: 'ST', language: 'E' };
const payload = () => ({ version: 1, key: { ...key }, lines: [{ tdformat: '*', tdline: '  Title' }, { tdformat: '', tdline: 'A  B' }] });

describe('isolated SO10 repository payloads', () => {
  let root;
  beforeEach(() => {
    root = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-files-test-'));
    execFileSync('git', ['init', '-q', root], { stdio: 'ignore' });
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'app-test' } }));
    execFileSync('git', ['add', '.abapgit-agent.json'], { cwd: root, stdio: 'ignore' });
  });
  afterEach(() => fs.rmSync(root, { recursive: true, force: true }));
  const writePayload = (repository, data, name = files.filename(data.key)) => {
    fs.mkdirSync(repository.folder, { recursive: true });
    fs.writeFileSync(path.join(repository.folder, name), JSON.stringify(data));
  };

  test('resolves tracked identity/default folder from a subdirectory without remote/network access', () => {
    const sub = path.join(root, 'sub'); fs.mkdirSync(sub);
    const repository = files.getRepository(sub);
    expect(repository).toEqual({ root: fs.realpathSync(root), repoId: 'app-test', folder: path.join(fs.realpathSync(root), 'texts/so10') });
    expect(files.readManagedFiles(repository).size).toBe(0);
  });
  test('rejects untracked project configuration instead of inventing stable ownership', () => {
    execFileSync('git', ['rm', '--cached', '.abapgit-agent.json'], { cwd: root, stdio: 'ignore' });
    expect(() => files.getRepository(root)).toThrow(/Track .abapgit-agent.json/);
  });
  test('optional discovery returns no repository only when SO10 is not enabled', () => {
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({}));
    expect(files.getRepository(root, { optional: true })).toBeNull();
    fs.unlinkSync(path.join(root, '.abapgit-agent.json'));
    expect(files.getRepository(root, { optional: true })).toBeNull();
    const isolated = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-optional-outside-'));
    try { expect(files.getRepository(isolated, { optional: true })).toBeNull(); }
    finally { fs.rmSync(isolated, { recursive: true, force: true }); }
  });
  test('optional discovery never downgrades invalid or untracked SO10 configuration', () => {
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: null }));
    expect(() => files.getRepository(root, { optional: true })).toThrow(/so10.repoId/);
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'app-test' } }));
    execFileSync('git', ['rm', '--cached', '.abapgit-agent.json'], { cwd: root, stdio: 'ignore' });
    expect(() => files.getRepository(root, { optional: true })).toThrow(/Track .abapgit-agent.json/);
  });
  test('optional discovery never hides malformed UTF-8 JSON or a symlinked configuration', () => {
    const config = path.join(root, '.abapgit-agent.json');
    fs.writeFileSync(config, Buffer.from([0xff]));
    expect(() => files.getRepository(root, { optional: true })).toThrow(/UTF-8 JSON/);
    fs.unlinkSync(config);
    const target = path.join(root, 'config.json'); fs.writeFileSync(target, '{}'); fs.symlinkSync(target, config);
    expect(() => files.getRepository(root, { optional: true })).toThrow(/symlinks/);
  });
  test('payload roundtrip preserves exact key, order, ITF format and meaningful whitespace', () => {
    const repository = files.getRepository(root), data = payload();
    writePayload(repository, data);
    expect(files.readManaged(repository, key).payload).toEqual(data);
    expect(files.validatePayload(data, key)).toBe(data);
  });
  test('a missing payload instructs explicit import and writes no file', () => {
    const repository = files.getRepository(root);
    expect(() => files.readManaged(repository, key)).toThrow(/explicit IMPORT/);
    expect(fs.existsSync(repository.folder)).toBe(false);
  });
  test('complete-key filenames are bounded and distinct for case, language and ID', () => {
    const names = [key, { ...key, name: key.name.toLowerCase() }, { ...key, language: 'D' }, { ...key, id: 'SYST' }].map(files.filename);
    expect(new Set(names).size).toBe(4);
    for (const name of names) expect(name).toMatch(/\.[a-f0-9]{64}\.itf\.json$/);
    const unsafeLooking = files.filename({ ...key, name: '../é /'.repeat(10) });
    expect(unsafeLooking).not.toMatch(/[\\/]/);
    expect(Buffer.byteLength(unsafeLooking)).toBeLessThan(255);
  });
  test.each([
    { version: 2, key, lines: payload().lines },
    { ...payload(), extra: true },
    { ...payload(), key: { ...key, language: 'EN' } },
    { ...payload(), key: { ...key, language: 'E\n' } },
    { ...payload(), key: { ...key, id: 'ST\n' } },
    { ...payload(), key: { ...key, name: 'X'.repeat(71) } },
    { ...payload(), key: { ...key, name: 'ZTEXT ' } },
    { ...payload(), key: { ...key, name: 'Z\tTEXT' } },
    { ...payload(), key: { ...key, name: 'Z\x7fTEXT' } },
    { ...payload(), key: { ...key, extra: true } },
    { ...payload(), lines: [] },
    { ...payload(), lines: [{ tdformat: '*', tdline: ' '.repeat(10) }] },
    { ...payload(), lines: [{ tdformat: '*', tdline: 'X'.repeat(133) }] },
    { ...payload(), lines: [{ tdformat: '*', tdline: 'a\nb' }] },
    { ...payload(), lines: [{ tdformat: '*', tdline: 'x', extra: true }] }
  ])('rejects malformed/versioned/unsafe payload: %j', (data) => {
    expect(() => files.validatePayload(data)).toThrow();
  });
  test('rejects a wrapper belonging to a different requested key', () => {
    expect(() => files.validatePayload(payload(), { ...key, name: 'OTHER' })).toThrow(/does not match/);
  });
  test('rejects duplicate identities rather than choosing a last file', () => {
    const repository = files.getRepository(root);
    writePayload(repository, payload()); writePayload(repository, payload(), 'duplicate.itf.json');
    expect(() => files.readManaged(repository, key)).toThrow(/Duplicate/);
  });
  test('rejects malformed UTF-8 even when it could be decoded with replacement characters', () => {
    const repository = files.getRepository(root); fs.mkdirSync(repository.folder, { recursive: true });
    fs.writeFileSync(path.join(repository.folder, 'bad.itf.json'), Buffer.from([0xff]));
    expect(() => files.readManagedFiles(repository)).toThrow(/UTF-8 JSON/);
  });
  test.each(['App', '', '../app', 'app\n', 'x'.repeat(81)])('rejects unstable or malformed repository identity %s', (repoId) => {
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId } }));
    expect(() => files.getRepository(root)).toThrow(/so10.repoId/);
  });
  test.each(['../outside', '/outside', 'texts/../../outside'])('rejects configured folder escapes %s', (folder) => {
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'app-test', folder } }));
    expect(() => files.getRepository(root)).toThrow(/inside the repository/);
  });
  test('rejects folder symlinks including dangling links', () => {
    fs.symlinkSync(path.join(root, 'missing'), path.join(root, 'linked'));
    fs.writeFileSync(path.join(root, '.abapgit-agent.json'), JSON.stringify({ so10: { repoId: 'app-test', folder: 'linked' } }));
    expect(() => files.getRepository(root)).toThrow(/symlinks/);
  });
  test('rejects payload symlinks before reading their target', () => {
    const repository = files.getRepository(root); fs.mkdirSync(repository.folder, { recursive: true });
    fs.writeFileSync(path.join(root, 'outside.json'), JSON.stringify(payload()));
    fs.symlinkSync(path.join(root, 'outside.json'), path.join(repository.folder, 'link.itf.json'));
    expect(() => files.readManagedFiles(repository)).toThrow(/symlinks/);
  });
  const commitFixture = (repository) => {
    execFileSync('git', ['add', '.'], { cwd: repository.root, stdio: 'ignore' });
    execFileSync('git', ['-c', 'user.name=SO10 Test', '-c', 'user.email=so10@example.invalid', '-c', 'commit.gpgsign=false', '-c', 'core.hooksPath=/dev/null', 'commit', '-qm', 'fixture'], { cwd: repository.root, stdio: 'ignore' });
  };
  test('explicit import writes the managed payload atomically without staging or committing it', () => {
    const repository = files.getRepository(root), data = payload();
    const result = files.writeManaged(repository, data);
    expect(result).toEqual({ file: path.join(repository.folder, files.filename(key)), fileWritten: true });
    expect(files.readManaged(repository, key).payload).toEqual(data);
    expect(execFileSync('git', ['ls-files', '--', 'texts/so10'], { cwd: root, encoding: 'utf8' })).toBe('');
    expect(fs.readdirSync(repository.folder)).toEqual([files.filename(key)]);
  });
  test('identical dirty or untracked payloads are left byte-for-byte unchanged', () => {
    const repository = files.getRepository(root), data = payload();
    const { file } = files.writeManaged(repository, data);
    const original = fs.readFileSync(file);
    expect(files.writeManaged(repository, data).fileWritten).toBe(false);
    expect(fs.readFileSync(file)).toEqual(original);
  });
  test('different existing content requires explicit adoption', () => {
    const repository = files.getRepository(root);
    files.writeManaged(repository, payload());
    commitFixture(repository);
    const other = payload(); other.lines[0].tdline = 'System correction';
    expect(() => files.writeManaged(repository, other)).toThrow(/explicit system adoption/);
    expect(files.readManaged(repository, key).payload).toEqual(payload());
    expect(files.writeManaged(repository, other, { replaceClean: true }).fileWritten).toBe(true);
    expect(files.readManaged(repository, key).payload).toEqual(other);
  });
  test('adoption never replaces different uncommitted local work', () => {
    const repository = files.getRepository(root), data = payload();
    const { file } = files.writeManaged(repository, data); commitFixture(repository);
    const local = payload(); local.lines[0].tdline = 'Local work';
    fs.writeFileSync(file, JSON.stringify(local));
    const original = fs.readFileSync(file), system = payload(); system.lines[0].tdline = 'SAP change';
    expect(() => files.writeManaged(repository, system, { replaceClean: true })).toThrow(/uncommitted/);
    expect(fs.readFileSync(file)).toEqual(original);
  });
  test.each(['--assume-unchanged', '--skip-worktree'])('adoption preserves hidden local work with %s', (flag) => {
    const repository = files.getRepository(root);
    const { file } = files.writeManaged(repository, payload()); commitFixture(repository);
    execFileSync('git', ['update-index', flag, path.relative(repository.root, file)], { cwd: repository.root });
    const local = payload(); local.lines[0].tdline = 'Hidden local work';
    fs.writeFileSync(file, JSON.stringify(local));
    const original = fs.readFileSync(file), system = payload(); system.lines[0].tdline = 'SAP correction';
    expect(execFileSync('git', ['status', '--porcelain'], { cwd: repository.root, encoding: 'utf8' })).toBe('');
    expect(() => files.writeManaged(repository, system, { replaceClean: true })).toThrow(/uncommitted/);
    expect(fs.readFileSync(file)).toEqual(original);
    expect(fs.readdirSync(repository.folder)).toEqual([files.filename(key)]);
    expect(files.writeManaged(repository, local, { replaceClean: true }).fileWritten).toBe(false);
  });
  test.each(['--assume-unchanged', '--skip-worktree'])('clean tracked content can still be adopted with %s', (flag) => {
    const repository = files.getRepository(root);
    const { file } = files.writeManaged(repository, payload()); commitFixture(repository);
    const relative = path.relative(repository.root, file);
    execFileSync('git', ['update-index', flag, relative], { cwd: repository.root });
    const indexBefore = execFileSync('git', ['ls-files', '--stage', '--', relative], { cwd: repository.root });
    const system = payload(); system.lines[0].tdline = 'SAP correction';
    expect(files.writeManaged(repository, system, { replaceClean: true }).fileWritten).toBe(true);
    expect(files.readManaged(repository, key).payload).toEqual(system);
    expect(execFileSync('git', ['ls-files', '--stage', '--', relative], { cwd: repository.root })).toEqual(indexBefore);
  });
  test('clean CRLF checkout content is compared with filtered index bytes', () => {
    const repository = files.getRepository(root);
    const { file } = files.writeManaged(repository, payload());
    fs.writeFileSync(path.join(root, '.gitattributes'), '*.itf.json text eol=crlf\n');
    commitFixture(repository);
    fs.unlinkSync(file);
    execFileSync('git', ['checkout', '--', path.relative(repository.root, file)], { cwd: repository.root });
    expect(fs.readFileSync(file).includes(Buffer.from('\r\n'))).toBe(true);
    const system = payload(); system.lines[0].tdline = 'SAP correction';
    expect(files.writeManaged(repository, system, { replaceClean: true }).fileWritten).toBe(true);
    expect(files.readManaged(repository, key).payload).toEqual(system);
  });
  test('clean tracked payloads larger than the default Git output buffer can still be adopted', () => {
    const repository = files.getRepository(root), data = payload();
    data.lines = Array.from({ length: 9000 }, () => ({ tdformat: '*', tdline: 'X'.repeat(132) }));
    const { file } = files.writeManaged(repository, data); commitFixture(repository);
    expect(fs.statSync(file).size).toBeGreaterThan(1024 * 1024);
    data.lines[0].tdline = 'SAP correction';
    expect(files.writeManaged(repository, data, { replaceClean: true }).fileWritten).toBe(true);
    expect(files.readManaged(repository, key).payload).toEqual(data);
  });
  test('ignored untracked content is not mistaken for clean tracked work', () => {
    const repository = files.getRepository(root), data = payload();
    const { file } = files.writeManaged(repository, data);
    fs.appendFileSync(path.join(root, '.git/info/exclude'), '\ntexts/so10/*\n');
    const other = payload(); other.lines[0].tdline = 'System change';
    expect(() => files.writeManaged(repository, other, { replaceClean: true })).toThrow(/untracked SO10 work/);
    expect(fs.readFileSync(file)).toEqual(Buffer.from(`${JSON.stringify(data, null, 2)}\n`));
  });
  test('failed replacement preserves the old file and removes owned temporary files', () => {
    const repository = files.getRepository(root);
    const { file } = files.writeManaged(repository, payload()); commitFixture(repository);
    const original = fs.readFileSync(file), other = payload(); other.lines[0].tdline = 'New content';
    const rename = jest.spyOn(fs, 'renameSync').mockImplementation(() => { throw new Error('rename denied'); });
    try { expect(() => files.writeManaged(repository, other, { replaceClean: true })).toThrow(/rename denied/); }
    finally { rename.mockRestore(); }
    expect(fs.readFileSync(file)).toEqual(original);
    expect(fs.readdirSync(repository.folder)).toEqual([files.filename(key)]);
  });
  test('new-file creation cannot overwrite a payload created concurrently', () => {
    const repository = files.getRepository(root), data = payload(), concurrent = payload();
    concurrent.lines[0].tdline = 'Concurrent work';
    const realLink = fs.linkSync;
    const link = jest.spyOn(fs, 'linkSync').mockImplementation((temporary, file) => {
      fs.writeFileSync(file, JSON.stringify(concurrent));
      return realLink(temporary, file);
    });
    try { expect(() => files.writeManaged(repository, data)).toThrow(); }
    finally { link.mockRestore(); }
    expect(files.readManaged(repository, key).payload).toEqual(concurrent);
  });
  test('cleanup failure reports that the local file was already written', () => {
    const repository = files.getRepository(root), data = payload();
    const unlink = jest.spyOn(fs, 'unlinkSync').mockImplementation(() => { throw new Error('cleanup failed'); });
    try {
      expect.assertions(3);
      try { files.writeManaged(repository, data); }
      catch (error) { expect(error.fileWritten).toBe(true); expect(error.message).toBe('cleanup failed'); }
      expect(files.readManaged(repository, key).payload).toEqual(data);
    } finally { unlink.mockRestore(); }
  });
  test('rejects non-Git directories without creating a repository', () => {
    // Prevent Git discovery from inheriting the enclosing test repository.
    const isolated = fs.mkdtempSync(path.join(os.tmpdir(), 'so10-no-git-test-'));
    try { expect(() => files.getRepository(isolated)).toThrow(/require a Git repository/); }
    finally { fs.rmSync(isolated, { recursive: true, force: true }); }
  });
});
