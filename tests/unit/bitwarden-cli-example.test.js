const { spawnSync } = require('node:child_process');
const { performance } = require('node:perf_hooks');

jest.mock('node:child_process', () => ({ spawnSync: jest.fn() }));
jest.mock('node:perf_hooks', () => ({ performance: { now: jest.fn() } }));

const id = '11111111-1111-4111-8111-111111111111';
const item = { id, type: 1, login: { username: 'example-user', password: '  p\nword  ' } };
const reply = data => ({ status: 0, stdout: Buffer.from(JSON.stringify(data)), stderr: Buffer.alloc(0) });

describe('standalone Bitwarden CLI adapter example', () => {
  let argv, session, exitCode, stdout, stderr;
  const run = () => jest.isolateModules(() => require('../../examples/credentials/bitwarden-cli.cjs'));

  beforeEach(() => {
    argv = process.argv;
    session = process.env.BW_SESSION;
    exitCode = process.exitCode;
    process.argv = ['node', 'helper', '/trusted/bw', id];
    process.env.BW_SESSION = 'synthetic-session';
    process.exitCode = undefined;
    stdout = jest.spyOn(process.stdout, 'write').mockReturnValue(true);
    stderr = jest.spyOn(process.stderr, 'write').mockReturnValue(true);
    performance.now.mockReset().mockReturnValue(0);
    spawnSync.mockReset().mockReturnValueOnce(reply({ status: 'unlocked' })).mockReturnValueOnce(reply(item));
  });

  afterEach(() => {
    process.argv = argv;
    if (session === undefined) delete process.env.BW_SESSION;
    else process.env.BW_SESSION = session;
    process.exitCode = exitCode;
    stdout.mockRestore();
    stderr.mockRestore();
  });

  const failed = () => {
    expect(stdout).not.toHaveBeenCalled();
    expect(stderr).toHaveBeenCalledTimes(1);
    expect(stderr.mock.calls[0][0]).not.toMatch(/synthetic-session|sensitive-vault-output|p\nword/);
    expect(process.exitCode).toBe(1);
  };

  test('returns only normalized credentials and preserves password bytes', () => {
    run();
    expect(stdout).toHaveBeenCalledTimes(1);
    expect(JSON.parse(stdout.mock.calls[0][0])).toEqual({ user: item.login.username, password: item.login.password });
    expect(stderr).not.toHaveBeenCalled();
    expect(spawnSync.mock.calls.map(call => call[1])).toEqual([
      ['--nointeraction', '--raw', 'status'],
      ['--nointeraction', '--raw', 'get', 'item', id]
    ]);
    for (const [path, args, options] of spawnSync.mock.calls) {
      expect(path).toBe('/trusted/bw');
      expect(args).not.toContain('synthetic-session');
      expect(options).toMatchObject({ shell: false, stdio: ['ignore', 'pipe', 'pipe'], timeout: 12000, killSignal: 'SIGKILL' });
      expect(options.env.BW_SESSION).toBe('synthetic-session');
    }
    expect(spawnSync.mock.calls[1][2].maxBuffer).toBeLessThan(64 * 1024);
  });

  test.each([
    ['relative executable', ['bw', id]],
    ['invalid UUID', ['/trusted/bw', 'bad;command']],
    ['extra argument', ['/trusted/bw', id, 'unexpected']]
  ])('rejects %s before executing', (_, args) => {
    process.argv = ['node', 'helper', ...args];
    run(); failed();
    expect(spawnSync).not.toHaveBeenCalled();
  });

  test('rejects a missing session before executing', () => {
    delete process.env.BW_SESSION;
    run(); failed();
    expect(spawnSync).not.toHaveBeenCalled();
  });

  test.each(['locked', 'unauthenticated'])('rejects %s without retrieving an item', status => {
    spawnSync.mockReset().mockReturnValue(reply({ status }));
    run(); failed();
    expect(spawnSync).toHaveBeenCalledTimes(1);
  });

  test.each([
    { ...item, id: '22222222-2222-4222-8222-222222222222' },
    { ...item, type: 2 },
    { ...item, deletedDate: '2026-01-01' },
    { ...item, login: { username: 'example-user', password: '' } },
    { ...item, login: { username: 42, password: 'value' } }
  ])('rejects wrong, deleted or incomplete Login items %#', invalid => {
    spawnSync.mockReset().mockReturnValueOnce(reply({ status: 'unlocked' })).mockReturnValueOnce(reply(invalid));
    run(); failed();
  });

  test.each([
    { status: 1, stdout: Buffer.from('sensitive-vault-output'), stderr: Buffer.from('synthetic-session') },
    { status: null, error: new Error('sensitive-vault-output') },
    { status: 0, stdout: Buffer.from('not-json sensitive-vault-output') },
    { status: 0, stdout: Buffer.from([0xff]) },
    { status: 0, stdout: Buffer.alloc(64 * 1024), stderr: Buffer.from('too much') }
  ])('fails closed for CLI, parser or output errors %#', result => {
    spawnSync.mockReset().mockReturnValue(result);
    run(); failed();
  });

  test('shares one deadline across status and item calls', () => {
    performance.now.mockReturnValueOnce(0).mockReturnValueOnce(0).mockReturnValueOnce(12001);
    run(); failed();
    expect(spawnSync).toHaveBeenCalledTimes(1);
  });
});
