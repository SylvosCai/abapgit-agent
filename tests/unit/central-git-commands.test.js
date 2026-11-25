jest.mock('../../src/utils/backgroundJobPoller', () => ({
  startBackgroundJob: jest.fn().mockResolvedValue({ jobNumber: '1' }),
  pollForCompletion: jest.fn().mockResolvedValue({ status: 'completed', result: '{"success":true}' }),
  displayProgress: jest.fn(), formatTimestamp: jest.fn(), calculateTimeSpent: jest.fn()
}));
jest.mock('../../src/config', () => ({ getExcludePatterns: () => [], getUnsupportedTypesFlag: () => true }));
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const drop = require('../../src/commands/drop');
const create = require('../../src/commands/create');
const pull = require('../../src/commands/pull');
const importing = require('../../src/commands/import');
const { startBackgroundJob } = require('../../src/utils/backgroundJobPoller');
describe('Git-consuming commands use the actual destination resolver', () => {
  let context, http, exitCode, dir, file;
  beforeEach(() => {
    dir = fs.mkdtempSync(path.join(os.tmpdir(), 'abgagt-drop-test-'));
    file = path.join(dir, 'zcl_fixture.clas.abap');
    fs.writeFileSync(file, 'dummy source');
    exitCode = process.exitCode;
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
    jest.spyOn(process, 'exit').mockImplementation(code => { throw new Error(`exit ${code}`); });
    http = { fetchCsrfToken: jest.fn().mockResolvedValue('csrf'), post: jest.fn().mockResolvedValue({ SUCCESS: 'X' }) };
    context = { loadConfig: () => ({ host: 'sap.example', package: 'ZTEST', gitProfile: 'work' }),
      AbapHttp: function () { return http; }, gitUtils: { getRemoteUrl: () => 'https://git.example/repo', getBranch: () => 'main' },
      getSafeguards: () => ({}), resolveGitCredentials: jest.fn().mockResolvedValue({ username: 'central', password: 'credential-bytes' }) };
    startBackgroundJob.mockClear();
  });
  afterEach(() => { fs.rmSync(dir, { recursive: true, force: true }); process.exitCode = exitCode; jest.restoreAllMocks(); });
  test('drop preflights and pins central credentials for the actual nested pull', async () => {
    await drop.execute(['--files', file, '--pull'], context);
    expect(context.resolveGitCredentials).toHaveBeenCalledTimes(1);
    expect(context.resolveGitCredentials).toHaveBeenCalledWith('https://git.example/repo');
    expect(context.resolveGitCredentials.mock.invocationCallOrder[0]).toBeLessThan(http.fetchCsrfToken.mock.invocationCallOrder[0]);
    expect(http.post.mock.calls[0][0]).toMatch(/\/drop$/);
    expect(http.post.mock.calls[1]).toEqual([expect.stringMatching(/\/pull$/), expect.objectContaining({ username: 'central', password: 'credential-bytes' }), expect.any(Object)]);
  });
  test('drop lookup failure or missing remote prevents deletion', async () => {
    context.resolveGitCredentials.mockRejectedValue(new Error('Destination mismatch'));
    await expect(drop.execute(['--files', file, '--pull'], context)).rejects.toThrow('Destination mismatch');
    context.gitUtils.getRemoteUrl = () => null;
    await expect(drop.execute(['--files', file, '--pull'], context)).rejects.toThrow('No object was deleted');
    expect(http.fetchCsrfToken).not.toHaveBeenCalled();
    expect(http.post).not.toHaveBeenCalled();
  });
  test('drop without pull does not resolve Git credentials', async () => {
    await drop.execute(['--files', file], context);
    expect(context.resolveGitCredentials).not.toHaveBeenCalled();
    expect(http.post).toHaveBeenCalledTimes(1);
  });
  test('create injects only resolved credentials', async () => {
    await create.execute([], context);
    expect(context.resolveGitCredentials).toHaveBeenCalledWith('https://git.example/repo');
    expect(http.post).toHaveBeenCalledWith(expect.any(String), expect.objectContaining({ username: 'central', password: 'credential-bytes' }), expect.any(Object));
  });
  test('import passes the actual remote through lazy resolution', async () => {
    await importing.execute([], context);
    expect(context.resolveGitCredentials).toHaveBeenCalledWith('https://git.example/repo');
    expect(startBackgroundJob).toHaveBeenCalled();
    expect(startBackgroundJob.mock.calls[0]).toEqual(expect.arrayContaining([expect.objectContaining({ username: 'central', password: 'credential-bytes' })]));
  });
  test('pull resolves an explicit URL, while explicit null remains public/no lookup', async () => {
    await pull.pull('https://other.example/repo', 'main', null, null, context.loadConfig, context.AbapHttp, true, undefined, 'abort', false, false, false, context.resolveGitCredentials);
    expect(context.resolveGitCredentials).toHaveBeenCalledWith('https://other.example/repo');
    context.resolveGitCredentials.mockClear(); http.post.mockClear();
    await pull.pull('https://public.example/repo', 'main', null, null, context.loadConfig, context.AbapHttp, true, null, 'abort', false, false, false, context.resolveGitCredentials);
    expect(context.resolveGitCredentials).not.toHaveBeenCalled();
    expect(http.post.mock.calls[0][1]).not.toHaveProperty('username');
    expect(http.post.mock.calls[0][1]).not.toHaveProperty('password');
  });
  test('embedded URL credentials are rejected before display or SAP contact', async () => {
    context.gitUtils.getRemoteUrl = () => 'https://name:private-marker@git.example/repo';
    await expect(create.execute([], context)).rejects.toThrow('embedded credentials');
    await expect(importing.execute([], context)).rejects.toThrow();
    await expect(pull.pull(context.gitUtils.getRemoteUrl(), 'main', null, null, context.loadConfig, context.AbapHttp, false, undefined, 'abort', false, false, false, context.resolveGitCredentials)).rejects.toThrow('embedded credentials');
    expect(http.fetchCsrfToken).not.toHaveBeenCalled();
    expect(http.post).not.toHaveBeenCalled();
    expect(context.resolveGitCredentials).not.toHaveBeenCalled();
    expect(console.log.mock.calls.flat().join(' ')).not.toContain('private-marker');
    expect(console.error.mock.calls.flat().join(' ')).not.toContain('private-marker');
  });
  test('resolver rejection prevents every Git-consuming SAP request', async () => {
    context.resolveGitCredentials.mockRejectedValue(new Error('Destination mismatch'));
    await expect(create.execute([], context)).rejects.toThrow('Destination mismatch');
    expect(http.post).not.toHaveBeenCalled();
    await expect(pull.pull('https://bad.example/repo', 'main', null, null, context.loadConfig, context.AbapHttp, true, undefined, 'abort', false, false, false, context.resolveGitCredentials)).resolves.toEqual({ success: false, message: 'Destination mismatch' });
    expect(http.post).not.toHaveBeenCalled();
    expect(process.exitCode).toBe(1);
    await expect(importing.execute([], context)).rejects.toThrow();
    expect(startBackgroundJob).not.toHaveBeenCalled();
  });
});
