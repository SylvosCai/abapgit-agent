describe('isolated existing-only transport selection', () => {
  let selector, http, profile, execute;
  beforeEach(() => {
    jest.resetModules();
    jest.dontMock('readline');
    profile = { client: '200', user: 'CTS_USER', password: 'test-only' };
    http = { get: jest.fn().mockResolvedValue({ transports: [] }), post: jest.fn() };
    execute = jest.fn(async (args, context) => {
      expect(context.loadConfig()).toBe(profile);
      expect(new context.AbapHttp()).toBe(http);
      console.log(JSON.stringify({ success: true, transports: [] }));
    });
    jest.doMock('../../src/commands/transport', () => ({ execute }));
    selector = require('../../src/utils/transport-selector');
  });
  afterEach(() => { jest.restoreAllMocks(); jest.dontMock('readline'); jest.dontMock('../../src/commands/transport'); });
  const runFor = (selector, profile, http) => selector.buildRun(profile, http, () => profile, function () { return http; },
    () => ({ allowCreate: true, allowRelease: true }), 'customizing', { existingOnly: true });

  test.each(['transport create --description test', 'transport release DEVK900001', 'so10 create NAME'])('blocks %s before command/HTTP execution', async (command) => {
    await expect(runFor(selector, profile, http)(command)).rejects.toThrow(/only transport list/);
    expect(execute).not.toHaveBeenCalled(); expect(http.post).not.toHaveBeenCalled();
  });
  test('runs an existing-task list in the effective profile with JSON/customizing type', async () => {
    await expect(runFor(selector, profile, http)('transport list --scope task')).resolves.toEqual({ success: true, transports: [] });
    expect(execute.mock.calls[0][0]).toEqual(['list', '--scope', 'task', '--type', 'customizing', '--json']);
    expect(http.post).not.toHaveBeenCalled();
  });
  test('selectTransport passes existing-only policy to the actual hook run helper', async () => {
    jest.spyOn(selector, '_getTransportHookConfig').mockReturnValue({ hook: 'test-hook.js' });
    jest.spyOn(selector, 'runHook').mockImplementation(async (_, context) => context.run('transport create'));
    await expect(selector.selectTransport(profile, http, () => profile, function () { return http; },
      () => ({ allowCreate: true }), 'customizing', { existingOnly: true })).resolves.toBeNull();
    expect(execute).not.toHaveBeenCalled(); expect(http.post).not.toHaveBeenCalled();
  });
  test('existing-only picker neither offers nor executes Create even when c is entered', async () => {
    const answers = ['c', '0']; const close = jest.fn();
    jest.doMock('readline', () => ({ createInterface: () => ({ question: (_, callback) => callback(answers.shift()), close }) }));
    const output = jest.spyOn(process.stderr, 'write').mockImplementation(() => true);
    await expect(selector.interactivePicker(http, 'customizing', { existingOnly: true })).resolves.toBeNull();
    expect(output.mock.calls.map(([text]) => text).join('')).not.toContain('Create new transport');
    expect(http.post).not.toHaveBeenCalled(); expect(close).toHaveBeenCalledTimes(1);
  });
  test('general picker retains its existing Create behavior when the policy is absent', async () => {
    const answers = ['c', 'description'];
    jest.doMock('readline', () => ({ createInterface: () => ({ question: (_, callback) => callback(answers.shift()), close: jest.fn() }) }));
    jest.spyOn(process.stderr, 'write').mockImplementation(() => true);
    http.post.mockResolvedValue({ number: 'DEVK900001' });
    await expect(selector.interactivePicker(http, 'customizing')).resolves.toBe('DEVK900001');
    expect(http.post).toHaveBeenCalledWith('/sap/bc/z_abapgit_agent/transport', { action: 'CREATE', description: 'description', type: 'customizing' }, {});
  });
});
