jest.mock('fs');
jest.mock('../../src/utils/so10-files', () => ({ ...jest.requireActual('../../src/utils/so10-files'), getRepository: jest.fn() }));
const so10Files = require('../../src/utils/so10-files');
jest.mock('../../src/utils/transport-selector', () => ({ selectTransport: jest.fn(), _getTransportHookConfig: jest.fn() }));
const selector = require('../../src/utils/transport-selector');
const fs = require('fs');
const command = require('../../src/commands/so10');

describe('so10 command', () => {
  let context, http, output, errors, environmentTransport;
  beforeEach(() => {
    jest.resetAllMocks();
    so10Files.getRepository.mockReturnValue(null);
    environmentTransport = process.env.ABAP_TRANSPORT;
    delete process.env.ABAP_TRANSPORT;
    process.exitCode = undefined;
    output = jest.spyOn(console, 'log').mockImplementation(() => {});
    errors = jest.spyOn(console, 'error').mockImplementation(() => {});
    http = { fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: jest.fn().mockResolvedValue({
      success: true, action: 'UPDATE', object: 'TEXT', name: 'Mixed.Name', id: 'ST', language: 'E', data_client: '100', saved: true,
      baseline_recorded: false, transport_recorded: false, message: 'SO10 text saved'
    }) };
    http.post.mockImplementation(async (_, body) => body.action === 'READ' ? {
      success: false, error_kind: 'NOT_FOUND', object: body.object, name: body.name, id: body.id, language: 'E', data_client: '100'
    } : { success: true, action: body.action, object: body.object, name: body.name, id: body.id, language: body.language,
      data_client: body.target_client || '100', source_client: body.source_client, saved: true,
      baseline_recorded: false, transport_recorded: false, message: 'SO10 text saved' });
    context = { loadConfig: jest.fn().mockReturnValue({ client: '100' }), AbapHttp: jest.fn(() => http) };
    fs.readFileSync.mockReturnValue(Buffer.from('First\r\n\r\nLast\r\n'));
  });
  afterEach(() => {
    jest.restoreAllMocks(); process.exitCode = undefined;
    if (environmentTransport === undefined) delete process.env.ABAP_TRANSPORT;
    else process.env.ABAP_TRANSPORT = environmentTransport;
  });

  test('help does not need config or files', async () => {
    await command.execute(['--help'], context);
    expect(context.loadConfig).not.toHaveBeenCalled();
    expect(fs.readFileSync).not.toHaveBeenCalled();
  });
  test.each(['create', 'update'])('%s preserves text name, normalizes delimiters and defaults Text ID', async (action) => {
    await command.execute([action, 'Mixed.Name', '--language', 'en', '--file', '/tmp/input.txt', '--conflict-mode', 'ignore', '--json'], context);
    expect(http.post).toHaveBeenCalledWith('/sap/bc/z_abapgit_agent/so10', {
      action: action.toUpperCase(), object: 'TEXT', name: 'Mixed.Name', id: 'ST', language: 'E', conflict_mode: 'ignore',
      lines: [{ tdformat: '*', tdline: 'First' }, { tdformat: '*', tdline: '' }, { tdformat: '*', tdline: 'Last' }]
    }, { csrfToken: 'token' });
    expect(JSON.parse(output.mock.calls[0][0]).success).toBe(true);
    expect(output).toHaveBeenCalledTimes(1);
  });
  test('COPY defaults source to the development profile and target to customizingClient, without changing logon or files', async () => {
    const config = { client: '100', customizingClient: '200', user: 'DEV_TEST', password: 'test-only' };
    context.loadConfig.mockReturnValue(config);
    await command.execute(['copy', 'Mixed.Name', '--language', 'E', '--json'], context);
    expect(context.AbapHttp).toHaveBeenCalledTimes(1); expect(context.AbapHttp).toHaveBeenCalledWith(config);
    expect(http.post).toHaveBeenCalledTimes(1);
    expect(http.post.mock.calls[0][1]).toMatchObject({ source_client: '100', target_client: '200', action: 'COPY' });
    expect(fs.readFileSync).not.toHaveBeenCalled(); expect(so10Files.getRepository).not.toHaveBeenCalled();
  });
  test('COPY explicit target permits omitted source but equal resolved defaults fail before connection', async () => {
    await command.execute(['copy', 'NAME', '--language', 'E', '--target-client', '124', '--json'], context);
    expect(http.post.mock.calls[0][1]).toMatchObject({ source_client: '100', target_client: '124' });
    http.post.mockClear(); context.AbapHttp.mockClear();
    await command.execute(['copy', 'NAME', '--language', 'E', '--json'], context);
    expect(context.AbapHttp).not.toHaveBeenCalled(); expect(http.post).not.toHaveBeenCalled();
  });
  test('transport explicit target resolves matching profile/task before lookup or HTTP', async () => {
    const config = { client: '100', transport: 'DEVK900001', customizingClient: '201', customizingLogon: {
      client: '200', user: 'CTS_TEST', password: 'test-only', transport: 'DEVK900002'
    } };
    context.loadConfig.mockReturnValue(config);
    await command.execute(['transport', 'NAME', '--language', 'E', '--target-client', '200', '--json'], context);
    expect(context.AbapHttp.mock.calls[0][0]).toMatchObject({ client: '200', user: 'CTS_TEST', transport: 'DEVK900002' });
    expect(http.post.mock.calls[0][1]).toMatchObject({ action: 'TRANSPORT', transport: 'DEVK900002' });
    expect(http.post.mock.calls[0][1]).not.toHaveProperty('target_client');
  });
  test('transport mismatched explicit target fails before HTTP/task lookup', async () => {
    context.loadConfig.mockReturnValue({ client: '100', customizingClient: '200', customizingLogon: {
      client: '200', user: 'CTS_TEST', password: 'test-only'
    } });
    await command.execute(['transport', 'NAME', '--language', 'E', '--target-client', '201', '--json'], context);
    expect(context.AbapHttp).not.toHaveBeenCalled(); expect(selector.selectTransport).not.toHaveBeenCalled();
    expect(http.post).not.toHaveBeenCalled();
  });
  test('read accepts SYST and does not read a local file', async () => {
    await command.execute(['read', 'Mixed.Name', '--language', 'E', '--id', 'SYST', '--json'], context);
    expect(http.post.mock.calls[0][1]).toEqual({ action: 'READ', object: 'TEXT', name: 'Mixed.Name', id: 'SYST', language: 'E' });
    expect(fs.readFileSync).not.toHaveBeenCalled();
  });
  test('read selects a data client without changing the authenticated profile', async () => {
    await command.execute(['read', 'Mixed.Name', '--language', 'E', '--source-client', '124', '--json'], context);
    expect(context.AbapHttp).toHaveBeenCalledWith({ client: '100' });
    expect(http.post.mock.calls[0][1]).toEqual({ action: 'READ', object: 'TEXT', name: 'Mixed.Name', id: 'ST', language: 'E', client: '124' });
    expect(fs.readFileSync).not.toHaveBeenCalled();
  });
  describe('header-only list', () => {
    test('sends repeated literal prefixes with default combined cap and no implicit filters', async () => {
      const result = { success: true, action: 'LIST', keys: [], max_hits: 100, at_least: 0, saved: false };
      http.post.mockResolvedValue(result);
      await command.execute(['list', '--prefix', 'Zapp_', '--prefix', 'Y_', '--json'], context);
      expect(http.post).toHaveBeenCalledWith('/sap/bc/z_abapgit_agent/so10', {
        action: 'LIST', object: 'TEXT', name: '', id: '', language: '', prefixes: ['Zapp_', 'Y_'], max_hits: 100
      }, { csrfToken: 'token' });
      expect(JSON.parse(output.mock.calls[0][0])).toEqual(result);
      expect(fs.readFileSync).not.toHaveBeenCalled();
    });
    test('accepts exact filters, source data client and hard maximum', async () => {
      await command.execute(['list', '--prefix', 'Z_', '--name', 'Mixed.Name', '--id', 'syst', '--language', 'en',
        '--source-client', '124', '--max-hits', '1000', '--json'], context);
      expect(http.post.mock.calls[0][1]).toEqual({ action: 'LIST', object: 'TEXT', name: 'Mixed.Name', id: 'SYST',
        language: 'EN', client: '124', prefixes: ['Z_'], max_hits: 1000 });
      expect(context.AbapHttp).toHaveBeenCalledWith({ client: '100' });
    });
    test.each([
      [], ['--prefix', '*'], ['--prefix', 'Z+'], ['--prefix', 'Z%'], ['--prefix', 'Z\n'],
      ['--prefix', ' '.repeat(2)], ['--prefix', 'Z'.repeat(71)],
      ['--prefix', 'Z', '--max-hits', '0'], ['--prefix', 'Z', '--max-hits', '1001'],
      ['--prefix', 'Z', '--max-hits', '100\n'], ['--prefix', 'Z', '--max-hits', '1e2'],
      ['--prefix', 'Z', '--max-hits', '1', '--max-hits', '2'],
      ['--prefix', 'Z', '--file', 'x'], ['--prefix', 'Z', '--transport', 'DEVK900001'],
      ['--prefix', 'Z', '--source-client', '124\n'], ['--prefix', 'Z', '--target-client', '124']
    ].map((args) => [args]))('rejects unsafe list arguments before config or HTTP: %j', async (args) => {
      await command.execute(['list', ...args, '--json'], context);
      expect(process.exitCode).toBe(1);
      expect(context.loadConfig).not.toHaveBeenCalled();
      expect(http.post).not.toHaveBeenCalled();
      expect(fs.readFileSync).not.toHaveBeenCalled();
    });
    test('overflow remains failure and performs no local writes or follow-up reads', async () => {
      http.post.mockResolvedValue({ success: false, error_kind: 'OVERFLOW', error: 'Narrow prefixes', keys: [],
        max_hits: 100, at_least: 101, has_more: true, saved: false });
      await command.execute(['list', '--prefix', 'Z', '--json'], context);
      expect(process.exitCode).toBe(1);
      expect(JSON.parse(output.mock.calls[0][0])).toMatchObject({ keys: [], at_least: 101, saved: false });
      expect(http.post).toHaveBeenCalledTimes(1);
      expect(fs.writeFileSync).not.toHaveBeenCalled();
    });
    test('lost list response never claims unknown save', async () => {
      http.post.mockRejectedValue(new Error('lost response'));
      await command.execute(['list', '--prefix', 'Z', '--json'], context);
      expect(JSON.parse(output.mock.calls[0][0])).toMatchObject({ success: false, saved: false, transport_recorded: false });
    });
    test('human output prints headers rather than undefined text fields', async () => {
      http.post.mockResolvedValue({ success: true, keys: [{ name: 'ZAPP_1', id: 'ST', language: 'E' }], max_hits: 100, at_least: 1 });
      await command.execute(['list', '--prefix', 'ZAPP_'], context);
      expect(output.mock.calls.flat().join('\n')).toContain('ZAPP_1');
      expect(output.mock.calls.flat().join('\n')).not.toContain('undefined');
    });
  });
  test('ITF formatting is unchanged', async () => {
    const lines = [{ tdformat: '*', tdline: 'Title' }, { tdformat: '', tdline: ' next' }];
    fs.readFileSync.mockReturnValue(Buffer.from(JSON.stringify(lines)));
    await command.execute(['update', 'NAME', '--language', 'EN', '--itf-file', 'texts/so10/name.json', '--conflict-mode', 'ignore'], context);
    expect(http.post.mock.calls.find(([, body]) => body.action === 'UPDATE')[1].lines).toEqual(lines);
  });
  test.each([
    [], ['delete', 'NAME', '--language', 'EN'], ['read', 'NAME'],
    ['read', 'NAME', '--language'], ['read', 'NAME', '--language', 'EN', '--file', 'x'],
    ['create', 'NAME', '--language', 'EN'],
    ['update', 'NAME', '--language', 'EN', '--file', 'a', '--itf-file', 'b'],
    ['read', 'NAME', '--language', 'EN', '--language', 'E'],
    ['read', '*', '--language', 'EN'], ['read', 'X'.repeat(71), '--language', 'EN'],
    ['read', 'NAME', '--language', 'EN', '--id', 'TOOLONG'],
    ['update', 'NAME', '--language', 'EN', '--file', 'a', '--transport', 'DEVK900001']
  ].map((args) => [args]))('rejects invalid arguments before SAP: %j', async (args) => {
    await command.execute([...args, '--json'], context);
    expect(process.exitCode).toBe(1);
    expect(context.loadConfig).not.toHaveBeenCalled();
    expect(http.post).not.toHaveBeenCalled();
    expect(JSON.parse(output.mock.calls[0][0]).success).toBe(false);
  });
  test.each([Buffer.from([]), Buffer.from('\n'), Buffer.from([0xff]), Buffer.from('x'.repeat(133))])('rejects unsafe plain input before HTTP', async (content) => {
    fs.readFileSync.mockReturnValue(content);
    await command.execute(['update', 'NAME', '--language', 'EN', '--file', 'x', '--json'], context);
    expect(http.post).not.toHaveBeenCalled();
    expect(process.exitCode).toBe(1);
  });
  test.each(['not json', '{}', '[]', '[{"tdformat":"***","tdline":"x"}]', '[{"tdformat":"*","tdline":"x","extra":1}]'])('rejects invalid ITF %s', async (content) => {
    fs.readFileSync.mockReturnValue(Buffer.from(content));
    await command.execute(['update', 'NAME', '--language', 'EN', '--itf-file', 'x', '--json'], context);
    expect(http.post).not.toHaveBeenCalled();
    expect(process.exitCode).toBe(1);
  });
  describe('client copy', () => {
    let sourceHttp, targetHttp, lines, config;
    const args = ['copy', 'Mixed.Name', '--language', 'EN', '--source-client', '100', '--target-client', '200', '--json'];
    beforeEach(() => {
      config = { client: '300', host: 'same-system' };
      lines = [{ tdformat: '*', tdline: 'Title' }, { tdformat: '', tdline: ' next' }];
      sourceHttp = { fetchCsrfToken: jest.fn().mockResolvedValue('source-token'), post: jest.fn().mockResolvedValue({
        success: true, object: 'TEXT', name: 'Mixed.Name', id: 'ST', language: 'E', client: '100', lines
      }) };
      targetHttp = { fetchCsrfToken: jest.fn().mockResolvedValue('target-token'), post: jest.fn().mockResolvedValue({
        success: true, action: 'COPY', saved: true, transport_recorded: false, client: '200', source_client: '100'
      }) };
      context.loadConfig.mockReturnValue(config);
      context.AbapHttp.mockImplementation(() => targetHttp);
    });
    test.each([false, true])('uses one authenticated session and backend client parameters; overwrite=%s', async (overwrite) => {
      await command.execute([...args, ...(overwrite ? ['--overwrite'] : [])], context);
      expect(context.AbapHttp.mock.calls.map(([c]) => c)).toEqual([config]);
      expect(config.client).toBe('300');
      expect(sourceHttp.post).not.toHaveBeenCalled();
      expect(targetHttp.post).toHaveBeenCalledWith('/sap/bc/z_abapgit_agent/so10', {
        action: 'COPY', object: 'TEXT', name: 'Mixed.Name', id: 'ST', language: 'EN', source_client: '100', target_client: '200', overwrite
      }, { csrfToken: 'target-token' });
      expect(targetHttp.post.mock.calls[0][1]).not.toHaveProperty('lines');
      expect(fs.readFileSync).not.toHaveBeenCalled();
      expect(output).toHaveBeenCalledTimes(1);
      expect(JSON.parse(output.mock.calls[0][0]).saved).toBe(true);
    });
    test.each(['Source text not found', 'Source authorization denied', 'Target client is closed', 'Target text already exists', 'Invalid source text'])('reports native copy failure without a separate source connection: %s', async (error) => {
      targetHttp.post.mockResolvedValue({ success: false, error, saved: false });
      await command.execute(args, context);
      expect(sourceHttp.post).not.toHaveBeenCalled();
      expect(targetHttp.post).toHaveBeenCalledTimes(1);
      expect(context.AbapHttp).toHaveBeenCalledTimes(1);
      expect(process.exitCode).toBe(1);
      expect(JSON.parse(output.mock.calls[0][0]).saved).toBe(false);
      expect(JSON.parse(output.mock.calls[0][0]).error).toBe(error);
    });
    test.each(['token', 'write'])('single-session network failure at %s preserves save outcome', async (stage) => {
      if (stage === 'token') targetHttp.fetchCsrfToken.mockRejectedValue(new Error('secret credentials'));
      else targetHttp.post.mockRejectedValue(new Error('secret text'));
      await command.execute(args, context);
      const result = JSON.parse(output.mock.calls[0][0]);
      expect(result.saved).toBe(stage === 'write' ? null : false);
      expect(output.mock.calls[0][0]).not.toMatch(/secret|credentials/);
      expect(sourceHttp.post).not.toHaveBeenCalled();
      expect(process.exitCode).toBe(1);
    });
    test('existing/locked target semantic failure stays nonzero and does not modify source', async () => {
      targetHttp.post.mockResolvedValue({ success: false, error: 'Target text already exists', saved: false });
      await command.execute(args, context);
      expect(process.exitCode).toBe(1);
      expect(JSON.parse(output.mock.calls[0][0]).saved).toBe(false);
      expect(sourceHttp.post).not.toHaveBeenCalled();
      expect(targetHttp.post).toHaveBeenCalledTimes(1);
    });
  });
  test.each([
    ['copy', 'NAME', '--language', 'E', '--target-client', '124\n'],
    ['copy', 'NAME', '--language', 'E', '--source-client', '100', '--target-client', '100'],
    ['copy', 'NAME', '--language', 'E', '--source-client', '1', '--target-client', '200'],
    ['copy', 'NAME', '--language', 'E', '--source-client', '100', '--target-client', 'bad'],
    ['copy', 'NAME', '--language', 'E', '--source-client', '100', '--target-client', '200', '--file', 'x'],
    ['read', 'NAME', '--language', 'E', '--overwrite'],
    ['create', 'NAME', '--language', 'E', '--file', 'x', '--source-client', '100'],
    ['copy', 'NAME', '--language', 'E', '--source-client', '100', '--target-client', '200', '--overwrite', '--overwrite']
  ].map((args) => [args]))('rejects invalid copy options before config: %j', async (args) => {
    await command.execute([...args, '--json'], context);
    expect(context.loadConfig).not.toHaveBeenCalled();
    expect(http.post).not.toHaveBeenCalled();
    expect(process.exitCode).toBe(1);
  });
  describe('separate customizing transport recording', () => {
    const args = ['transport', 'Mixed.Name', '--language', 'E', '--json'];
    beforeEach(() => {
      context.getTransport = jest.fn();
      context.getTransportSettings = jest.fn();
      selector.selectTransport.mockResolvedValue(null);
      selector._getTransportHookConfig.mockReturnValue(null);
      http.post.mockResolvedValue({ success: true, action: 'TRANSPORT', saved: false, transport_recorded: true,
        transport_request: 'DEVK900001', transport_task: 'DEVK900002', object_key: 'TEXT,Mixed.Name,ST,E' });
    });
    test('explicit request has priority and sends no text content', async () => {
      context.getTransport.mockReturnValue('DEVK900003');
      await command.execute([...args, '--transport', 'DEVK900001'], context);
      expect(http.post).toHaveBeenCalledWith('/sap/bc/z_abapgit_agent/so10', {
        action: 'TRANSPORT', object: 'TEXT', name: 'Mixed.Name', id: 'ST', language: 'E', transport: 'DEVK900001'
      }, { csrfToken: 'token' });
      expect(context.getTransport).not.toHaveBeenCalled();
      expect(fs.readFileSync).not.toHaveBeenCalled();
      expect(JSON.parse(output.mock.calls[0][0]).saved).toBe(false);
      expect(JSON.parse(output.mock.calls[0][0]).transport_recorded).toBe(true);
    });
    test.each(['config', 'environment'])('uses effective %s transport resolution', async (source) => {
      if (source === 'config') context.loadConfig.mockReturnValue({ client: '100', transport: 'DEVK900003' });
      else process.env.ABAP_TRANSPORT = 'DEVK900003';
      await command.execute(args, context);
      expect(http.post.mock.calls[0][1].transport).toBe('DEVK900003');
      expect(context.getTransport).not.toHaveBeenCalled();
      expect(selector.selectTransport).not.toHaveBeenCalled();
    });
    test('customizing hook may resolve a request in JSON mode without a picker', async () => {
      selector._getTransportHookConfig.mockReturnValue({ hook: 'existing-hook.js' });
      selector.selectTransport.mockResolvedValue('DEVK900004');
      await command.execute(args, context);
      expect(selector.selectTransport).toHaveBeenCalledWith({ client: '100' }, http, expect.any(Function),
        context.AbapHttp, context.getTransportSettings, 'customizing', { existingOnly: true });
      expect(selector.selectTransport.mock.calls[0][2]()).toEqual({ client: '100' });
      expect(http.post.mock.calls[0][1].transport).toBe('DEVK900004');
    });
    test('resolves customizing credentials and task before constructing HTTP, without using a development task', async () => {
      const config = { client: '300', user: 'DEV_USER', password: 'dev-test-only', transport: 'DEVK900001',
        customizingClient: '200', customizingLogon: { client: '200', user: 'CTS_USER', password: 'cts-test-only', transport: 'DEVK900003' } };
      context.loadConfig.mockReturnValue(config);
      context.getTransport.mockReturnValue('DEVK900001');
      await command.execute(args, context);
      expect(context.AbapHttp.mock.calls[0][0]).toMatchObject({ client: '200', user: 'CTS_USER', password: 'cts-test-only', transport: 'DEVK900003' });
      expect(http.post.mock.calls[0][1].transport).toBe('DEVK900003');
      expect(context.getTransport).not.toHaveBeenCalled();
      expect(config.client).toBe('300');
    });
    test('a customizing hook receives the effective profile and existing-only selection', async () => {
      context.loadConfig.mockReturnValue({ client: '300', transport: 'DEVK900001', customizingClient: '200',
        customizingLogon: { client: '200', user: 'CTS_USER', password: 'cts-test-only' } });
      selector._getTransportHookConfig.mockReturnValue({ hook: 'existing-hook.js' });
      selector.selectTransport.mockResolvedValue('DEVK900004');
      await command.execute(args, context);
      const [profile, selectedHttp, loadEffective, , , type, policy] = selector.selectTransport.mock.calls[0];
      expect(profile.client).toBe('200'); expect(profile.transport).toBeUndefined();
      expect(selectedHttp).toBe(http); expect(loadEffective()).toBe(profile);
      expect(type).toBe('customizing'); expect(policy).toEqual({ existingOnly: true });
    });
    test('missing or mismatched customizing credentials fail before hooks, HTTP or task lookup', async () => {
      context.loadConfig.mockReturnValue({ client: '300', customizingClient: '200',
        customizingLogon: { client: '201', user: 'CTS_USER', password: 'test-only' } });
      await command.execute([...args, '--transport', 'DEVK900003'], context);
      expect(context.AbapHttp).not.toHaveBeenCalled(); expect(context.getTransport).not.toHaveBeenCalled();
      expect(selector.selectTransport).not.toHaveBeenCalled(); expect(http.fetchCsrfToken).not.toHaveBeenCalled();
      expect(JSON.parse(output.mock.calls[0][0]).transport_recorded).toBe(false);
      expect(process.exitCode).toBe(1);
    });
    test('missing request fails before recording, with no implicit workbench fallback', async () => {
      await command.execute(args, context);
      expect(selector.selectTransport).not.toHaveBeenCalled();
      expect(http.post).not.toHaveBeenCalled();
      expect(process.exitCode).toBe(1);
      expect(JSON.parse(output.mock.calls[0][0]).transport_recorded).toBe(false);
    });
    test.each(['token', 'record'])('network failure at %s keeps text unchanged and distinguishes recording uncertainty', async (stage) => {
      context.loadConfig.mockReturnValue({ client: '100', transport: 'DEVK900003' });
      if (stage === 'token') http.fetchCsrfToken.mockRejectedValue(new Error('secret'));
      else http.post.mockRejectedValue(new Error('secret'));
      await command.execute(args, context);
      const result = JSON.parse(output.mock.calls[0][0]);
      expect(result.saved).toBe(false);
      expect(result.transport_recorded).toBe(stage === 'record' ? null : false);
      expect(process.exitCode).toBe(1);
      expect(output.mock.calls[0][0]).not.toContain('secret');
    });
    test.each([
      ['transport', 'NAME', '--language', 'E', '--transport', 'bad'],
      ['transport', 'NAME', '--language', 'E', '--transport', 'DEVK900001\n'],
      ['transport', 'NAME', '--language', 'E', '--transport', 'DEVK900001', '--file', 'x'],
      ['read', 'NAME', '--language', 'E', '--transport', 'DEVK900001'],
      ['copy', 'NAME', '--language', 'E', '--source-client', '100', '--target-client', '200', '--transport', 'DEVK900001']
    ].map((argv) => [argv]))('rejects invalid or combined operations before config: %j', async (argv) => {
      await command.execute([...argv, '--json'], context);
      expect(context.loadConfig).not.toHaveBeenCalled();
      expect(http.post).not.toHaveBeenCalled();
      expect(process.exitCode).toBe(1);
    });
  });
  test('semantic false is nonzero JSON, not truthy success', async () => {
    http.post.mockResolvedValue({ SUCCESS: 'false', ERROR: 'Text already exists' });
    await command.execute(['create', 'NAME', '--language', 'E', '--file', 'x', '--conflict-mode', 'ignore', '--json'], context);
    expect(process.exitCode).toBe(1);
    expect(JSON.parse(output.mock.calls[0][0]).ERROR).toBe('Text already exists');
  });
  test('write network failure does not claim nothing was saved or leak the request', async () => {
    http.post.mockRejectedValue(new Error('secret content and password'));
    await command.execute(['update', 'NAME', '--language', 'E', '--file', 'x', '--conflict-mode', 'ignore', '--json'], context);
    const result = JSON.parse(output.mock.calls[0][0]);
    expect(result.saved).toBeNull();
    expect(result.error).toMatch(/unknown/);
    expect(output.mock.calls[0][0]).not.toMatch(/secret|password/);
    expect(errors).not.toHaveBeenCalled();
    expect(process.exitCode).toBe(1);
  });
});
