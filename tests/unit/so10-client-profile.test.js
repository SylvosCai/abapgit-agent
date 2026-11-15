const { transportProfile } = require('../../src/utils/so10-client-profile');

describe('isolated SO10 transport profile', () => {
  const base = { host: 'same-system', client: '300', user: 'DEV_USER', password: 'dev-test-only', transport: 'DEVK900001' };
  test('same-client transport uses the existing profile', () => {
    expect(transportProfile(base)).toBe(base);
  });
  test('different-client logon replaces credentials and does not inherit a development task', () => {
    const config = { ...base, customizingClient: '200', customizingLogon: { client: '200', user: 'CTS_USER', password: 'cts-test-only' } };
    expect(transportProfile(config)).toMatchObject({ host: 'same-system', client: '200', user: 'CTS_USER', password: 'cts-test-only', transport: undefined });
    expect(config.client).toBe('300'); expect(config.transport).toBe('DEVK900001');
  });
  test('explicit target selects its matching logon before defaults', () => {
    expect(transportProfile({ ...base, customizingClient: '201', customizingLogon: {
      client: '200', user: 'CTS_USER', password: 'cts-test-only', transport: 'DEVK900002'
    } }, '200')).toMatchObject({ client: '200', user: 'CTS_USER', transport: 'DEVK900002' });
  });
  test('explicit configured-client test target retains only that original profile/task', () => {
    const config = { ...base, customizingClient: '200', customizingLogon: { client: '200', user: 'CTS_USER', password: 'cts-test-only' } };
    expect(transportProfile(config, '300')).toBe(config);
  });
  test('different explicit target cannot clone another logon credentials', () => {
    expect(() => transportProfile({ ...base, customizingClient: '200', customizingLogon: {
      client: '200', user: 'CTS_USER', password: 'cts-test-only'
    } }, '201')).toThrow(/Explicit customizingLogon/);
  });
  test('accepts a task explicitly associated with the customizing logon', () => {
    expect(transportProfile({ ...base, customizingClient: '200', customizingLogon: {
      client: '200', user: 'CTS_USER', password: 'cts-test-only', transport: 'DEVK900002'
    } }).transport).toBe('DEVK900002');
  });
  test.each([undefined, {}, { client: '201', user: 'CTS_USER', password: 'test' },
    { client: '200', user: '', password: 'test' }, { client: '200', user: 'CTS_USER' }])('fails closed for missing/mismatched customizing credentials: %j', (logon) => {
    expect(() => transportProfile({ ...base, customizingClient: '200', customizingLogon: logon })).toThrow(/Explicit customizingLogon/);
  });
  test.each(['2', 'bad', '200\n'])('rejects invalid client %s', (customizingClient) => {
    expect(() => transportProfile({ ...base, customizingClient })).toThrow(/Invalid customizing client/);
  });
});
