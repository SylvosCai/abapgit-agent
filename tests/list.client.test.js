/**
 * Unit tests for list command - ABAPClient
 */

// Mock fs module
const mockExistsSync = jest.fn();
const mockReadFileSync = jest.fn();
const mockWriteFileSync = jest.fn();
const mockUnlinkSync = jest.fn();

jest.mock('fs', () => ({
  existsSync: mockExistsSync,
  readFileSync: mockReadFileSync,
  writeFileSync: mockWriteFileSync,
  unlinkSync: mockUnlinkSync
}));

// Mock https and http
jest.mock('https', () => ({
  Agent: jest.fn(),
  request: jest.fn()
}));

jest.mock('http', () => ({
  request: jest.fn()
}));

// Mock logger
jest.mock('../src/logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
  debug: jest.fn()
}));

// Mock config
jest.mock('../src/config', () => ({
  getAbapConfig: jest.fn(() => ({
    host: 'test.sap.com',
    sapport: 44300,
    user: 'testuser',
    password: 'testpass',
    client: '100',
    language: 'EN'
  }))
}));

describe('List Command - ABAPClient', () => {
  let ABAPClient;
  let client;

  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();

    ABAPClient = require('../src/abap-client').ABAPClient;
    client = new ABAPClient();
  });

  test('list method exists and is callable', () => {
    expect(typeof client.list).toBe('function');
  });

  test('list sends correct request data with all parameters', async () => {
    client.request = jest.fn().mockResolvedValue({
      success: 'X',
      objects: [{ type: 'CLAS', name: 'ZCL_TEST' }],
      total: 1
    });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', 'CLAS,INTF', 'ZCL_*', 50, 10);

    expect(client.request).toHaveBeenCalled();
    const callArgs = client.request.mock.calls[0];
    expect(callArgs[0]).toBe('POST');
    expect(callArgs[1]).toBe('/list');
    expect(callArgs[2].package).toBe('$ZTEST');
    expect(callArgs[2].type).toBe('CLAS,INTF');
    expect(callArgs[2].name).toBe('ZCL_*');
    expect(callArgs[2].limit).toBe(50);
    expect(callArgs[2].offset).toBe(10);
  });

  test('list sends correct request with only package', async () => {
    client.request = jest.fn().mockResolvedValue({
      success: 'X',
      objects: []
    });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST');

    expect(client.request).toHaveBeenCalled();
    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].package).toBe('$ZTEST');
    expect(callArgs[2].limit).toBe(100);
    expect(callArgs[2].offset).toBe(0);
    expect(callArgs[2].type).toBeUndefined();
    expect(callArgs[2].name).toBeUndefined();
  });

  test('list clamps limit to valid range', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, null, 2000);
    await client.list('$ZTEST', null, null, 0);
    await client.list('$ZTEST', null, null, -5);

    // First call should clamp to 1000
    let callArgs = client.request.mock.calls[0];
    expect(callArgs[2].limit).toBe(1000);

    // Second call should clamp to 1
    callArgs = client.request.mock.calls[1];
    expect(callArgs[2].limit).toBe(1);

    // Third call should clamp to 1 (negative)
    callArgs = client.request.mock.calls[2];
    expect(callArgs[2].limit).toBe(1);
  });

  test('list clamps offset to non-negative', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, null, 100, -10);

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].offset).toBe(0);
  });

  test('list does not include type when null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null);

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].type).toBeUndefined();
  });

  test('list does not include name when null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, null);

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].name).toBeUndefined();
  });

  test('list fetches CSRF token before making request', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST');

    expect(client.fetchCsrfToken).toHaveBeenCalled();
    expect(client.request).toHaveBeenCalled();
  });

  test('list passes CSRF token to request', async () => {
    // Mock both fetchCsrfToken and request to verify token is passed
    client.fetchCsrfToken = jest.fn().mockImplementation(async function() {
      this.csrfToken = 'my-csrf-token';
      return 'my-csrf-token';
    });
    client.request = jest.fn().mockResolvedValue({ success: 'X' });

    await client.list('$ZTEST');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[3]).toEqual({ csrfToken: 'my-csrf-token' });
  });

  test('list handles complex type filter', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', 'CLAS,INTF,PROG,FUGR,DDLS');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].type).toBe('CLAS,INTF,PROG,FUGR,DDLS');
  });

  test('list handles wildcard name pattern', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, 'ZCL_ABAPGIT*');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].name).toBe('ZCL_ABAPGIT*');
  });

  test('list handles empty string type as null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', '');

    const callArgs = client.request.mock.calls[0];
    // Empty string is falsy, so it won't be included
    expect(callArgs[2].type).toBeUndefined();
  });

  test('list handles empty string name as null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, '');

    const callArgs = client.request.mock.calls[0];
    // Empty string is falsy, so it won't be included
    expect(callArgs[2].name).toBeUndefined();
  });

  test('list uses default values when parameters omitted', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].package).toBe('$ZTEST');
    expect(callArgs[2].limit).toBe(100);
    expect(callArgs[2].offset).toBe(0);
  });
});
