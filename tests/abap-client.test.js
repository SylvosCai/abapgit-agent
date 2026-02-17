/**
 * Unit tests for abap-client.js
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
    language: 'EN',
    gitUsername: 'gituser',
    gitPassword: 'gittoken'
  }))
}));

describe('ABAPClient', () => {
  let ABAPClient;
  let client;

  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();

    ABAPClient = require('../src/abap-client').ABAPClient;
    client = new ABAPClient();
  });

  describe('getConfig', () => {
    test('returns configuration object', () => {
      const config = client.getConfig();

      expect(config.baseUrl).toBe('https://test.sap.com:44300/sap/bc/z_abapgit_agent');
      expect(config.username).toBe('testuser');
      expect(config.password).toBe('testpass');
      expect(config.client).toBe('100');
      expect(config.language).toBe('EN');
    });

    test('caches configuration', () => {
      const config1 = client.getConfig();
      const config2 = client.getConfig();

      expect(config1).toBe(config2);
    });
  });

  describe('readNetscapeCookies', () => {
    test('returns empty string when cookie file does not exist', () => {
      mockExistsSync.mockReturnValue(false);

      const result = client.readNetscapeCookies();

      expect(result).toBe('');
    });

    test('parses Netscape cookie format correctly', () => {
      mockExistsSync.mockReturnValue(true);
      mockReadFileSync.mockReturnValue(
        '#HttpOnly_test.sap.com\tTRUE\t/\tFALSE\t0\tJSESSIONID\tabc123\n' +
        'test.sap.com\tTRUE\t/\tFALSE\t0\tSAP_SESSIONID\txyz789\n' +
        '\t\t\t\t\t\n' +
        '# comment line\n'
      );

      const result = client.readNetscapeCookies();

      expect(result).toBe('JSESSIONID=abc123; SAP_SESSIONID=xyz789');
    });

    test('skips empty lines and regular comment lines', () => {
      mockExistsSync.mockReturnValue(true);
      mockReadFileSync.mockReturnValue(
        '# regular comment\n' +
        '\t\t\t\t\t\n' +
        '#HttpOnly_test.sap.com\tTRUE\t/\tFALSE\t0\tCOOKIE1\tvalue1\n'
      );

      const result = client.readNetscapeCookies();

      expect(result).toBe('COOKIE1=value1');
    });
  });

  describe('request method', () => {
    test('request method exists and is callable', () => {
      expect(typeof client.request).toBe('function');
    });
  });

  describe('fetchCsrfToken method', () => {
    test('fetchCsrfToken method exists and is callable', () => {
      expect(typeof client.fetchCsrfToken).toBe('function');
    });
  });

  describe('healthCheck', () => {
    test('healthCheck method returns expected structure', async () => {
      // Mock the request method
      client.request = jest.fn().mockResolvedValue({ status: 'OK', version: '1.0.0' });

      const result = await client.healthCheck();

      // Result contains both the original 'OK' and added 'healthy'
      expect(result.abap).toBe('connected');
      expect(result.version).toBe('1.0.0');
    });

    test('healthCheck returns unhealthy on error', async () => {
      client.request = jest.fn().mockRejectedValue(new Error('Connection refused'));

      const result = await client.healthCheck();

      expect(result.status).toBe('unhealthy');
      expect(result.abap).toBe('disconnected');
    });
  });

  describe('pull method', () => {
    test('pull method includes transport_request in request data when specified', async () => {
      // Mock the request method to capture the data
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        message: 'Pull completed successfully',
        activated_count: 1,
        failed_count: 0
      });

      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.pull(
        'https://github.com/test/repo.git',
        'main',
        'gituser',
        'gittoken',
        ['zcl_test.clas.abap'],
        'DEVK900001'
      );

      // Verify request was called with transport_request
      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2]).toHaveProperty('transport_request');
      expect(callArgs[2].transport_request).toBe('DEVK900001');
    });

    test('pull method does not include transport_request when not specified', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        message: 'Pull completed successfully',
        activated_count: 1,
        failed_count: 0
      });

      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.pull(
        'https://github.com/test/repo.git',
        'main',
        'gituser',
        'gittoken',
        ['zcl_test.clas.abap']
      );

      // Verify request was called without transport_request
      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2]).not.toHaveProperty('transport_request');
    });
  });

  describe('create method', () => {
    test('create method exists and is callable', () => {
      expect(typeof client.create).toBe('function');
    });

    test('create sends correct request data', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        repo_key: 'REPO123',
        repo_name: 'test-repo'
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.create(
        'https://github.com/org/repo.git',
        'ZTEST_PACKAGE',
        'main',
        'test-repo'
      );

      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[0]).toBe('POST');
      expect(callArgs[1]).toBe('/create');
      expect(callArgs[2].url).toBe('https://github.com/org/repo.git');
      expect(callArgs[2].package).toBe('ZTEST_PACKAGE');
      expect(callArgs[2].branch).toBe('main');
      expect(callArgs[2].display_name).toBe('test-repo');
    });

    test('create includes name when specified', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.create(
        'https://github.com/org/repo.git',
        'ZTEST_PACKAGE',
        'main',
        'My Display',
        'my-repo-name'
      );

      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].display_name).toBe('My Display');
      expect(callArgs[2].name).toBe('my-repo-name');
    });

    test('create includes folder_logic when specified', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.create(
        'https://github.com/org/repo.git',
        'ZTEST_PACKAGE',
        'main',
        null,
        null,
        'FULL'
      );

      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].folder_logic).toBe('FULL');
    });
  });

  describe('import method', () => {
    test('import method exists and is callable', () => {
      expect(typeof client.import).toBe('function');
    });

    test('import sends correct request data', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        files_staged: '15'
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.import('https://github.com/org/repo.git');

      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[0]).toBe('POST');
      expect(callArgs[1]).toBe('/import');
      expect(callArgs[2].url).toBe('https://github.com/org/repo.git');
    });

    test('import includes custom message when specified', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.import('https://github.com/org/repo.git', 'Custom commit message');

      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].message).toBe('Custom commit message');
    });
  });

  describe('syntaxCheck method', () => {
    test('syntaxCheck method exists and is callable', () => {
      expect(typeof client.syntaxCheck).toBe('function');
    });

    test('syntaxCheck sends correct request data', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        object_type: 'CLAS',
        object_name: 'ZCL_TEST',
        error_count: 0,
        errors: []
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      const result = await client.syntaxCheck('CLAS', 'ZCL_TEST');

      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[0]).toBe('POST');
      expect(callArgs[1]).toBe('/syntax-check');
      expect(callArgs[2].object_type).toBe('CLAS');
      expect(callArgs[2].object_name).toBe('ZCL_TEST');
      expect(result.object_type).toBe('CLAS');
    });
  });

  describe('unitTest method', () => {
    test('unitTest method exists and is callable', () => {
      expect(typeof client.unitTest).toBe('function');
    });

    test('unitTest sends package in request', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        test_count: 5,
        passed_count: 5,
        failed_count: 0
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.unitTest('ZTEST_PACKAGE');

      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].package).toBe('ZTEST_PACKAGE');
    });

    test('unitTest sends objects array in request', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        test_count: 2
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      const objects = [
        { object_type: 'CLAS', object_name: 'ZCL_TEST1' },
        { object_type: 'CLAS', object_name: 'ZCL_TEST2' }
      ];

      await client.unitTest(null, objects);

      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].objects).toEqual(objects);
    });

    test('unitTest handles empty parameters', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.unitTest();

      expect(client.request).toHaveBeenCalled();
    });
  });

  describe('tree method', () => {
    test('tree method exists and is callable', () => {
      expect(typeof client.tree).toBe('function');
    });

    test('tree sends correct request data', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        hierarchy: []
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.tree('$ZTEST', 3, false);

      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[0]).toBe('POST');
      expect(callArgs[1]).toBe('/tree');
      expect(callArgs[2].package).toBe('$ZTEST');
      expect(callArgs[2].depth).toBe(3);
      expect(callArgs[2].include_objects).toBe(false);
    });

    test('tree clamps depth to valid range', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.tree('$ZTEST', 100, false);
      await client.tree('$ZTEST', 0, false);

      // First call should clamp to 10
      let callArgs = client.request.mock.calls[0];
      expect(callArgs[2].depth).toBe(10);

      // Second call should clamp to 1
      callArgs = client.request.mock.calls[1];
      expect(callArgs[2].depth).toBe(1);
    });

    test('tree includes objects when requested', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.tree('$ZTEST', 3, true);

      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].include_objects).toBe(true);
    });
  });

  describe('preview method', () => {
    test('preview method exists and is callable', () => {
      expect(typeof client.preview).toBe('function');
    });

    test('preview sends correct request data', async () => {
      client.request = jest.fn().mockResolvedValue({
        success: 'X',
        objects: []
      });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.preview(['SFLIGHT']);

      expect(client.request).toHaveBeenCalled();
      const callArgs = client.request.mock.calls[0];
      expect(callArgs[0]).toBe('POST');
      expect(callArgs[1]).toBe('/preview');
      expect(callArgs[2].objects).toEqual(['SFLIGHT']);
    });

    test('preview includes type when specified', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.preview(['ZC_VIEW'], 'DDLS');

      const callArgs = client.request.mock.calls[0];
      expect(callArgs[2].type).toBe('DDLS');
    });

    test('preview clamps limit to valid range', async () => {
      client.request = jest.fn().mockResolvedValue({ success: 'X' });
      client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

      await client.preview(['SFLIGHT'], null, 200);
      await client.preview(['SFLIGHT'], null, 0);

      // First call should clamp to 100
      let callArgs = client.request.mock.calls[0];
      expect(callArgs[2].limit).toBe(100);

      // Second call should clamp to 1
      callArgs = client.request.mock.calls[1];
      expect(callArgs[2].limit).toBe(1);
    });
  });

  describe('getClient singleton', () => {
    test('getClient returns singleton instance', () => {
      const { getClient } = require('../src/abap-client');
      const client1 = getClient();
      const client2 = getClient();

      expect(client1).toBe(client2);
    });
  });
});
