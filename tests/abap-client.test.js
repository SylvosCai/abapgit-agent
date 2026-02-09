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
});
