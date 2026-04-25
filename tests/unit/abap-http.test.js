'use strict';

/**
 * Unit tests for AbapHttp (axios-based)
 */

jest.mock('fs', () => ({
  existsSync: jest.fn(() => false),
  readFileSync: jest.fn(() => '{}'),
  writeFileSync: jest.fn(),
  unlinkSync: jest.fn()
}));

jest.mock('path', () => ({
  join: jest.fn((...args) => args.join('/'))
}));

jest.mock('os', () => ({
  tmpdir: jest.fn(() => '/tmp')
}));

const fs = require('fs');
const { AbapHttp } = require('../../src/utils/abap-http');

function makeConfig() {
  return { host: 'test.sap.com', sapport: 443, user: 'TEST_USER', password: 'test_password', client: '100', language: 'EN' };
}

/**
 * Mock the axios adapter to return a canned response.
 * Preserves interceptors (cookie merging, auth injection).
 */
function mockAxios(client, statusCode, headers, body) {
  return client._axios.defaults.adapter = jest.fn().mockResolvedValue({
    status: statusCode,
    statusText: statusCode < 400 ? 'OK' : 'Error',
    headers: headers || {},
    data: body || '',
  });
}

describe('AbapHttp Error Handling', () => {
  let abapHttp;

  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
    abapHttp = new AbapHttp(makeConfig());
    abapHttp.csrfToken = 'test-csrf-token';
    abapHttp.cookies = 'test-cookie';
  });

  describe('HTTP Status Code Handling', () => {
    it('should reject on 401 Unauthorized', async () => {
      mockAxios(abapHttp, 401, {}, 'Unauthorized');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 401,
        isAuthError: true
      });
    });

    it('should reject on 403 Forbidden', async () => {
      mockAxios(abapHttp, 403, {}, 'Forbidden');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 403,
        isAuthError: true
      });
    });

    it('should reject on 500 Internal Server Error', async () => {
      mockAxios(abapHttp, 500, {}, '<html><body>Internal Server Error</body></html>');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 500,
        message: expect.stringContaining('(HTTP 500)')
      });
    });

    it('should reject on 404 Not Found', async () => {
      mockAxios(abapHttp, 404, {}, 'Not Found');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 404,
        message: expect.stringContaining('(HTTP 404)')
      });
    });
  });

  describe('JSON Parsing', () => {
    it('should reject when JSON parsing fails', async () => {
      mockAxios(abapHttp, 200, {}, 'Invalid JSON response');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 200,
        message: 'Invalid response format (not JSON)'
      });
    });

    it('should reject when extracted JSON fails to parse', async () => {
      mockAxios(abapHttp, 200, {}, 'Some text { "invalid": json } more text');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 200,
        message: 'Failed to parse JSON response'
      });
    });

    it('should resolve with valid JSON response', async () => {
      mockAxios(abapHttp, 200, {}, '{"success": true, "data": "test"}');
      const result = await abapHttp._makeRequest('GET', '/test');
      expect(result).toEqual({ success: true, data: 'test' });
    });

    it('should resolve when extracting valid JSON from mixed content', async () => {
      mockAxios(abapHttp, 200, {}, 'Some text before {"success": true, "data": "test"} text after');
      const result = await abapHttp._makeRequest('GET', '/test');
      expect(result).toEqual({ success: true, data: 'test' });
    });
  });

  describe('CSRF and Session Error Detection', () => {
    it('should reject when response contains CSRF error', async () => {
      mockAxios(abapHttp, 200, {}, '{"error": "CSRF token invalid", "message": "Token expired"}');
      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        isAuthError: true,
        message: 'CSRF token or session error'
      });
    });

    it('should detect auth errors in error objects', () => {
      expect(abapHttp.isAuthError({ statusCode: 401 })).toBe(true);
      expect(abapHttp.isAuthError({ statusCode: 403 })).toBe(true);
      expect(abapHttp.isAuthError({ message: 'CSRF validation failed' })).toBe(true);
      expect(abapHttp.isAuthError({ message: 'Session expired' })).toBe(true);
      expect(abapHttp.isAuthError({ statusCode: 500, message: 'Server error' })).toBe(false);
    });
  });

  describe('Auth retry', () => {
    it('should retry once on auth error then propagate', async () => {
      let callCount = 0;
      abapHttp._axios.defaults.adapter = jest.fn().mockImplementation((config) => {
        callCount++;
        if (config.url === '/sap/bc/z_abapgit_agent/health') {
          // CSRF token fetch succeeds
          return Promise.resolve({ status: 200, headers: { 'x-csrf-token': 'new-token' }, data: '' });
        }
        // All other requests return 403
        return Promise.resolve({ status: 403, statusText: 'Forbidden', headers: {}, data: '' });
      });

      await expect(abapHttp.get('/test')).rejects.toMatchObject({
        statusCode: 403,
        isAuthError: true
      });
      // Should have made: 1st request (403) + CSRF fetch + retry (403) = 3 calls
      expect(callCount).toBe(3);
    });
  });
});

describe('AbapHttp HTTP protocol support', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
  });

  test('uses http:// base URL when protocol is "http"', () => {
    const config = { ...makeConfig(), sapport: 8000, protocol: 'http' };
    const client = new AbapHttp(config);
    expect(client._axios.defaults.baseURL).toBe('http://test.sap.com:8000');
  });
});

describe('AbapHttp session caching', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
  });

  test('loads cached session when file exists and not expired', () => {
    const session = { csrfToken: 'tok123', cookies: 'cookie=abc', expiresAt: Date.now() + 10 * 60 * 1000, savedAt: Date.now() };
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify(session));

    const client = new AbapHttp(makeConfig());
    expect(client.csrfToken).toBe('tok123');
    expect(client.cookies).toBe('cookie=abc');
  });

  test('clears session when file is expired', () => {
    const session = { csrfToken: 'old', cookies: 'x', expiresAt: Date.now() - 1000, savedAt: Date.now() - 20000 };
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify(session));

    const client = new AbapHttp(makeConfig());
    expect(client.csrfToken).toBeNull();
  });

  test('saves session to file after CSRF fetch', async () => {
    const client = new AbapHttp(makeConfig());
    mockAxios(client, 200, { 'x-csrf-token': 'new-tok' }, '');
    await client.fetchCsrfToken();
    expect(fs.writeFileSync).toHaveBeenCalled();
    expect(client.csrfToken).toBe('new-tok');
  });
});
