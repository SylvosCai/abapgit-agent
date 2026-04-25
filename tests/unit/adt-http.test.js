'use strict';

/**
 * Unit tests for AdtHttp (axios-based)
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
const { AdtHttp } = require('../../src/utils/adt-http');

function makeConfig() {
  return { host: 'test.sap.com', sapport: 443, user: 'TESTUSER', password: 'pass', client: '100', language: 'EN' };
}

/**
 * Mock the axios instance's adapter to return a canned response.
 * This preserves interceptors (cookie merging, auth injection).
 */
function mockAxios(adt, statusCode, headers, body) {
  const mockAdapter = jest.fn().mockResolvedValue({
    status: statusCode,
    statusText: statusCode < 400 ? 'OK' : 'Error',
    headers: headers || {},
    data: body || '',
  });
  adt._axios.defaults.adapter = mockAdapter;
  return mockAdapter;
}

// ─── extractXmlAttr ──────────────────────────────────────────────────────────

describe('AdtHttp.extractXmlAttr', () => {
  test('extracts text content of a tag', () => {
    const xml = '<adtcore:uri>/sap/bc/adt/breakpoints/BP001</adtcore:uri>';
    expect(AdtHttp.extractXmlAttr(xml, 'adtcore:uri', null)).toBe('/sap/bc/adt/breakpoints/BP001');
  });

  test('extracts attribute value from tag', () => {
    const xml = '<entry id="BP001" name="test">';
    expect(AdtHttp.extractXmlAttr(xml, 'entry', 'id')).toBe('BP001');
  });

  test('returns null when tag not found', () => {
    const xml = '<foo>bar</foo>';
    expect(AdtHttp.extractXmlAttr(xml, 'missing', null)).toBeNull();
  });
});

// ─── extractXmlAll ───────────────────────────────────────────────────────────

describe('AdtHttp.extractXmlAll', () => {
  test('extracts multiple text content occurrences', () => {
    const xml = '<name>Alice</name><name>Bob</name>';
    expect(AdtHttp.extractXmlAll(xml, 'name', null)).toEqual(['Alice', 'Bob']);
  });

  test('returns empty array when tag not found', () => {
    expect(AdtHttp.extractXmlAll('<foo/>', 'bar', null)).toEqual([]);
  });
});

// ─── Session caching ─────────────────────────────────────────────────────────

describe('AdtHttp session caching', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
  });

  test('loads cached session when file exists and not expired', () => {
    const session = { csrfToken: 'tok123', cookies: 'SAP_SESSIONID=abc', expiresAt: Date.now() + 10 * 60 * 1000, savedAt: Date.now() };
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify(session));

    const adt = new AdtHttp(makeConfig());
    expect(adt.csrfToken).toBe('tok123');
    expect(adt.cookies).toBe('SAP_SESSIONID=abc');
  });

  test('clears session when file is expired', () => {
    const session = { csrfToken: 'old', cookies: 'x', expiresAt: Date.now() - 1000, savedAt: Date.now() - 20000 };
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify(session));

    const adt = new AdtHttp(makeConfig());
    expect(adt.csrfToken).toBeNull();
  });

  test('clears session when file is corrupted', () => {
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue('NOT JSON');

    const adt = new AdtHttp(makeConfig());
    expect(adt.csrfToken).toBeNull();
  });
});

// ─── fetchCsrfToken ──────────────────────────────────────────────────────────

describe('AdtHttp.fetchCsrfToken', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
  });

  test('extracts CSRF token from response headers', async () => {
    const adt = new AdtHttp(makeConfig());
    mockAxios(adt, 200, { 'x-csrf-token': 'mytoken', 'set-cookie': ['SAP_SESSIONID=xyz; Path=/'] }, '<?xml version="1.0"?>');

    const token = await adt.fetchCsrfToken();
    expect(token).toBe('mytoken');
    expect(adt.csrfToken).toBe('mytoken');
  });

  test('saves cookies from set-cookie header', async () => {
    const adt = new AdtHttp(makeConfig());
    mockAxios(adt, 200, { 'x-csrf-token': 'tok', 'set-cookie': ['SAP_SESSIONID=abc; Path=/', 'sap-usercontext=xyz; Path=/'] }, '');

    await adt.fetchCsrfToken();
    expect(adt.cookies).toContain('SAP_SESSIONID=abc');
    expect(adt.cookies).toContain('sap-usercontext=xyz');
  });

  test('saves session to file after token fetch', async () => {
    const adt = new AdtHttp(makeConfig());
    mockAxios(adt, 200, { 'x-csrf-token': 'tok' }, '');

    await adt.fetchCsrfToken();
    expect(fs.writeFileSync).toHaveBeenCalled();
  });
});

// ─── _makeRequest error handling ─────────────────────────────────────────────

describe('AdtHttp._makeRequest error handling', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
  });

  test('rejects with ADT-specific message on 404', async () => {
    const adt = new AdtHttp(makeConfig());
    mockAxios(adt, 404, {}, '<html>Not Found</html>');

    await expect(adt.get('/sap/bc/adt/debugger/breakpoints')).rejects.toMatchObject({
      statusCode: 404,
      message: expect.stringContaining('404')
    });
  });

  test('rejects with auth message on 403', async () => {
    const adt = new AdtHttp(makeConfig());
    // First call: 403 → triggers CSRF refresh → fetchCsrfToken (200) → retry → 403 again
    let callCount = 0;
    adt._axios.defaults.adapter = jest.fn().mockImplementation((config) => {
      callCount++;
      if (config.url === '/sap/bc/adt/discovery') {
        // CSRF token fetch
        return Promise.resolve({ status: 200, headers: { 'x-csrf-token': 'new' }, data: '' });
      }
      // Both first and retry call return 403
      return Promise.resolve({ status: 403, statusText: 'Forbidden', headers: {}, data: '' });
    });

    await expect(adt.get('/sap/bc/adt/debugger/breakpoints')).rejects.toMatchObject({
      statusCode: 403,
      message: expect.stringContaining('S_ADT_RES')
    });
  });

  test('successful GET returns body, headers, statusCode', async () => {
    const adt = new AdtHttp(makeConfig());
    mockAxios(adt, 200, { 'content-type': 'application/atom+xml' }, '<feed/>');

    const result = await adt.get('/sap/bc/adt/debugger/breakpoints');
    expect(result.statusCode).toBe(200);
    expect(result.body).toBe('<feed/>');
    expect(result.headers).toHaveProperty('content-type');
  });
});

// ─── Protocol: HTTP support ───────────────────────────────────────────────────

describe('AdtHttp HTTP protocol support', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    fs.existsSync.mockReturnValue(false);
  });

  test('uses http:// base URL when protocol is "http"', () => {
    const config = { ...makeConfig(), sapport: 8000, protocol: 'http' };
    const adt = new AdtHttp(config);
    // The axios instance baseURL should use http://
    expect(adt._axios.defaults.baseURL).toBe('http://test.sap.com:8000');
  });
});
