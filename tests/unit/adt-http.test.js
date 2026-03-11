'use strict';

/**
 * Unit tests for AdtHttp
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

jest.mock('https', () => {
  const Agent = jest.fn().mockImplementation(() => ({}));
  return { Agent, request: jest.fn() };
});

jest.mock('http', () => ({
  request: jest.fn()
}));

const fs = require('fs');
const https = require('https');
const http = require('http');
const { AdtHttp } = require('../../src/utils/adt-http');

function makeConfig() {
  return { host: 'test.sap.com', sapport: 443, user: 'TESTUSER', password: 'pass', client: '100', language: 'EN' };
}

function mockRequest(statusCode, headers, body) {
  const EventEmitter = require('events');
  const res = new EventEmitter();
  res.statusCode = statusCode;
  res.headers = headers;

  const req = new EventEmitter();
  req.write = jest.fn();
  req.end = jest.fn(() => {
    process.nextTick(() => {
      https.request.mock.calls[https.request.mock.calls.length - 1][1](res);
      process.nextTick(() => {
        res.emit('data', body);
        res.emit('end');
      });
    });
  });

  https.request.mockReturnValueOnce(req);
  return { res, req };
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
    mockRequest(200, { 'x-csrf-token': 'mytoken', 'set-cookie': ['SAP_SESSIONID=xyz; Path=/'] }, '<?xml version="1.0"?>');

    const adt = new AdtHttp(makeConfig());
    const token = await adt.fetchCsrfToken();
    expect(token).toBe('mytoken');
    expect(adt.csrfToken).toBe('mytoken');
  });

  test('saves cookies from set-cookie header', async () => {
    mockRequest(200, { 'x-csrf-token': 'tok', 'set-cookie': ['SAP_SESSIONID=abc; Path=/', 'sap-usercontext=xyz; Path=/'] }, '');

    const adt = new AdtHttp(makeConfig());
    await adt.fetchCsrfToken();
    expect(adt.cookies).toContain('SAP_SESSIONID=abc');
    expect(adt.cookies).toContain('sap-usercontext=xyz');
  });

  test('saves session to file after token fetch', async () => {
    mockRequest(200, { 'x-csrf-token': 'tok' }, '');

    const adt = new AdtHttp(makeConfig());
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
    const EventEmitter = require('events');
    const res = new EventEmitter();
    res.statusCode = 404;
    res.headers = {};
    let capturedCallback;
    const req = new EventEmitter();
    req.write = jest.fn();
    req.end = jest.fn(() => {
      process.nextTick(() => {
        capturedCallback(res);
        // 404 handler reads body before rejecting — emit end so promise resolves
        process.nextTick(() => res.emit('end'));
      });
    });
    https.request.mockImplementationOnce((_opts, cb) => {
      capturedCallback = cb;
      return req;
    });

    const adt = new AdtHttp(makeConfig());
    await expect(adt.get('/sap/bc/adt/debugger/breakpoints')).rejects.toMatchObject({
      statusCode: 404,
      message: expect.stringContaining('404')
    });
  });

  test('rejects with auth message on 403', async () => {
    const EventEmitter = require('events');

    // Helper: queue up a mock https.request that immediately delivers statusCode
    // and optionally a response body (emitted via data+end events)
    function queueRequest(statusCode, body) {
      const res = new EventEmitter();
      res.statusCode = statusCode;
      res.headers = {};
      let cb;
      const req = new EventEmitter();
      req.write = jest.fn();
      req.end = jest.fn(() => {
        process.nextTick(() => {
          if (!cb) return;
          cb(res);
          if (body !== undefined) {
            process.nextTick(() => {
              res.emit('data', body);
              res.emit('end');
            });
          }
        });
      });
      https.request.mockImplementationOnce((_opts, c) => { cb = c; return req; });
    }

    // Call sequence: adt.get() → _makeRequest → 403
    // → retry: fetchCsrfToken (needs a successful response) → _makeRequest again → 403
    // → isRetry=true so propagates
    queueRequest(403);                               // first GET attempt
    queueRequest(200, '<xml/>');                     // fetchCsrfToken GET
    queueRequest(403);                               // second GET (retry, isRetry=true)

    const adt = new AdtHttp(makeConfig());
    await expect(adt.get('/sap/bc/adt/debugger/breakpoints')).rejects.toMatchObject({
      statusCode: 403,
      message: expect.stringContaining('S_ADT_RES')
    });
  });

  test('successful GET returns body, headers, statusCode', async () => {
    mockRequest(200, { 'content-type': 'application/atom+xml' }, '<feed/>');

    const adt = new AdtHttp(makeConfig());
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

  test('uses http:// base URL and http.request when protocol is "http"', async () => {
    const EventEmitter = require('events');
    const res = new EventEmitter();
    res.statusCode = 200;
    res.headers = { 'content-type': 'application/atom+xml' };

    const req = new EventEmitter();
    req.write = jest.fn();
    req.end = jest.fn(() => {
      process.nextTick(() => {
        http.request.mock.calls[http.request.mock.calls.length - 1][1](res);
        process.nextTick(() => {
          res.emit('data', '<feed/>');
          res.emit('end');
        });
      });
    });
    http.request.mockReturnValueOnce(req);

    const config = { ...makeConfig(), sapport: 8000, protocol: 'http' };
    const adt = new AdtHttp(config);
    const result = await adt.get('/sap/bc/adt/discovery');

    expect(result.statusCode).toBe(200);
    // http.request should have been called (not https.request)
    expect(http.request).toHaveBeenCalled();
    expect(https.request).not.toHaveBeenCalled();
    // Verify URL uses http://
    const callArgs = http.request.mock.calls[0][0];
    expect(callArgs.port).toBe('8000');
  });
});
