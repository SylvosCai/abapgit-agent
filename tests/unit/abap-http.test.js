/**
 * Unit tests for AbapHttp error handling
 */

const { AbapHttp } = require('../../src/utils/abap-http');
const https = require('https');
const { EventEmitter } = require('events');

// Mock https module
jest.mock('https');

describe('AbapHttp Error Handling', () => {
  let abapHttp;
  let mockConfig;

  beforeEach(() => {
    mockConfig = {
      host: 'test.sap.com',
      sapport: 443,
      client: '100',
      user: 'TEST_USER',
      password: 'test_password',
      language: 'EN'
    };

    abapHttp = new AbapHttp(mockConfig);

    // Mock session methods to avoid file I/O
    abapHttp.loadSession = jest.fn();
    abapHttp.saveSession = jest.fn();
    abapHttp.clearSession = jest.fn();
    abapHttp.csrfToken = 'test-csrf-token';
    abapHttp.cookies = 'test-cookie';

    // Reset mocks
    jest.clearAllMocks();
  });

  describe('HTTP Status Code Handling', () => {
    it('should reject on 401 Unauthorized', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 401;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      // Trigger response end with empty body
      setImmediate(() => {
        mockResponse.emit('data', 'Unauthorized');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 401,
        isAuthError: true
      });
    });

    it('should reject on 403 Forbidden', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 403;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', 'Forbidden');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 403,
        isAuthError: true
      });
    });

    it('should reject on 500 Internal Server Error', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 500;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', '<html><body>Internal Server Error</body></html>');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 500,
        message: 'HTTP 500 error'
      });
    });

    it('should reject on 404 Not Found', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 404;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', 'Not Found');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 404,
        message: 'HTTP 404 error'
      });
    });
  });

  describe('JSON Parsing Error Handling', () => {
    it('should reject when JSON parsing fails', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 200;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', 'Invalid JSON response');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 200,
        message: 'Invalid response format (not JSON)'
      });
    });

    it('should reject when extracted JSON fails to parse', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 200;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', 'Some text { "invalid": json } more text');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        statusCode: 200,
        message: 'Failed to parse JSON response'
      });
    });

    it('should resolve with valid JSON response', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 200;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', '{"success": true, "data": "test"}');
        mockResponse.emit('end');
      });

      const result = await abapHttp._makeRequest('GET', '/test');
      expect(result).toEqual({ success: true, data: 'test' });
    });

    it('should resolve when extracting valid JSON from mixed content', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 200;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', 'Some text before {"success": true, "data": "test"} text after');
        mockResponse.emit('end');
      });

      const result = await abapHttp._makeRequest('GET', '/test');
      expect(result).toEqual({ success: true, data: 'test' });
    });
  });

  describe('Network Error Handling', () => {
    it('should reject on network error', async () => {
      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn(() => mockRequest);

      setImmediate(() => {
        mockRequest.emit('error', new Error('Network error'));
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toThrow('Network error');
    });
  });

  describe('CSRF and Session Error Detection', () => {
    it('should reject when response contains CSRF error', async () => {
      const mockResponse = new EventEmitter();
      mockResponse.statusCode = 200;
      mockResponse.headers = {};

      const mockRequest = new EventEmitter();
      mockRequest.end = jest.fn();
      mockRequest.write = jest.fn();

      https.Agent = jest.fn();
      https.request = jest.fn((options, callback) => {
        callback(mockResponse);
        return mockRequest;
      });

      setImmediate(() => {
        mockResponse.emit('data', '{"error": "CSRF token invalid", "message": "Token expired"}');
        mockResponse.emit('end');
      });

      await expect(abapHttp._makeRequest('GET', '/test')).rejects.toMatchObject({
        isAuthError: true,
        message: 'CSRF token or session error'
      });
    });
  });
});
