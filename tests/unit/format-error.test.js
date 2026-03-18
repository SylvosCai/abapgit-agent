/**
 * Unit tests for format-error.js
 * Tests extractBodyDetail(), formatHttpError(), printHttpError()
 */

const { extractBodyDetail, formatHttpError, printHttpError } = require('../../src/utils/format-error');

describe('extractBodyDetail()', () => {
  test('returns null for null/undefined/empty body', () => {
    expect(extractBodyDetail(null)).toBeNull();
    expect(extractBodyDetail(undefined)).toBeNull();
    expect(extractBodyDetail('')).toBeNull();
  });

  test('returns message from plain object with message field', () => {
    expect(extractBodyDetail({ message: 'Not authorized' })).toBe('Not authorized');
  });

  test('returns error from plain object with error field', () => {
    expect(extractBodyDetail({ error: 'Bad request' })).toBe('Bad request');
  });

  test('returns MESSAGE (uppercase) from plain object', () => {
    expect(extractBodyDetail({ MESSAGE: 'ABAP error' })).toBe('ABAP error');
  });

  test('returns ERROR (uppercase) from plain object', () => {
    expect(extractBodyDetail({ ERROR: 'ICM error' })).toBe('ICM error');
  });

  test('prefers message over error in plain object', () => {
    expect(extractBodyDetail({ message: 'primary', error: 'secondary' })).toBe('primary');
  });

  test('returns null for plain object with no known field', () => {
    expect(extractBodyDetail({ code: 404, status: 'not found' })).toBeNull();
  });

  test('extracts message from JSON string body', () => {
    const body = JSON.stringify({ message: 'Token expired' });
    expect(extractBodyDetail(body)).toBe('Token expired');
  });

  test('extracts error from JSON string body', () => {
    const body = JSON.stringify({ error: 'Unauthorized' });
    expect(extractBodyDetail(body)).toBe('Unauthorized');
  });

  test('extracts MESSAGE (uppercase) from JSON string body', () => {
    const body = JSON.stringify({ MESSAGE: 'ABAP exception' });
    expect(extractBodyDetail(body)).toBe('ABAP exception');
  });

  test('returns null for JSON string with no known field', () => {
    const body = JSON.stringify({ status: 500 });
    expect(extractBodyDetail(body)).toBeNull();
  });

  test('extracts title from SAP HTML error page', () => {
    const html = '<html><head><title>ICM Error: 403 Forbidden</title></head><body></body></html>';
    expect(extractBodyDetail(html)).toBe('ICM Error: 403 Forbidden');
  });

  test('extracts first <p> content from SAP HTML when no title', () => {
    const html = '<html><body><p>Service not active on this system</p></body></html>';
    expect(extractBodyDetail(html)).toBe('Service not active on this system');
  });

  test('ignores <p> with fewer than 10 characters', () => {
    const html = '<html><head><title>Error</title></head><body><p>short</p></body></html>';
    // title wins but let's also verify short <p> is skipped when there's no title
    const htmlNoTitle = '<html><body><p>short</p><p>This is a longer paragraph message</p></body></html>';
    expect(extractBodyDetail(htmlNoTitle)).toBe('This is a longer paragraph message');
  });

  test('returns first non-empty line of plain text (up to 200 chars)', () => {
    expect(extractBodyDetail('Connection refused')).toBe('Connection refused');
  });

  test('skips blank lines at start of plain text', () => {
    expect(extractBodyDetail('\n\nFirst real line')).toBe('First real line');
  });

  test('truncates plain text first line at 200 chars', () => {
    const longLine = 'X'.repeat(250);
    expect(extractBodyDetail(longLine)).toBe('X'.repeat(200));
  });
});

describe('formatHttpError()', () => {
  test('returns base message when no body', () => {
    const err = { message: 'HTTP 500' };
    expect(formatHttpError(err)).toBe('HTTP 500');
  });

  test('appends body detail when detail differs from base message', () => {
    const err = { message: 'HTTP 403', body: '{"message":"Token CSRF invalid"}' };
    expect(formatHttpError(err)).toBe('HTTP 403\n     Detail: Token CSRF invalid');
  });

  test('does not duplicate detail when detail equals base message', () => {
    const err = { message: 'Token CSRF invalid', body: '{"message":"Token CSRF invalid"}' };
    expect(formatHttpError(err)).toBe('Token CSRF invalid');
  });

  test('uses String(error) when message is missing', () => {
    const err = { toString: () => 'raw error string', body: null };
    const result = formatHttpError(err);
    expect(result).toContain('raw error string');
  });
});

describe('printHttpError()', () => {
  let errOutput;
  let origError;

  beforeEach(() => {
    errOutput = [];
    origError = console.error;
    console.error = (...args) => errOutput.push(args.join(' '));
  });

  afterEach(() => {
    console.error = origError;
  });

  test('prints formatted error to stderr with default prefix', () => {
    printHttpError({ message: 'HTTP 500', body: null });
    expect(errOutput.join('\n')).toMatch(/❌ Error: HTTP 500/);
  });

  test('uses custom prefix when provided', () => {
    printHttpError({ message: 'HTTP 401', body: null }, { prefix: '🔒 Auth error' });
    expect(errOutput.join('\n')).toMatch(/🔒 Auth error: HTTP 401/);
  });

  test('does not dump raw body by default (verbose=false)', () => {
    printHttpError({ message: 'HTTP 500', body: '{"message":"internal error"}' });
    const text = errOutput.join('\n');
    expect(text).not.toMatch(/Raw response body/);
    expect(text).not.toMatch(/internal error.*internal error/);
  });

  test('dumps raw string body when verbose=true', () => {
    printHttpError({ message: 'HTTP 500', body: 'raw text error' }, { verbose: true });
    const text = errOutput.join('\n');
    expect(text).toMatch(/Raw response body/);
    expect(text).toMatch(/raw text error/);
    expect(text).toMatch(/End of response body/);
  });

  test('dumps JSON-stringified body object when verbose=true', () => {
    printHttpError({ message: 'HTTP 500', body: { code: 500, detail: 'crash' } }, { verbose: true });
    const text = errOutput.join('\n');
    expect(text).toMatch(/Raw response body/);
    expect(text).toMatch(/"detail": "crash"/);
  });
});
