/**
 * Shared HTTP error formatting utilities.
 *
 * When ABAP returns HTTP 4xx/5xx the abap-http.js layer captures the raw
 * response body in `error.body`.  This module extracts a human-readable
 * detail line from that body and provides a consistent display helper used
 * by all command modules.
 */

/**
 * Extract a short, human-readable detail string from an HTTP error's response
 * body.  Handles the three common SAP response shapes:
 *  - JSON with a `message` or `error` field  (our own ABAP handlers)
 *  - SAP ICF XML/HTML error pages            (raw ICM 500 page)
 *  - Plain text
 *
 * @param {string|object} body  - raw response body (string or already-parsed object)
 * @returns {string|null}       - detail text, or null if nothing useful found
 */
function extractBodyDetail(body) {
  if (!body) return null;

  // Already a parsed object (e.g. CSRF error shape)
  if (typeof body === 'object') {
    return body.message || body.error || body.MESSAGE || body.ERROR || null;
  }

  // Try to parse as JSON first
  try {
    const parsed = JSON.parse(body);
    return parsed.message || parsed.error || parsed.MESSAGE || parsed.ERROR || null;
  } catch (_) {
    // Not JSON — fall through
  }

  // SAP ICF HTML/XML error page: grab the first <p> or <title> content
  const titleMatch = body.match(/<title[^>]*>([^<]+)<\/title>/i);
  if (titleMatch) return titleMatch[1].trim();

  const pMatch = body.match(/<p[^>]*>([^<]{10,})<\/p>/i);
  if (pMatch) return pMatch[1].trim();

  // Plain text — return first non-empty line (up to 200 chars)
  const firstLine = body.split('\n').map(l => l.trim()).find(l => l.length > 0);
  if (firstLine) return firstLine.substring(0, 200);

  return null;
}

/**
 * Build a display message for an HTTP error, including body detail when
 * available.
 *
 * @param {Error|object} error  - error object from abap-http.js
 * @returns {string}            - formatted message for console output
 */
function formatHttpError(error) {
  const base = error.message || String(error);
  const detail = extractBodyDetail(error.body);
  if (detail && detail !== base) {
    return `${base}\n     Detail: ${detail}`;
  }
  return base;
}

/**
 * Print a formatted HTTP error to stderr.
 * Optionally dump the full raw body when verbose mode is active.
 *
 * @param {Error|object} error    - error from abap-http.js
 * @param {object}       [opts]   - options
 * @param {boolean}      [opts.verbose=false]  - dump full raw body
 * @param {string}       [opts.prefix='❌ Error']  - prefix for first line
 */
function printHttpError(error, { verbose = false, prefix = '❌ Error' } = {}) {
  const msg = formatHttpError(error);
  console.error(`\n${prefix}: ${msg}`);

  if (verbose && error.body) {
    console.error('\n--- Raw response body ---');
    const raw = typeof error.body === 'object'
      ? JSON.stringify(error.body, null, 2)
      : String(error.body);
    console.error(raw);
    console.error('--- End of response body ---');
  }
}

module.exports = { formatHttpError, printHttpError, extractBodyDetail };
