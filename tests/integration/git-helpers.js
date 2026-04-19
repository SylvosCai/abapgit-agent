/**
 * Shared git helper for integration tests.
 *
 * `gitExec(cmd, opts)` — runs a git command and retries on transient server
 * errors (HTTP 5xx, connection reset, timeout).  All integration-test runners
 * should use this instead of bare execSync for any network git operation
 * (clone, fetch, push) so that retry logic lives in one place.
 */
const { execSync } = require('child_process');

const MAX_ATTEMPTS  = 3;
const RETRY_DELAY_MS = 2000;

/** Patterns that indicate a transient server-side error worth retrying. */
const TRANSIENT_PATTERNS = [
  /\b5\d\d\b/,              // HTTP 5xx numeric code (500, 503 …)
  /internal server error/i,  // git server rejection without numeric code
  /remote rejected/i,        // push rejected by remote (often transient)
  /connection reset/i,
  /timed? ?out/i,
  /unable to access/i,
  /could not read/i,
  /early EOF/i,
];

function isTransient(err) {
  const msg = (err.stderr || err.stdout || err.message || '').toString();
  return TRANSIENT_PATTERNS.some(p => p.test(msg));
}

/**
 * Run a shell command, retrying up to MAX_ATTEMPTS times on transient errors.
 *
 * @param {string} cmd   - shell command to execute
 * @param {Object} [opts] - execSync options (cwd, encoding, timeout, …)
 * @returns {string} stdout (same as execSync)
 * @throws  last error if all attempts fail
 */
function gitExec(cmd, opts = {}) {
  let lastErr;
  for (let attempt = 1; attempt <= MAX_ATTEMPTS; attempt++) {
    try {
      return execSync(cmd, { encoding: 'utf8', ...opts });
    } catch (err) {
      lastErr = err;
      if (attempt < MAX_ATTEMPTS && isTransient(err)) {
        // brief back-off before retry
        execSync(`sleep ${RETRY_DELAY_MS / 1000}`);
        continue;
      }
      throw err;
    }
  }
  throw lastErr;
}

module.exports = { gitExec };
