/**
 * Generic background job polling utility
 *
 * Provides reusable functions for commands that run as background jobs
 * with progress reporting.
 */

/**
 * Start a background job for a command
 *
 * @param {Object} http - AbapHttp instance
 * @param {string} endpoint - API endpoint (e.g., '/sap/bc/z_abapgit_agent/import')
 * @param {Object} data - Request data
 * @param {Object} csrfToken - CSRF token
 * @returns {Object} Job info with jobNumber, jobName
 * @throws {Error} If job failed to start
 */
async function startBackgroundJob(http, endpoint, data, csrfToken) {
  const result = await http.post(endpoint, data, { csrfToken });

  const success = result.SUCCESS || result.success;
  const jobNumber = result.JOB_NUMBER || result.jobNumber;
  const jobName = result.JOB_NAME || result.jobName;
  const error = result.ERROR || result.error;

  if (success !== 'X' && success !== true) {
    throw new Error(error || 'Failed to start background job');
  }

  return {
    jobNumber,
    jobName,
    status: result.STATUS || result.status
  };
}

/**
 * Poll for job completion with progress updates
 *
 * @param {Object} http - AbapHttp instance
 * @param {string} endpoint - Status endpoint (e.g., '/sap/bc/z_abapgit_agent/import')
 * @param {string} jobNumber - Job number to poll
 * @param {Object} options - Polling options
 * @param {number} options.pollInterval - Milliseconds between polls (default: 2000)
 * @param {number} options.maxAttempts - Max polling attempts (default: 300 = 10 minutes)
 * @param {Function} options.onProgress - Callback for progress updates (progress, message)
 * @returns {Object} Final job result
 * @throws {Error} If job fails or times out
 */
async function pollForCompletion(http, endpoint, jobNumber, options = {}) {
  const {
    pollInterval = 2000,
    maxAttempts = 300,  // 10 minutes with 2-second intervals
    onProgress = () => {}
  } = options;

  let status = 'running';
  let lastProgress = -1;
  let pollCount = 0;

  while (status === 'running' || status === 'scheduled' || status === 'STARTING') {
    // Wait before polling (except first time)
    if (pollCount > 0) {
      await sleep(pollInterval);
    }
    pollCount++;

    // Check timeout
    if (pollCount > maxAttempts) {
      throw new Error(`Job polling timeout after ${maxAttempts * pollInterval / 1000} seconds`);
    }

    // Get status
    const statusResult = await http.get(`${endpoint}?jobNumber=${jobNumber}`);

    status = statusResult.STATUS || statusResult.status;
    const progress = statusResult.PROGRESS || statusResult.progress || 0;
    const message = statusResult.MESSAGE || statusResult.message || '';
    const errorMessage = statusResult.ERROR_MESSAGE || statusResult.errorMessage;

    // Handle job not found
    if (statusResult.ERROR || statusResult.error) {
      throw new Error(statusResult.ERROR || statusResult.error);
    }

    // Show progress if changed
    if (progress !== lastProgress) {
      onProgress(progress, message);
      lastProgress = progress;
    }

    // Handle error status
    if (status === 'error') {
      throw new Error(errorMessage || 'Job failed with unknown error');
    }
  }

  // Get final result
  const finalResult = await http.get(`${endpoint}?jobNumber=${jobNumber}`);

  return {
    status: finalResult.STATUS || finalResult.status,
    result: finalResult.RESULT || finalResult.result,
    startedAt: finalResult.STARTED_AT || finalResult.startedAt,
    completedAt: finalResult.COMPLETED_AT || finalResult.completedAt,
    jobNumber: finalResult.JOB_NUMBER || finalResult.jobNumber,
    jobName: finalResult.JOB_NAME || finalResult.jobName
  };
}

/**
 * Display progress bar in console
 *
 * @param {number} progress - Progress percentage (0-100)
 * @param {string} message - Progress message
 * @param {Object} options - Display options
 * @param {number} options.barWidth - Width of progress bar (default: 30)
 * @param {boolean} options.showPercentage - Show percentage number (default: false)
 */
function displayProgress(progress, message, options = {}) {
  const {
    barWidth = 30,
    showPercentage = false
  } = options;

  const filled = Math.floor(progress * barWidth / 100);
  const empty = barWidth - filled;
  const bar = '[' + '='.repeat(filled) + ' '.repeat(empty) + ']';

  // Clear line and print progress
  let output = `\r${bar}`;
  if (showPercentage) {
    output += ` ${progress}%`;
  }
  output += ` ${message}`;

  process.stdout.write(output);
}

/**
 * Format ABAP timestamp (YYYYMMDDHHMMSS) to readable local format
 *
 * @param {string|number} timestamp - ABAP timestamp
 * @returns {string} Formatted timestamp (YYYY-MM-DD HH:MM:SS)
 */
function formatTimestamp(timestamp) {
  if (!timestamp) return timestamp;

  const ts = timestamp.toString();
  if (ts.length !== 14) return timestamp;

  const year = parseInt(ts.substr(0, 4));
  const month = parseInt(ts.substr(4, 2)) - 1; // JS months are 0-indexed
  const day = parseInt(ts.substr(6, 2));
  const hour = parseInt(ts.substr(8, 2));
  const minute = parseInt(ts.substr(10, 2));
  const second = parseInt(ts.substr(12, 2));

  // Create UTC date
  const utcDate = new Date(Date.UTC(year, month, day, hour, minute, second));

  // Format in local timezone
  const localYear = utcDate.getFullYear();
  const localMonth = String(utcDate.getMonth() + 1).padStart(2, '0');
  const localDay = String(utcDate.getDate()).padStart(2, '0');
  const localHour = String(utcDate.getHours()).padStart(2, '0');
  const localMinute = String(utcDate.getMinutes()).padStart(2, '0');
  const localSecond = String(utcDate.getSeconds()).padStart(2, '0');

  return `${localYear}-${localMonth}-${localDay} ${localHour}:${localMinute}:${localSecond}`;
}

/**
 * Calculate time difference between two ABAP timestamps
 *
 * @param {string|number} startTimestamp - Start timestamp (YYYYMMDDHHMMSS)
 * @param {string|number} endTimestamp - End timestamp (YYYYMMDDHHMMSS)
 * @returns {string} Human-readable duration (e.g., "2 minutes 30 seconds")
 */
function calculateTimeSpent(startTimestamp, endTimestamp) {
  if (!startTimestamp || !endTimestamp) return 'unknown';

  const start = parseAbapTimestamp(startTimestamp);
  const end = parseAbapTimestamp(endTimestamp);

  if (!start || !end) return 'unknown';

  const diffMs = end - start;
  const diffSeconds = Math.floor(diffMs / 1000);

  if (diffSeconds < 60) {
    return `${diffSeconds} second${diffSeconds !== 1 ? 's' : ''}`;
  }

  const minutes = Math.floor(diffSeconds / 60);
  const seconds = diffSeconds % 60;

  if (minutes < 60) {
    if (seconds === 0) {
      return `${minutes} minute${minutes !== 1 ? 's' : ''}`;
    }
    return `${minutes} minute${minutes !== 1 ? 's' : ''} ${seconds} second${seconds !== 1 ? 's' : ''}`;
  }

  const hours = Math.floor(minutes / 60);
  const remainingMinutes = minutes % 60;

  return `${hours} hour${hours !== 1 ? 's' : ''} ${remainingMinutes} minute${remainingMinutes !== 1 ? 's' : ''}`;
}

/**
 * Parse ABAP timestamp to JavaScript Date
 *
 * @param {string|number} timestamp - ABAP timestamp (YYYYMMDDHHMMSS)
 * @returns {Date|null} JavaScript Date object or null if invalid
 */
function parseAbapTimestamp(timestamp) {
  if (!timestamp) return null;

  const ts = timestamp.toString();
  if (ts.length !== 14) return null;

  const year = parseInt(ts.substr(0, 4));
  const month = parseInt(ts.substr(4, 2)) - 1;
  const day = parseInt(ts.substr(6, 2));
  const hour = parseInt(ts.substr(8, 2));
  const minute = parseInt(ts.substr(10, 2));
  const second = parseInt(ts.substr(12, 2));

  return new Date(Date.UTC(year, month, day, hour, minute, second));
}

/**
 * Sleep for specified milliseconds
 *
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise} Promise that resolves after ms milliseconds
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

module.exports = {
  startBackgroundJob,
  pollForCompletion,
  displayProgress,
  formatTimestamp,
  calculateTimeSpent,
  parseAbapTimestamp,
  sleep
};
