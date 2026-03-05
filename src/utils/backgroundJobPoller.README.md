# Background Job Polling Utility

Generic utility for commands that run as ABAP background jobs with progress reporting.

## Overview

This utility provides a reusable pattern for CLI commands that need to:
1. Start an ABAP background job
2. Poll for job status with real-time progress updates
3. Display progress bars and final results

## Usage

### Basic Example

```javascript
const {
  startBackgroundJob,
  pollForCompletion,
  displayProgress,
  formatTimestamp,
  calculateTimeSpent
} = require('../utils/backgroundJobPoller');

// In your command's execute() method:
async execute(args, context) {
  const { AbapHttp, loadConfig } = context;
  const config = loadConfig();
  const http = new AbapHttp(config);
  const csrfToken = await http.fetchCsrfToken();

  // 1. Start background job
  const endpoint = '/sap/bc/z_abapgit_agent/mycommand';
  const jobInfo = await startBackgroundJob(http, endpoint, data, csrfToken);

  console.log(`✅ Job started: ${jobInfo.jobNumber}`);

  // 2. Poll for completion
  const finalResult = await pollForCompletion(http, endpoint, jobInfo.jobNumber, {
    pollInterval: 2000,
    maxAttempts: 300,
    onProgress: (progress, message) => {
      displayProgress(progress, message);
    }
  });

  // 3. Show final result
  console.log('\n');
  if (finalResult.status === 'completed') {
    console.log(`✅ Command completed successfully!`);

    const timeSpent = calculateTimeSpent(finalResult.startedAt, finalResult.completedAt);
    console.log(`⏱️  Time spent: ${timeSpent}`);
    console.log(`   Started: ${formatTimestamp(finalResult.startedAt)}`);
    console.log(`   Completed: ${formatTimestamp(finalResult.completedAt)}`);
  }
}
```

## API Reference

### `startBackgroundJob(http, endpoint, data, csrfToken)`

Start a background job.

**Parameters:**
- `http` - AbapHttp instance
- `endpoint` - API endpoint (e.g., `/sap/bc/z_abapgit_agent/import`)
- `data` - Request data object
- `csrfToken` - CSRF token from `http.fetchCsrfToken()`

**Returns:** `{ jobNumber, jobName, status }`

**Throws:** Error if job failed to start

### `pollForCompletion(http, endpoint, jobNumber, options)`

Poll for job completion with progress updates.

**Parameters:**
- `http` - AbapHttp instance
- `endpoint` - Status endpoint (same as start endpoint)
- `jobNumber` - Job number from `startBackgroundJob()`
- `options` - Polling options:
  - `pollInterval` - Milliseconds between polls (default: 2000)
  - `maxAttempts` - Max polling attempts (default: 300 = 10 minutes)
  - `onProgress(progress, message)` - Callback for progress updates

**Returns:**
```javascript
{
  status: 'completed',
  result: '{"filesStaged": 100}',  // JSON string or object
  startedAt: '20260305103045',
  completedAt: '20260305103520',
  jobNumber: '12345678',
  jobName: 'IMPORT_20260305103045'
}
```

**Throws:** Error if job fails or times out

### `displayProgress(progress, message, options)`

Display progress bar in console.

**Parameters:**
- `progress` - Progress percentage (0-100)
- `message` - Progress message
- `options` - Display options:
  - `barWidth` - Width of progress bar (default: 30)
  - `showPercentage` - Show percentage number (default: false)

**Example Output:**
```
[===============               ] Staging files (1250 of 3701)
```

### `formatTimestamp(timestamp)`

Format ABAP timestamp to readable local format.

**Parameters:**
- `timestamp` - ABAP timestamp (YYYYMMDDHHMMSS)

**Returns:** Formatted timestamp (YYYY-MM-DD HH:MM:SS)

**Example:**
```javascript
formatTimestamp('20260305103045')
// => "2026-03-05 10:30:45"
```

### `calculateTimeSpent(startTimestamp, endTimestamp)`

Calculate time difference between two ABAP timestamps.

**Parameters:**
- `startTimestamp` - Start timestamp (YYYYMMDDHHMMSS)
- `endTimestamp` - End timestamp (YYYYMMDDHHMMSS)

**Returns:** Human-readable duration

**Examples:**
```javascript
calculateTimeSpent('20260305103000', '20260305103015')
// => "15 seconds"

calculateTimeSpent('20260305103000', '20260305103145')
// => "1 minute 45 seconds"

calculateTimeSpent('20260305103000', '20260305123015')
// => "2 hours 0 minutes"
```

## ABAP Requirements

For a command to use this utility, the ABAP side must:

1. **Implement `zif_abgagt_progressable` interface** on the command class:
   ```abap
   CLASS zcl_abgagt_command_mycommand DEFINITION.
     PUBLIC SECTION.
       INTERFACES zif_abgagt_command.
       INTERFACES zif_abgagt_progressable.  " ← Add this
   ENDCLASS.
   ```

2. **Raise progress events** during execution:
   ```abap
   METHOD zif_abgagt_progressable~execute_with_progress.
     " Update progress
     RAISE EVENT zif_abgagt_progressable~progress_update
       EXPORTING
         iv_stage    = 'PROCESSING'
         iv_message  = 'Processing items'
         iv_progress = 50
         iv_current  = 500
         iv_total    = 1000.
   ENDMETHOD.
   ```

3. **Return structured result** in JSON format:
   ```abap
   " Final result
   ls_result = VALUE #(
     success = abap_true
     items_processed = 1000
     message = 'Processing completed'
   ).
   ```

The background job infrastructure will automatically:
- ✅ Detect the `progressable` interface
- ✅ Schedule the command as a background job
- ✅ Listen to progress events and update status
- ✅ Return HTTP 202 Accepted with job number
- ✅ Support status polling via GET endpoint

## Example Commands Using This Utility

- **import** - Import objects from ABAP package to git (already implemented)
- **pull** - Could be enhanced to support async execution for large file sets
- **inspect** - Could be enhanced for package-wide inspection

## Testing

Unit tests are available in `tests/unit/backgroundJobPoller.test.js`:

```bash
npm test -- tests/unit/backgroundJobPoller.test.js
```

## See Also

- [Background Job Architecture](../../docs/architecture/README.md) - Complete architecture documentation
- [Import Command](../commands/import.js) - Reference implementation
- [API Documentation](../../API.md) - REST API for async commands
