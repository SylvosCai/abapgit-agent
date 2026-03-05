/**
 * Pull command - Pull and activate ABAP objects from git repository
 */

module.exports = {
  name: 'pull',
  description: 'Pull and activate ABAP objects from git repository',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp, gitUtils, getTransport, getSafeguards } = context;

    // Check project-level safeguards
    const safeguards = getSafeguards();

    // SAFEGUARD 1: Check if pull is completely disabled
    if (safeguards.disablePull) {
      console.error('❌ Error: pull command is disabled for this project\n');
      if (safeguards.reason) {
        console.error(`Reason: ${safeguards.reason}\n`);
      }
      console.error('The pull command has been disabled in .abapgit-agent.json');
      console.error('Please contact the project maintainer to enable it.');
      process.exit(1);
    }

    const urlArgIndex = args.indexOf('--url');
    const branchArgIndex = args.indexOf('--branch');
    const filesArgIndex = args.indexOf('--files');
    const transportArgIndex = args.indexOf('--transport');
    const jsonOutput = args.includes('--json');

    // Auto-detect git remote URL if not provided
    let gitUrl = urlArgIndex !== -1 ? args[urlArgIndex + 1] : null;
    let branch = branchArgIndex !== -1 ? args[branchArgIndex + 1] : gitUtils.getBranch();
    let files = null;

    // Transport: CLI arg takes priority, then config/environment, then null
    let transportRequest = null;
    if (transportArgIndex !== -1 && transportArgIndex + 1 < args.length) {
      // Explicit --transport argument
      transportRequest = args[transportArgIndex + 1];
    } else {
      // Fall back to config or environment variable
      transportRequest = getTransport();
    }

    if (filesArgIndex !== -1 && filesArgIndex + 1 < args.length) {
      files = args[filesArgIndex + 1].split(',').map(f => f.trim());
    }

    // SAFEGUARD 2: Check if files are required but not provided
    if (safeguards.requireFilesForPull && !files) {
      console.error('❌ Error: --files parameter is required for this project\n');
      if (safeguards.reason) {
        console.error(`Reason: ${safeguards.reason}\n`);
      }
      console.error('Usage: abapgit-agent pull --files <file1>,<file2>\n');
      console.error('This safeguard is configured in .abapgit-agent.json');
      console.error('Contact the project maintainer if you need to change this setting.');
      process.exit(1);
    }

    if (!gitUrl) {
      gitUrl = gitUtils.getRemoteUrl();
      if (!gitUrl) {
        console.error('Error: Not in a git repository or no remote configured.');
        console.error('Either run from a git repo, or specify --url <git-url>');
        process.exit(1);
      }
      if (!jsonOutput) {
        console.log(`📌 Auto-detected git remote: ${gitUrl}`);
      }
    }

    await this.pull(gitUrl, branch, files, transportRequest, loadConfig, AbapHttp, jsonOutput);
  },

  async pull(gitUrl, branch = 'main', files = null, transportRequest = null, loadConfig, AbapHttp, jsonOutput = false) {
    const TERM_WIDTH = process.stdout.columns || 80;

    if (!jsonOutput) {
      console.log(`\n🚀 Starting pull for: ${gitUrl}`);
      console.log(`   Branch: ${branch}`);
      if (files && files.length > 0) {
        console.log(`   Files: ${files.join(', ')}`);
      }
      if (transportRequest) {
        console.log(`   Transport Request: ${transportRequest}`);
      }
    }

    try {
      const config = loadConfig();

      // Fetch CSRF token first
      const http = new AbapHttp(config);
      const csrfToken = await http.fetchCsrfToken();

      // Prepare request data with git credentials
      const data = {
        url: gitUrl,
        branch: branch,
        username: config.gitUsername,
        password: config.gitPassword
      };

      // Add files array if specified
      if (files && files.length > 0) {
        data.files = files;
      }

      // Add transport request if specified
      if (transportRequest) {
        data.transport_request = transportRequest;
      }

      const result = await http.post('/sap/bc/z_abapgit_agent/pull', data, { csrfToken });

      // JSON output mode
      if (jsonOutput) {
        console.log(JSON.stringify(result, null, 2));
        return result;
      }

      console.log('\n');

      // Display raw result for debugging
      if (process.env.DEBUG) {
        console.log('Raw result:', JSON.stringify(result, null, 2));
      }

      // Handle uppercase keys from ABAP
      const success = result.SUCCESS || result.success;
      const jobId = result.JOB_ID || result.job_id;
      const message = result.MESSAGE || result.message;
      const errorDetail = result.ERROR_DETAIL || result.error_detail;
      const transportRequestUsed = result.TRANSPORT_REQUEST || result.transport_request;
      const activatedCount = result.ACTIVATED_COUNT || result.activated_count || 0;
      const failedCount = result.FAILED_COUNT || result.failed_count || 0;
      const logMessages = result.LOG_MESSAGES || result.log_messages || [];
      const activatedObjects = result.ACTIVATED_OBJECTS || result.activated_objects || [];
      const failedObjects = result.FAILED_OBJECTS || result.failed_objects || [];

      // Icon mapping for message types
      const getIcon = (type) => {
        const icons = {
          'S': '✅',  // Success
          'E': '❌',  // Error
          'W': '⚠️',  // Warning
          'A': '🛑'   // Abort
        };
        return icons[type] || '';
      };

      // Calculate display width accounting for emoji (2 cells) vs ASCII (1 cell)
      const calcWidth = (str) => {
        if (!str) return 0;
        let width = 0;
        let i = 0;
        while (i < str.length) {
          const code = str.codePointAt(i);
          if (!code) break;
          // Variation selectors (FE00-FE0F) and ZWJ (200D) take 0 width
          if (code >= 0xFE00 && code <= 0xFE0F) {
            i += 1;
            continue;
          }
          if (code === 0x200D) {  // Zero width joiner
            i += 1;
            continue;
          }
          // Emoji and wide characters take 2 cells
          if (code > 0xFFFF) {
            width += 2;
            i += 2;  // Skip surrogate pair
          } else if (code > 127) {
            width += 2;
            i += 1;
          } else {
            width += 1;
            i += 1;
          }
        }
        return width;
      };

      // Pad string to display width
      const padToWidth = (str, width) => {
        const s = str || '';
        const currentWidth = calcWidth(s);
        return s + ' '.repeat(Math.max(0, width - currentWidth));
      };

      if (success === 'X' || success === true) {
        console.log(`✅ Pull completed successfully!`);
        console.log(`   Job ID: ${jobId || 'N/A'}`);
        console.log(`   Message: ${message || 'N/A'}`);
      } else {
        console.log(`❌ Pull completed with errors!`);
        console.log(`   Job ID: ${jobId || 'N/A'}`);
        console.log(`   Message: ${message || 'N/A'}`);
      }

      // Display error detail if available
      if (errorDetail && errorDetail.trim()) {
        console.log(`\n📋 Error Details:`);
        console.log('─'.repeat(TERM_WIDTH));
        // Handle escaped newlines from ABAP JSON
        const formattedDetail = errorDetail.replace(/\\n/g, '\n');
        console.log(formattedDetail);
      }

      // Display all messages as table (from log_messages)
      if (logMessages && logMessages.length > 0) {
        console.log(`\n📋 Pull Log (${logMessages.length} messages):`);

        // Calculate column widths based on terminal width
        const tableWidth = Math.min(TERM_WIDTH, 120);
        const iconCol = 4;  // Fixed width for icon column
        const objCol = 28;
        const msgCol = tableWidth - iconCol - objCol - 6; // Account for vertical lines (3 chars)

        const headerLine = '─'.repeat(iconCol) + '┼' + '─'.repeat(objCol) + '┼' + '─'.repeat(msgCol);
        const headerText = padToWidth('Icon', iconCol) + '│' + padToWidth('Object', objCol) + '│' + 'Message'.substring(0, msgCol);
        const borderLine = '─'.repeat(tableWidth);

        console.log(borderLine);
        console.log(headerText);
        console.log(headerLine);

        for (const msg of logMessages) {
          const icon = getIcon(msg.TYPE);
          const objType = msg.OBJ_TYPE || '';
          const objName = msg.OBJ_NAME || '';
          const obj = objType && objName ? `${objType} ${objName}` : '';
          const text = msg.TEXT || '';

          // Truncate long text
          const displayText = text.length > msgCol ? text.substring(0, msgCol - 3) + '...' : text;

          console.log(padToWidth(icon, iconCol) + '│' + padToWidth(obj.substring(0, objCol), objCol) + '│' + displayText);
        }
      }

      // Extract objects with errors from log messages (type 'E' = Error)
      const failedObjectsFromLog = [];
      const objectsWithErrors = new Set();

      for (const msg of logMessages) {
        if (msg.TYPE === 'E' && msg.OBJ_TYPE && msg.OBJ_NAME) {
          const objKey = `${msg.OBJ_TYPE} ${msg.OBJ_NAME}`;
          objectsWithErrors.add(objKey);
          failedObjectsFromLog.push({
            OBJ_TYPE: msg.OBJ_TYPE,
            OBJ_NAME: msg.OBJ_NAME,
            TEXT: msg.TEXT || 'Activation failed',
            EXCEPTION: msg.EXCEPTION || ''
          });
        }
      }

      // Filter activated objects - only include objects without errors
      const filteredActivatedObjects = activatedObjects.filter(obj => {
        const objKey = obj.OBJ_TYPE && obj.OBJ_NAME ? `${obj.OBJ_TYPE} ${obj.OBJ_NAME}` : '';
        return objKey && !objectsWithErrors.has(objKey);
      });

      // Display unique activated objects (excluding objects with errors)
      if (filteredActivatedObjects && filteredActivatedObjects.length > 0) {
        console.log(`\n📦 Activated Objects (${filteredActivatedObjects.length}):`);
        console.log('─'.repeat(TERM_WIDTH));
        for (const obj of filteredActivatedObjects) {
          console.log(`✅ ${obj.OBJ_TYPE} ${obj.OBJ_NAME}`);
        }
      }

      // Display failed objects log (all error messages, duplicates allowed)
      if (failedObjectsFromLog.length > 0) {
        console.log(`\n❌ Failed Objects Log (${failedObjectsFromLog.length} entries):`);
        console.log('─'.repeat(TERM_WIDTH));
        for (const obj of failedObjectsFromLog) {
          const objKey = obj.OBJ_TYPE && obj.OBJ_NAME ? `${obj.OBJ_TYPE} ${obj.OBJ_NAME}` : '';
          let text = obj.TEXT || 'Activation failed';
          // Include exception text if available
          if (obj.EXCEPTION && obj.EXCEPTION.trim()) {
            text = `${text}\nException: ${obj.EXCEPTION}`;
          }
          console.log(`❌ ${objKey}: ${text}`);
        }
      } else if (failedObjects && failedObjects.length > 0) {
        // Fallback to API failed_objects if no errors in log
        console.log(`\n❌ Failed Objects Log (${failedObjects.length}):`);
        console.log('─'.repeat(TERM_WIDTH));
        for (const obj of failedObjects) {
          const objKey = obj.OBJ_TYPE && obj.OBJ_NAME ? `${obj.OBJ_TYPE} ${obj.OBJ_NAME}` : '';
          let text = obj.TEXT || 'Activation failed';
          // Include exception text if available
          if (obj.EXCEPTION && obj.EXCEPTION.trim()) {
            text = `${text}\nException: ${obj.EXCEPTION}`;
          }
          console.log(`❌ ${objKey}: ${text}`);
        }
      } else if (failedCount > 0) {
        console.log(`\n❌ Failed Objects Log (${failedCount})`);
      }

      return result;
    } catch (error) {
      console.error(`\n❌ Error: ${error.message}`);
      process.exit(1);
    }
  }
};
