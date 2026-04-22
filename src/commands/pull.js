/**
 * Pull command - Pull and activate ABAP objects from git repository
 */

const { printHttpError } = require('../utils/format-error');
const fs = require('fs');
const pathModule = require('path');
const { execSync } = require('child_process');

// Calculate display width accounting for emoji (2 cells) vs ASCII (1 cell)
function calcWidth(str) {
  if (!str) return 0;
  let width = 0;
  let i = 0;
  while (i < str.length) {
    const code = str.codePointAt(i);
    if (!code) break;
    // Variation selectors (FE00-FE0F) and ZWJ (200D) take 0 width
    if (code >= 0xFE00 && code <= 0xFE0F) { i += 1; continue; }
    if (code === 0x200D) { i += 1; continue; }  // Zero width joiner
    // Emoji and wide characters take 2 cells
    if (code > 0xFFFF) { width += 2; i += 2; }  // Skip surrogate pair
    else if (code > 127) { width += 2; i += 1; }
    else { width += 1; i += 1; }
  }
  return width;
}

// Pad string to display width
function padToWidth(str, width) {
  const s = str || '';
  const currentWidth = calcWidth(s);
  return s + ' '.repeat(Math.max(0, width - currentWidth));
}
module.exports = {
  name: 'pull',
  description: 'Pull and activate ABAP objects from git repository',
  requiresAbapConfig: true,
  requiresVersionCheck: true,
  _calcWidth: calcWidth,
  _padToWidth: padToWidth,

  async execute(args, context) {
    const { loadConfig, AbapHttp, gitUtils, getTransport, getSafeguards, getConflictSettings, getTransportSettings } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent pull [--url <git-url>] [--branch <branch>] [--files <file1,file2,...>]
                     [--transport <TRANSPORT>] [--conflict-mode <mode>] [--sync-xml]

Description:
  Pull and activate ABAP objects from a git repository into the ABAP system.
  Auto-detects git remote URL and branch from the current directory.

Parameters:
  --url <git-url>         Git repository URL. Auto-detected from git remote if omitted.
  --branch <branch>       Branch to pull from. Auto-detected from current branch if omitted.
  --files <file1,...>     Comma-separated list of files to pull (selective activation).
  --transport <TRANSPORT> Transport request (e.g. DEVK900001).
  --conflict-mode <mode>  Conflict handling: abort (default) or ignore.
  --sync-xml              Rewrite XML metadata files to match ABAP serializer output.
  --verbose               Show detailed activation output.

Examples:
  abapgit-agent pull
  abapgit-agent pull --branch develop
  abapgit-agent pull --files src/zcl_my_class.clas.abap
  abapgit-agent pull --files src/zcl_foo.clas.abap,src/zif_foo.intf.abap
  abapgit-agent pull --transport DEVK900001
  abapgit-agent pull --conflict-mode ignore
  abapgit-agent pull --files src/zcl_foo.clas.abap --sync-xml
`);
      return;
    }

    const verbose = args.includes('--verbose');
    const syncXml = args.includes('--sync-xml');

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
    const conflictModeArgIndex = args.indexOf('--conflict-mode');
    const jsonOutput = args.includes('--json');

    // Auto-detect git remote URL if not provided
    let gitUrl = urlArgIndex !== -1 ? args[urlArgIndex + 1] : null;
    let branch = branchArgIndex !== -1 ? args[branchArgIndex + 1] : gitUtils.getBranch();
    let files = null;
    let conflictMode = conflictModeArgIndex !== -1 ? args[conflictModeArgIndex + 1] : getConflictSettings().mode;

    // Transport: CLI arg takes priority, then config/environment, then selector
    let transportRequest = null;
    if (transportArgIndex !== -1 && transportArgIndex + 1 < args.length) {
      // Explicit --transport argument
      transportRequest = args[transportArgIndex + 1];
    } else {
      // Fall back to config or environment variable
      transportRequest = getTransport();
    }

    // Auto-detect URL early (needed for transport_required check)
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

    if (filesArgIndex !== -1 && filesArgIndex + 1 < args.length) {
      files = args[filesArgIndex + 1].split(',').map(f => f.trim());

      // Validate that every file has a recognised extension:
      // - .abap / .asddls  — ABAP source files
      // - name.type.xml    — XML-only objects (TABL, STRU, DTEL, TTYP, ...)
      //   Must have exactly 3 dot-separated parts with a non-empty name part
      //   to exclude .abapgit.xml, package.devc.xml, plain .xml, etc.
      const ABAP_SOURCE_EXTS = new Set(['abap', 'asddls']);
      const isXmlOnlyObject = (f) => {
        const base = f.split('/').pop();
        const parts = base.split('.');
        return parts.length === 3 && parts[0].length > 0 && parts[2].toLowerCase() === 'xml';
      };
      const nonSourceFiles = files.filter(f => {
        const base = f.split('/').pop(); // strip directory
        const parts = base.split('.');
        const ext = parts[parts.length - 1].toLowerCase();
        if (ABAP_SOURCE_EXTS.has(ext)) return false;  // .abap / .asddls — valid
        if (isXmlOnlyObject(f)) return false;          // name.type.xml — valid
        return true;                                   // everything else — invalid
      });
      if (nonSourceFiles.length > 0) {
        console.error('❌ Error: --files only accepts ABAP source files (.abap, .asddls) or XML-only object files (name.type.xml).');
        console.error('   The following file(s) are not recognised:');
        nonSourceFiles.forEach(f => console.error(`     ${f}`));
        console.error('   Tip: for source objects pass the .abap file (e.g. zcl_foo.clas.abap or zcl_foo.enho.28bbfe2f.abap);');
        console.error('   for XML-only objects (TABL, STRU, DTEL, TTYP) or enhancement metadata,');
        console.error('   pass the .xml file, e.g. --files abap/ztable.tabl.xml or abap/zfoo.enho.xml');
        process.exit(1);
      }

      // Validate that every specified file exists on disk.
      // Skip when --url is explicit: the files belong to a different repository
      // and won't be present in the current working directory.
      if (urlArgIndex === -1) {
        const fs = require('fs');
        const missingFiles = files.filter(f => !fs.existsSync(f));
        if (missingFiles.length > 0) {
          console.error('❌ Error: the following file(s) do not exist:');
          missingFiles.forEach(f => console.error(`     ${f}`));
          console.error('   Check the path and try again.');
          process.exit(1);
        }
      }
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

    // Auto-select transport when none configured and not in JSON mode
    if (!transportRequest && !jsonOutput) {
      const { selectTransport, isNonInteractive, _getTransportHookConfig } = require('../utils/transport-selector');

      // Check repository status — confirms the repo is registered and tells us
      // whether a transport is required. If the check fails, abort: pulling into
      // an uninitialised repository does not make sense.
      let transportRequired = false;
      let statusResult;
      try {
        const config = loadConfig();
        const http = new AbapHttp(config);
        const csrfToken = await http.fetchCsrfToken();
        statusResult = await http.post('/sap/bc/z_abapgit_agent/status', { url: gitUrl }, { csrfToken });
      } catch (e) {
        const isNetworkError = e.code && ['ENOTFOUND', 'ECONNREFUSED', 'ETIMEDOUT', 'ECONNRESET'].includes(e.code);
        console.error(`❌ Repository status check failed: ${e.message}`);
        if (isNetworkError) {
          console.error('   Cannot reach the ABAP system. Check your network connection and the host in .abapGitAgent.');
        } else {
          console.error('   Make sure the repository is registered with abapgit-agent (run "abapgit-agent create").');
        }
        process.exit(1);
      }

      if (!statusResult || statusResult.status === 'Not found') {
        console.error(`❌ Repository not found in ABAP system: ${gitUrl}`);
        console.error('   Run "abapgit-agent create" to register it first.');
        process.exit(1);
      }

      if (statusResult.transport_required === true || statusResult.transport_required === 'true') {
        transportRequired = true;
      }

      if (transportRequired) {
        const config = loadConfig();
        const http = new AbapHttp(config);
        transportRequest = await selectTransport(config, http, loadConfig, AbapHttp, getTransportSettings);

        // If a hook was configured but returned no transport, handle based on context
        if (transportRequest === null) {
          const hookConfig = _getTransportHookConfig();
          if (hookConfig && hookConfig.hook) {
            if (isNonInteractive()) {
              // Non-interactive (AI/CI): fail — a configured hook must return a transport
              console.error('❌ Error: transport hook returned no transport request.');
              console.error(`   Hook: ${hookConfig.hook}`);
              if (hookConfig.description) console.error(`   ${hookConfig.description}`);
              process.exit(1);
            } else {
              // Interactive (TTY): warn and fall through to the picker
              process.stderr.write(`⚠️  Transport hook returned no transport request (${hookConfig.hook}).\n`);
              process.stderr.write('   Please select one manually:\n');
              const { interactivePicker } = require('../utils/transport-selector');
              transportRequest = await interactivePicker(http);
            }
          }
        }
      }
    }

    await this.pull(gitUrl, branch, files, transportRequest, loadConfig, AbapHttp, jsonOutput, undefined, conflictMode, verbose, syncXml);
  },

  async pull(gitUrl, branch = 'main', files = null, transportRequest = null, loadConfig, AbapHttp, jsonOutput = false, gitCredentials = undefined, conflictMode = 'abort', verbose = false, syncXml = false, isRepull = false) {
    const TERM_WIDTH = process.stdout.columns || 80;

    if (!jsonOutput && !isRepull) {
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
      // gitCredentials=null means no credentials (public repo); undefined means use config defaults
      const resolvedCredentials = gitCredentials === undefined
        ? { username: config.gitUsername, password: config.gitPassword }
        : gitCredentials;

      const data = {
        url: gitUrl,
        branch: branch,
        conflict_mode: conflictMode,
        ...(resolvedCredentials ? { username: resolvedCredentials.username, password: resolvedCredentials.password } : {})
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

      // Detect missing .abapgit.xml — without it abapGit's stored starting_folder
      // may not match the actual source folder, causing pull to silently return ACTIVATED_COUNT=0
      const missingAbapgitXml = fs.existsSync(pathModule.join(process.cwd(), '.git')) &&
          !fs.existsSync(pathModule.join(process.cwd(), '.abapgit.xml'));

      // JSON output mode
      if (jsonOutput) {
        if (missingAbapgitXml) {
          result.missing_abapgit_xml = true;
        }
        console.log(JSON.stringify(result, null, 2));
        return result;
      }

      if (missingAbapgitXml) {
        console.warn('⚠️  .abapgit.xml not found in repository root.');
        console.warn('   Pull may return ACTIVATED_COUNT=0 if the ABAP-side starting_folder is wrong.');
        console.warn('   Run: abapgit-agent init --package <PACKAGE> to create it.\n');
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
      const activatedCount = result.ACTIVATED_COUNT || result.activated_count || 0;
      const failedCount = result.FAILED_COUNT || result.failed_count || 0;
      const logMessages = result.LOG_MESSAGES || result.log_messages || [];
      const activatedObjects = result.ACTIVATED_OBJECTS || result.activated_objects || [];
      const failedObjects = result.FAILED_OBJECTS || result.failed_objects || [];
      const conflictReport = result.CONFLICT_REPORT || result.conflict_report || '';
      const conflictCount = result.CONFLICT_COUNT || result.conflict_count || 0;

      // --- Conflict report (pull was aborted) ---
      if (conflictCount > 0 && conflictReport) {
        console.error(`⚠️  Pull aborted — ${conflictCount} conflict(s) detected\n`);
        console.error('─'.repeat(TERM_WIDTH));
        console.error(conflictReport.replace(/\\n/g, '\n'));
        const err = new Error(message || `Pull aborted — ${conflictCount} conflict(s) detected`);
        err._isPullError = true;
        throw err;
      }

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


      if (success === 'X' || success === true) {
        console.log(`✅ Pull completed successfully!`);
        console.log(`   Message: ${message || 'N/A'}`);
        if (activatedCount === 0 && files && !isRepull) {
          console.warn(`⚠️  ACTIVATED_COUNT: 0 — no objects were activated. Check for unpushed commits: git log origin/<branch>..HEAD`);
        }
      } else if (failedCount === 0 && failedObjects.length === 0 &&
                 activatedCount === 0 && logMessages.length === 0 &&
                 (!message || /activation cancelled|nothing to activate|already active/i.test(message))) {
        // abapGit returns SUCCESS='' with "Activation cancelled" when there are
        // no inactive objects to activate — the object is already active and consistent.
        console.log(`✅ Pull completed — object already active, nothing to activate.`);
        console.log(`   Message: ${message || 'N/A'}`);
      } else {
        console.error(`❌ Pull completed with errors!`);
        console.error(`   Message: ${message || 'N/A'}`);
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

      // Throw if pull was not successful so callers (e.g. upgrade) can detect failure.
      // Exception: SUCCESS='' with no failures and no log = object already active, not a real error.
      const alreadyActive = failedCount === 0 && failedObjects.length === 0 &&
                            activatedCount === 0 && logMessages.length === 0 &&
                            (!message || /activation cancelled|nothing to activate|already active/i.test(message));
      if (success !== 'X' && success !== true && !alreadyActive) {
        const err = new Error(message || 'Pull completed with errors');
        err._isPullError = true;
        throw err;
      }

      // --- Post-pull XML sync ---
      // abapGit's status calculation already identified which XML files differ
      // (match=false) — only those are returned in local_xml_files.
      const localXmlFiles = result.local_xml_files || result.LOCAL_XML_FILES || [];

      if (localXmlFiles.length > 0) {
        const diffFiles = [];
        for (const f of localXmlFiles) {
          const relPath = ((f.path || f.PATH || '') + (f.filename || f.FILENAME || '')).replace(/^\//, '');
          const absPath = pathModule.join(process.cwd(), relPath);
          if (!fs.existsSync(absPath)) continue;
          const incoming = Buffer.from(f.data || f.DATA, 'base64');
          // Double-check: only write if bytes actually differ (guard against encoding quirks)
          const current = fs.readFileSync(absPath);
          if (!current.equals(incoming)) {
            diffFiles.push({ relPath, absPath, incoming });
          }
        }

        if (diffFiles.length > 0 && !syncXml) {
          console.log(`\n⚠️  ${diffFiles.length} XML file(s) differ from serializer output:`);
          for (const f of diffFiles) console.log(`   ${f.relPath}`);
          console.log(`   Run with --sync-xml to accept serializer output and amend the last commit`);
        } else if (diffFiles.length > 0 && syncXml) {
          console.log(`\n🔄 Syncing ${diffFiles.length} XML file(s) to match serializer output:`);
          for (const f of diffFiles) console.log(`   ${f.relPath}`);

          // 1. Write serializer XML to disk
          for (const f of diffFiles) fs.writeFileSync(f.absPath, f.incoming);

          // 2. Stage changed XML files
          const quotedPaths = diffFiles.map(f => `"${f.relPath}"`).join(' ');
          execSync(`git add ${quotedPaths}`, { cwd: process.cwd() });

          // 3. Capture pre-amend SHA before amending (used for --force-with-lease below)
          const preAmendSha = execSync('git rev-parse HEAD', { cwd: process.cwd(), stdio: 'pipe' }).toString().trim();

          // 3b. Check whether origin/<branch> already exists on the remote.
          //     Use git ls-remote rather than rev-parse origin/<branch> because the local
          //     remote-tracking ref may not exist even when the branch is on the remote
          //     (e.g. after a push that only wrote branch config but not refs/remotes/).
          let remoteRefExists = false;
          try {
            const lsOut = execSync(`git ls-remote origin "refs/heads/${branch}"`, { cwd: process.cwd(), stdio: 'pipe' }).toString().trim();
            remoteRefExists = lsOut.length > 0;
          } catch (_) { /* cannot reach remote — treat as new branch */ }

          // 4. Amend last commit
          execSync('git commit --amend --no-edit', { cwd: process.cwd() });

          // 5. Push with force-with-lease using the pre-amend SHA as the expected remote value.
          //    Using --force-with-lease=<refname>:<sha> tells git: "the remote must have exactly
          //    <preAmendSha> — if it does, replace it with our amended commit".
          //    Plain --force-with-lease (no sha) re-checks the tracking ref which was just updated
          //    by git fetch, making it always fail after an amend.
          let pushed = false;
          try {
            // Retry the push up to 3 times on transient server errors (e.g. GitHub Enterprise 500)
            let pushErr;
            for (let attempt = 1; attempt <= 3; attempt++) {
              try {
                // If origin/<branch> exists: use --force-with-lease=branch:preAmendSha so we only
                // overwrite if the remote still has the pre-amend commit (safe concurrent guard).
                // If origin/<branch> doesn't exist yet: plain push with --set-upstream (no lease needed).
                const pushCmd = remoteRefExists
                  ? `git push --set-upstream origin ${branch} --force-with-lease=${branch}:${preAmendSha}`
                  : `git push --set-upstream origin ${branch}`;
                execSync(pushCmd, { cwd: process.cwd(), stdio: 'pipe' });
                pushed = true;
                break;
              } catch (err) {
                pushErr = err;
                const msg = (err.stderr || err.stdout || err.message || '').toString();
                const transient = /internal server error|remote rejected|\b5\d\d\b|connection reset|timed? ?out/i.test(msg);
                if (attempt < 3 && transient) {
                  execSync('sleep 2', { stdio: 'pipe' });
                  continue;
                }
                throw err;
              }
            }
          } catch (pushErr) {
            // Any push error (no remote, auth failure, etc.) → skip silently
          }

          if (pushed) {
            console.log(`   Re-pulling so ABAP system matches the amended commit...`);
            // 5. Re-pull so ABAP system matches the amended commit (no sync loop)
            await this.pull(gitUrl, branch, files, transportRequest, loadConfig, AbapHttp, jsonOutput, gitCredentials, conflictMode, verbose, false, true);
            console.log(`\n✅ Synced ${diffFiles.length} XML file(s), amended commit, re-pulled`);
          } else {
            // No remote at all — files are written and committed locally
            console.log(`\n✅ Synced ${diffFiles.length} XML file(s), amended commit`);
            console.log(`   Push skipped (no remote). Push manually to sync with remote.`);
          }
        }
      }

      return result;
    } catch (error) {
      if (error._isPullError) {
        throw error;
      }
      printHttpError(error, { verbose });
      process.exit(1);
    }
  }
};
