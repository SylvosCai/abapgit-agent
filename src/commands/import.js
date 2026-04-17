/**
 * Import command - Import existing objects from package to git repository
 */

const {
  startBackgroundJob,
  pollForCompletion,
  displayProgress,
  formatTimestamp,
  calculateTimeSpent
} = require('../utils/backgroundJobPoller');

module.exports = {
  name: 'import',
  description: 'Import existing objects from package to git repository',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    try {
      const { loadConfig, gitUtils, AbapHttp, getSafeguards } = context;

      // Show help if requested
    const helpIndex = args.findIndex(a => a === '--help' || a === '-h');
    if (helpIndex !== -1) {
      console.log(`
Usage:
  abapgit-agent import [--message <message>]

Description:
  Import existing objects from package to git repository.
  Uses the git remote URL to find the abapGit online repository.

  This command runs asynchronously using a background job and displays
  real-time progress updates.

Prerequisites:
  - Run "abapgit-agent create" first or create repository in abapGit UI
  - Package must have objects to import

Options:
  --branch     Branch to push to. Auto-detected from current git branch if omitted.
  --message    Commit message (default: "feat: initial import from ABAP package <package>")

Examples:
  abapgit-agent import
  abapgit-agent import --message "Initial import from SAP"
  abapgit-agent import --branch main
  abapgit-agent import --branch feature/my-branch --message "Import objects"
`);
      return;
    }

    // Get parameters from config
    const safeguards = getSafeguards();

    // disableImport check — bypass if current user is in importAllowedUsers
    if (safeguards.disableImport) {
      const config0 = loadConfig();
      const currentUser = (config0.user || '').toUpperCase();
      const allowedUsers = safeguards.importAllowedUsers || [];
      const isAllowed = allowedUsers.length > 0 && allowedUsers.includes(currentUser);

      if (!isAllowed) {
        console.error('❌ Error: import command is disabled for this project\n');
        if (safeguards.reason) {
          console.error(`Reason: ${safeguards.reason}\n`);
        }
        if (allowedUsers.length > 0) {
          console.error(`Allowed users: ${allowedUsers.join(', ')}`);
        }
        console.error('The import command has been disabled in .abapgit-agent.json');
        console.error('Please contact the project maintainer to enable it.');
        process.exit(1);
      }
    }

    // Get parameters from config
    const config = loadConfig();
    const repoUrl = gitUtils.getRemoteUrl();

    if (!repoUrl) {
      console.error('Error: No git remote configured. Please configure a remote origin.');
      process.exit(1);
    }

    const messageArgIndex = args.indexOf('--message');
    let commitMessage = null;
    if (messageArgIndex !== -1 && messageArgIndex + 1 < args.length) {
      commitMessage = args[messageArgIndex + 1];
    }

    const branchArgIndex = args.indexOf('--branch');
    const branch = branchArgIndex !== -1 ? args[branchArgIndex + 1] : gitUtils.getBranch();

    if (safeguards.requireImportMessage && !commitMessage) {
      console.error('❌ Error: import requires a commit message for this project\n');
      console.error('Please provide one with:');
      console.error('  abapgit-agent import --message "Your descriptive message"\n');
      if (safeguards.reason) {
        console.error(`Reason: ${safeguards.reason}\n`);
      }
      process.exit(1);
    }

    console.log(`\n📦 Starting import job`);
    console.log(`   URL: ${repoUrl}`);
    console.log(`   Branch: ${branch}`);
    if (commitMessage) {
      console.log(`   Message: ${commitMessage}`);
    }

    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const data = {
      url: repoUrl,
      branch
    };

    if (commitMessage) {
      data.message = commitMessage;
    }

    if (config.gitUsername) {
      data.username = config.gitUsername;
    }

    if (config.gitPassword) {
      data.password = config.gitPassword;
    }

    // Step 1: Start the background job
    const endpoint = '/sap/bc/z_abapgit_agent/import';
    const jobInfo = await startBackgroundJob(http, endpoint, data, csrfToken);

    console.log(`✅ Job started: ${jobInfo.jobNumber}`);
    console.log('');

    // Step 2: Poll for completion with progress updates
    const finalResult = await pollForCompletion(http, endpoint, jobInfo.jobNumber, {
      pollInterval: 2000,
      maxAttempts: 300,
      onProgress: (progress, message) => {
        displayProgress(progress, message);
      }
    });

    // Step 3: Show final result
    console.log('\n');

    if (finalResult.status === 'completed' && finalResult.result) {
      // Parse result JSON string
      let resultData;
      try {
        if (typeof finalResult.result === 'string') {
          resultData = JSON.parse(finalResult.result);
        } else {
          resultData = finalResult.result;
        }
      } catch (e) {
        resultData = { filesStaged: 'unknown', commitMessage: commitMessage };
      }

      if (resultData.success !== 'X' && resultData.success !== true) {
        const errorMsg = resultData.error || resultData.ERROR || 'Unknown error';
        console.log(`❌ Import failed: ${errorMsg}`);
        process.exit(1);
      }

      console.log(`✅ Import completed successfully!`);
      console.log(`   Files staged: ${resultData.filesStaged || resultData.FILES_STAGED || resultData.files_staged || 'unknown'}`);
      console.log(`   Commit: ${resultData.commitMessage || resultData.COMMIT_MESSAGE || resultData.commit_message || commitMessage || 'Initial import'}`);
      console.log(``);

      // Calculate time spent
      if (finalResult.startedAt && finalResult.completedAt) {
        const timeSpent = calculateTimeSpent(finalResult.startedAt, finalResult.completedAt);
        console.log(`⏱️  Time spent: ${timeSpent}`);
      }

      console.log(`📈 Stats:`);
      console.log(`   Job number: ${jobInfo.jobNumber}`);
      if (finalResult.startedAt) {
        console.log(`   Started: ${formatTimestamp(finalResult.startedAt)}`);
      }
      if (finalResult.completedAt) {
        console.log(`   Completed: ${formatTimestamp(finalResult.completedAt)}`);
      }
    } else {
      console.log(`❌ Import failed`);
      console.log(`   Status: ${finalResult.status}`);
      process.exit(1);
    }
    } catch (error) {
      console.error('\n❌ Error during import:');
      console.error(`   ${error.message || error}`);
      if (error.response) {
        console.error(`   HTTP Status: ${error.response.status}`);
        console.error(`   Response: ${JSON.stringify(error.response.data)}`);
      }
      process.exit(1);
    }
  }
};
