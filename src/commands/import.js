/**
 * Import command - Import existing objects from package to git repository
 */

module.exports = {
  name: 'import',
  description: 'Import existing objects from package to git repository',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, gitUtils, AbapHttp } = context;

    // Show help if requested
    const helpIndex = args.findIndex(a => a === '--help' || a === '-h');
    if (helpIndex !== -1) {
      console.log(`
Usage:
  abapgit-agent import [--message <message>]

Description:
  Import existing objects from package to git repository.
  Uses the git remote URL to find the abapGit online repository.

Prerequisites:
  - Run "abapgit-agent create" first or create repository in abapGit UI
  - Package must have objects to import

Options:
  --message    Commit message (default: "feat: initial import from ABAP package <package>")

Examples:
  abapgit-agent import
  abapgit-agent import --message "Initial import from SAP"
`);
      return;
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

    console.log(`\n📦 Importing objects to git repository`);
    console.log(`   URL: ${repoUrl}`);
    if (commitMessage) {
      console.log(`   Message: ${commitMessage}`);
    }

    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const data = {
      url: repoUrl
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

    const result = await http.post('/sap/bc/z_abapgit_agent/import', data, { csrfToken });

    console.log('\n');

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const filesStaged = result.FILES_STAGED || result.files_staged;
    const abapCommitMessage = result.COMMIT_MESSAGE || result.commit_message;
    const error = result.ERROR || result.error;

    if (success === 'X' || success === true) {
      console.log(`✅ Objects imported successfully!`);
      console.log(`   Files staged: ${filesStaged}`);
      console.log(`   Commit: ${commitMessage || abapCommitMessage}`);
    } else {
      console.log(`❌ Import failed`);
      console.log(`   Error: ${error || 'Unknown error'}`);
      process.exit(1);
    }
  }
};
