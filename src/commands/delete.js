/**
 * Delete command - Delete abapGit online repository from ABAP system
 */

module.exports = {
  name: 'delete',
  description: 'Delete abapGit online repository from ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, gitUtils, AbapHttp } = context;

    // Show help if requested
    const helpIndex = args.findIndex(a => a === '--help' || a === '-h');
    if (helpIndex !== -1) {
      console.log(`
Usage:
  abapgit-agent delete

Description:
  Delete abapGit online repository from ABAP system.
  Auto-detects URL from git remote of current directory.

Prerequisites:
  - Run "abapgit-agent create" first

Examples:
  abapgit-agent delete   # Delete repo for current git remote
`);
      return;
    }

    // Get URL from current git remote
    const config = loadConfig();
    const repoUrl = gitUtils.getRemoteUrl();

    if (!repoUrl) {
      console.error('Error: No git remote configured. Please configure a remote origin.');
      process.exit(1);
    }

    console.log(`\n🗑️  Deleting online repository`);
    console.log(`   URL: ${repoUrl}`);

    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const data = {
      url: repoUrl
    };

    const result = await http.post('/sap/bc/z_abapgit_agent/delete', data, { csrfToken });

    console.log('\n');

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const repoKey = result.REPO_KEY || result.repo_key;
    const message = result.MESSAGE || result.message;
    const error = result.ERROR || result.error;

    if (success === 'X' || success === true) {
      console.log(`✅ Repository deleted successfully!`);
      console.log(`   Key: ${repoKey}`);
    } else {
      console.log(`❌ Failed to delete repository`);
      console.log(`   Error: ${error || message || 'Unknown error'}`);
      process.exit(1);
    }
  }
};
