/**
 * Create command - Create abapGit online repository in ABAP system
 */

module.exports = {
  name: 'create',
  description: 'Create abapGit online repository in ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, gitUtils, AbapHttp } = context;

    // Show help if requested
    const helpIndex = args.findIndex(a => a === '--help' || a === '-h');
    if (helpIndex !== -1) {
      console.log(`
Usage:
  abapgit-agent create

Description:
  Create abapGit online repository in ABAP system.
  Auto-detects URL from git remote and package from .abapGitAgent.

Prerequisites:
  - Run "abapgit-agent init" first
  - Edit .abapGitAgent with credentials (host, user, password)

Examples:
  abapgit-agent create                    # Create repo in ABAP
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

    if (!config.package) {
      console.error('Error: Package not configured. Run "abapgit-agent init" first or set package in .abapGitAgent.');
      process.exit(1);
    }

    const branch = gitUtils.getBranch();

    // Extract repo name from git URL
    const repoName = repoUrl.split('/').pop().replace('.git', '');

    console.log(`\n🚀 Creating online repository`);
    console.log(`   URL: ${repoUrl}`);
    console.log(`   Package: ${config.package}`);
    console.log(`   Folder: ${config.folder || '/src/'}`);
    console.log(`   Name: ${repoName}`);
    console.log(`   Branch: ${branch}`);

    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const data = {
      url: repoUrl,
      package: config.package,
      name: repoName,
      branch: branch,
      folder: config.folder || '/src/'
    };

    if (config.gitUsername) {
      data.username = config.gitUsername;
    }

    if (config.gitPassword) {
      data.password = config.gitPassword;
    }

    const result = await http.post('/sap/bc/z_abapgit_agent/create', data, { csrfToken });

    console.log('\n');

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const repoKey = result.REPO_KEY || result.repo_key;
    const createdRepoName = result.REPO_NAME || result.repo_name;
    const message = result.MESSAGE || result.message;
    const error = result.ERROR || result.error;

    if (success === 'X' || success === true) {
      console.log(`✅ Repository created successfully!`);
      console.log(`   URL: ${repoUrl}`);
      console.log(`   Package: ${config.package}`);
      console.log(`   Name: ${createdRepoName || repoName}`);
    } else {
      console.log(`❌ Failed to create repository`);
      console.log(`   Error: ${error || message || 'Unknown error'}`);
      process.exit(1);
    }
  }
};
