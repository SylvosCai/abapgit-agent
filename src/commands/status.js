/**
 * Status command - Check if ABAP integration is configured
 */

const pathModule = require('path');

module.exports = {
  name: 'status',
  description: 'Check if ABAP integration is configured for this repo',
  requiresAbapConfig: false,  // We check manually in execute
  requiresVersionCheck: false,

  async execute(args, context) {
    const { gitUtils, isAbapIntegrationEnabled, AbapHttp, loadConfig } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent status

Description:
  Check whether ABAP integration is configured for the current repository.
  Shows the config location, SAP host, and whether the abapGit online repository
  is registered in the ABAP system.
`);
      return;
    }

    if (isAbapIntegrationEnabled()) {
      console.log('✅ ABAP Git Agent is ENABLED');
      console.log('   Config location:', pathModule.join(process.cwd(), '.abapGitAgent'));

      // Check if repo exists in ABAP
      const config = loadConfig();
      const repoUrl = gitUtils.getRemoteUrl();

      if (repoUrl) {
        try {
          const http = new AbapHttp(config);
          const csrfToken = await http.fetchCsrfToken();
          const result = await http.post('/sap/bc/z_abapgit_agent/status', { url: repoUrl }, { csrfToken });

          const status = result.status || result.STATUS || result.SUCCESS;
          if (status === 'Found' || status === 'X' || status === true) {
            console.log('   Repository: ✅ Created');
            const pkg = result.package || result.PACKAGE || 'N/A';
            const key = result.key || result.KEY || result.REPO_KEY || result.repo_key || 'N/A';
            const transportRequired = result.transport_required === true || result.transport_required === 'true';
            console.log(`      Package: ${pkg}`);
            console.log(`      Transport required: ${transportRequired ? 'Yes' : 'No'}`);
            console.log(`      URL: ${repoUrl}`);
            console.log(`      Key: ${key}`);
          } else {
            console.log('   Repository: ❌ Not created');
            console.log('      Run "abapgit-agent create" to create it');
          }
        } catch (error) {
          console.log('   Repository: ❓ Unknown (could not check)');
          console.log(`      Error: ${error.message}`);
        }
      }
    } else {
      console.log('❌ ABAP Git Agent is NOT configured');
      console.log('   Run "abapgit-agent init" to set up configuration');
    }
  }
};
