/**
 * Health command - Check ABAP REST API health
 */

module.exports = {
  name: 'health',
  description: 'Check if ABAP REST API is healthy',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { config, AbapHttp } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent health

Description:
  Check if the ABAP REST API (z_abapgit_agent) is reachable and responding.
  Useful for verifying connectivity and configuration.
`);
      return;
    }

    try {
      const http = new AbapHttp(config);
      const result = await http.get('/sap/bc/z_abapgit_agent/health');
      console.log(JSON.stringify(result, null, 2));
      return result;
    } catch (error) {
      console.error(`Health check failed: ${error.message}`);
      process.exit(1);
    }
  }
};
