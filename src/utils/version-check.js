/**
 * CLI <> ABAP version compatibility check
 */
const pathModule = require('path');
const fs = require('fs');
const https = require('https');

/**
 * Get CLI version from package.json
 * @returns {string} CLI version or '1.0.0' as default
 */
function getCliVersion() {
  const packageJsonPath = pathModule.join(__dirname, '..', '..', 'package.json');
  if (fs.existsSync(packageJsonPath)) {
    const pkg = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
    return pkg.version || '1.0.0';
  }
  return '1.0.0';
}

/**
 * Check version compatibility between CLI and ABAP API
 * @param {object} config - ABAP connection config
 * @returns {Promise<object>} Version compatibility result
 */
async function checkCompatibility(config) {
  const cliVersion = getCliVersion();

  try {
    const url = new URL(`/sap/bc/z_abapgit_agent/health`, `https://${config.host}:${config.sapport}`);

    return new Promise((resolve) => {
      const options = {
        hostname: url.hostname,
        port: url.port,
        path: url.pathname,
        method: 'GET',
        headers: {
          'Authorization': `Basic ${Buffer.from(`${config.user}:${config.password}`).toString('base64')}`,
          'sap-client': config.client,
          'sap-language': config.language || 'EN',
          'Content-Type': 'application/json'
        },
        agent: new https.Agent({ rejectUnauthorized: false })
      };

      const req = https.request(options, (res) => {
        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          try {
            const result = JSON.parse(body);
            const apiVersion = result.version || '1.0.0';

            if (cliVersion !== apiVersion) {
              console.log(`\n⚠️  Version mismatch: CLI ${cliVersion}, ABAP API ${apiVersion}`);
              console.log('   Some commands may not work correctly.');
              console.log('   Update ABAP code: abapgit-agent pull\n');
            }
            resolve({ cliVersion, apiVersion, compatible: cliVersion === apiVersion });
          } catch (e) {
            resolve({ cliVersion, apiVersion: null, compatible: false, error: e.message });
          }
        });
      });

      req.on('error', (e) => {
        resolve({ cliVersion, apiVersion: null, compatible: false, error: e.message });
      });
      req.end();
    });
  } catch (error) {
    return { cliVersion, apiVersion: null, compatible: false, error: error.message };
  }
}

module.exports = {
  getCliVersion,
  checkCompatibility
};
