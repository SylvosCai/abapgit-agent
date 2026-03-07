/**
 * CLI <> ABAP version compatibility check
 */
const pathModule = require('path');
const fs = require('fs');
const os = require('os');
const https = require('https');

/**
 * Get cache directory for abapgit-agent
 * Follows OS-specific conventions:
 * - Windows: %LOCALAPPDATA%\abapgit-agent\cache
 * - Linux/macOS: ~/.cache/abapgit-agent (respects XDG_CACHE_HOME)
 * @returns {string} Cache directory path
 */
function getCacheDir() {
  const homeDir = os.homedir();

  // Windows: %LOCALAPPDATA%\abapgit-agent\cache
  if (process.platform === 'win32') {
    const localAppData = process.env.LOCALAPPDATA || pathModule.join(homeDir, 'AppData', 'Local');
    return pathModule.join(localAppData, 'abapgit-agent', 'cache');
  }

  // Linux/macOS: respect XDG_CACHE_HOME or use ~/.cache
  const xdgCache = process.env.XDG_CACHE_HOME || pathModule.join(homeDir, '.cache');
  return pathModule.join(xdgCache, 'abapgit-agent');
}

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
              console.error(`\n⚠️  Version mismatch: CLI ${cliVersion}, ABAP API ${apiVersion}`);
              console.error('   Some commands may not work correctly.');
              console.error('   Update ABAP code: abapgit-agent upgrade --match\n');
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

/**
 * Get configured npm registry URL
 * Respects user's npm config (e.g., corporate mirrors, regional mirrors)
 * @returns {string} Registry URL
 */
function getNpmRegistry() {
  try {
    const { execSync } = require('child_process');
    const registry = execSync('npm config get registry', {
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'ignore'] // Suppress stderr
    }).trim();

    // Validate registry URL
    if (registry && registry.startsWith('http')) {
      return registry;
    }
  } catch (e) {
    // Fall back to default if npm config fails
  }

  // Default npm registry
  return 'https://registry.npmjs.org/';
}

/**
 * Get latest version from npm registry
 * Respects user's configured npm registry (mirrors, corporate proxies, etc.)
 * @returns {Promise<string|null>} Latest version or null on error
 */
async function getLatestNpmVersion() {
  return new Promise((resolve) => {
    try {
      const registry = getNpmRegistry();
      // Ensure registry ends with /
      const baseUrl = registry.endsWith('/') ? registry : registry + '/';
      const url = `${baseUrl}abapgit-agent/latest`;

      https.get(url, (res) => {
        let body = '';
        res.on('data', chunk => body += chunk);
        res.on('end', () => {
          try {
            const data = JSON.parse(body);
            resolve(data.version || null);
          } catch (e) {
            resolve(null);
          }
        });
      }).on('error', () => resolve(null));
    } catch (e) {
      resolve(null);
    }
  });
}

/**
 * Compare two semver versions
 * @param {string} a - Version A (e.g., "1.9.0")
 * @param {string} b - Version B (e.g., "1.8.6")
 * @returns {number} 1 if a > b, -1 if a < b, 0 if equal
 */
function compareVersions(a, b) {
  const aParts = a.split('.').map(Number);
  const bParts = b.split('.').map(Number);

  for (let i = 0; i < 3; i++) {
    if (aParts[i] > bParts[i]) return 1;
    if (aParts[i] < bParts[i]) return -1;
  }
  return 0;
}

/**
 * Check for new version availability with daily caching
 * Cache file location (OS-specific):
 * - Linux/macOS: ~/.cache/abapgit-agent/version-check.json
 * - Windows: %LOCALAPPDATA%\abapgit-agent\cache\version-check.json
 * Format: { lastCheck: timestamp, latestVersion: "1.9.0" }
 * @returns {Promise<object>} { hasNewVersion, latestVersion, currentVersion }
 */
async function checkForNewVersion() {
  const cliVersion = getCliVersion();
  const cacheFile = pathModule.join(getCacheDir(), 'version-check.json');
  const now = Date.now();
  const ONE_DAY = 24 * 60 * 60 * 1000;

  // Read cache if exists and less than 24 hours old
  if (fs.existsSync(cacheFile)) {
    try {
      const cache = JSON.parse(fs.readFileSync(cacheFile, 'utf8'));
      if (now - cache.lastCheck < ONE_DAY && cache.latestVersion) {
        // Use cached result
        return {
          hasNewVersion: compareVersions(cache.latestVersion, cliVersion) > 0,
          latestVersion: cache.latestVersion,
          currentVersion: cliVersion
        };
      }
    } catch (e) {
      // Invalid cache, continue to fetch
    }
  }

  // Fetch latest version from npm
  const latestVersion = await getLatestNpmVersion();

  if (latestVersion) {
    // Write cache
    try {
      const cacheDir = pathModule.dirname(cacheFile);
      if (!fs.existsSync(cacheDir)) {
        fs.mkdirSync(cacheDir, { recursive: true });
      }
      fs.writeFileSync(cacheFile, JSON.stringify({
        lastCheck: now,
        latestVersion: latestVersion
      }));
    } catch (e) {
      // Ignore cache write errors
    }

    return {
      hasNewVersion: compareVersions(latestVersion, cliVersion) > 0,
      latestVersion: latestVersion,
      currentVersion: cliVersion
    };
  }

  // Failed to fetch, return no update
  return {
    hasNewVersion: false,
    latestVersion: null,
    currentVersion: cliVersion
  };
}

/**
 * Show new version reminder at end of command output
 * Non-blocking, silent on error
 */
async function showNewVersionReminder() {
  try {
    const { hasNewVersion, latestVersion, currentVersion } = await checkForNewVersion();

    if (hasNewVersion) {
      console.error('');
      console.error(`💡 New version available: ${latestVersion} (current: ${currentVersion})`);
      console.error(`   Run: abapgit-agent upgrade`);
    }
  } catch (e) {
    // Silent - don't interrupt user's workflow
  }
}

module.exports = {
  getCliVersion,
  checkCompatibility,
  getLatestNpmVersion,
  compareVersions,
  checkForNewVersion,
  showNewVersionReminder
};
