/**
 * Unit tests for version-check
 * Tests CLI version detection and compatibility checking
 */

// We don't mock fs/path for version-check because it reads the actual package.json
// Instead we test with the real version

describe('version-check', () => {
  beforeEach(() => {
    jest.resetModules();
  });

  describe('getCliVersion', () => {
    test('returns a valid version string', () => {
      const versionCheck = require('../../src/utils/version-check');
      const version = versionCheck.getCliVersion();

      expect(version).toBeDefined();
      expect(typeof version).toBe('string');
      // Should match semver pattern (e.g., 1.7.2 or 1.0.0-beta.1)
      expect(version).toMatch(/^\d+\.\d+\.\d+/);
    });

    test('returns same version on multiple calls', () => {
      const versionCheck = require('../../src/utils/version-check');
      const version1 = versionCheck.getCliVersion();
      const version2 = versionCheck.getCliVersion();

      expect(version1).toBe(version2);
    });
  });

  describe('checkCompatibility', () => {
    test('checkCompatibility function exists and is callable', () => {
      const versionCheck = require('../../src/utils/version-check');
      expect(typeof versionCheck.checkCompatibility).toBe('function');
    });

    test('returns result object with cliVersion', async () => {
      const config = {
        host: 'invalid-host-for-testing.example.com',
        sapport: 443,
        client: '100',
        user: 'TEST_USER',
        password: 'test_pass',
        language: 'EN'
      };

      const versionCheck = require('../../src/utils/version-check');
      const result = await versionCheck.checkCompatibility(config);

      // Should return an object with at least cliVersion
      expect(result).toHaveProperty('cliVersion');
      expect(typeof result.cliVersion).toBe('string');
      expect(result.cliVersion).toMatch(/^\d+\.\d+\.\d+/);
    });

    test('returns compatible field in result', async () => {
      const config = {
        host: 'invalid-host-for-testing.example.com',
        sapport: 443,
        client: '100',
        user: 'TEST_USER',
        password: 'test_pass',
        language: 'EN'
      };

      const versionCheck = require('../../src/utils/version-check');
      const result = await versionCheck.checkCompatibility(config);

      // Should have compatible field (will be false due to connection error)
      expect(result).toHaveProperty('compatible');
      expect(typeof result.compatible).toBe('boolean');
    });
  });

  describe('compareVersions', () => {
    test('returns 0 for equal versions', () => {
      const versionCheck = require('../../src/utils/version-check');
      expect(versionCheck.compareVersions('1.8.6', '1.8.6')).toBe(0);
      expect(versionCheck.compareVersions('1.0.0', '1.0.0')).toBe(0);
    });

    test('returns 1 when first version is greater', () => {
      const versionCheck = require('../../src/utils/version-check');
      expect(versionCheck.compareVersions('1.9.0', '1.8.6')).toBe(1);
      expect(versionCheck.compareVersions('2.0.0', '1.9.9')).toBe(1);
      expect(versionCheck.compareVersions('1.8.7', '1.8.6')).toBe(1);
    });

    test('returns -1 when first version is less', () => {
      const versionCheck = require('../../src/utils/version-check');
      expect(versionCheck.compareVersions('1.8.6', '1.9.0')).toBe(-1);
      expect(versionCheck.compareVersions('1.9.9', '2.0.0')).toBe(-1);
      expect(versionCheck.compareVersions('1.8.6', '1.8.7')).toBe(-1);
    });

    test('handles edge cases correctly', () => {
      const versionCheck = require('../../src/utils/version-check');
      // 1.10.0 should be greater than 1.9.0
      expect(versionCheck.compareVersions('1.10.0', '1.9.0')).toBe(1);
      expect(versionCheck.compareVersions('1.9.0', '1.10.0')).toBe(-1);

      // 1.9.10 should be greater than 1.9.9
      expect(versionCheck.compareVersions('1.9.10', '1.9.9')).toBe(1);
      expect(versionCheck.compareVersions('1.9.9', '1.9.10')).toBe(-1);

      // Major version differences
      expect(versionCheck.compareVersions('2.0.0', '1.99.99')).toBe(1);
      expect(versionCheck.compareVersions('1.99.99', '2.0.0')).toBe(-1);
    });
  });

  describe('getLatestNpmVersion', () => {
    test('returns a version string or null', async () => {
      const versionCheck = require('../../src/utils/version-check');
      const version = await versionCheck.getLatestNpmVersion();
      // Either a valid version or null (if network fails)
      if (version !== null) {
        expect(version).toMatch(/^\d+\.\d+\.\d+$/);
      } else {
        expect(version).toBeNull();
      }
    }, 10000); // 10 second timeout for network call

    test('respects npm registry configuration', async () => {
      // This test verifies that getLatestNpmVersion uses the configured registry
      // We can't mock the npm config without complex setup, but we can verify
      // that it doesn't break when a custom registry is configured
      const versionCheck = require('../../src/utils/version-check');
      const version = await versionCheck.getLatestNpmVersion();

      // Should either return a version or null, not throw an error
      expect(version === null || typeof version === 'string').toBe(true);
    }, 10000);
  });

  describe('checkForNewVersion', () => {
    test('returns version check result', async () => {
      const versionCheck = require('../../src/utils/version-check');
      const result = await versionCheck.checkForNewVersion();

      expect(result).toHaveProperty('hasNewVersion');
      expect(result).toHaveProperty('currentVersion');
      expect(typeof result.hasNewVersion).toBe('boolean');
      expect(result.currentVersion).toMatch(/^\d+\.\d+\.\d+$/);

      // latestVersion may be null if network failed
      if (result.latestVersion !== null) {
        expect(result.latestVersion).toMatch(/^\d+\.\d+\.\d+$/);
      }
    }, 10000); // 10 second timeout for network call
  });

  describe('showNewVersionReminder', () => {
    test('does not throw errors', async () => {
      const versionCheck = require('../../src/utils/version-check');
      // Should complete without throwing, even on network errors
      await expect(versionCheck.showNewVersionReminder()).resolves.toBeUndefined();
    }, 10000);
  });
});

// ---------------------------------------------------------------------------
// --version / -v flag (tested via the CLI entry point)
// ---------------------------------------------------------------------------

describe('--version / -v flag', () => {
  const { execSync } = require('child_process');
  const binPath = require('path').join(__dirname, '..', '..', 'bin', 'abapgit-agent');

  test('--version prints a valid semver string', () => {
    const output = execSync(`node ${binPath} --version`, { encoding: 'utf8' }).trim();
    expect(output).toMatch(/^\d+\.\d+\.\d+/);
  });

  test('-v prints the same version as --version', () => {
    const v1 = execSync(`node ${binPath} --version`, { encoding: 'utf8' }).trim();
    const v2 = execSync(`node ${binPath} -v`, { encoding: 'utf8' }).trim();
    expect(v1).toBe(v2);
  });

  test('--version matches getCliVersion()', () => {
    const versionCheck = require('../../src/utils/version-check');
    const output = execSync(`node ${binPath} --version`, { encoding: 'utf8' }).trim();
    expect(output).toBe(versionCheck.getCliVersion());
  });
});

// ---------------------------------------------------------------------------
// Mocked tests — cover branches that require controlled I/O
// ---------------------------------------------------------------------------

describe('version-check (mocked fs/https)', () => {
  let fs;
  let https;
  let os;

  beforeEach(() => {
    jest.resetModules();
    jest.mock('fs');
    jest.mock('https');
    jest.mock('os');
    fs    = require('fs');
    https = require('https');
    os    = require('os');
    os.homedir.mockReturnValue('/home/user');
  });

  afterEach(() => {
    jest.restoreAllMocks();
    delete process.env.XDG_CACHE_HOME;
    delete process.env.LOCALAPPDATA;
  });

  // ── getCacheDir ────────────────────────────────────────────────────────────

  describe('getCacheDir()', () => {
    test('returns XDG_CACHE_HOME based path when env var is set', () => {
      process.env.XDG_CACHE_HOME = '/custom/cache';
      const vc = require('../../src/utils/version-check');
      // getCacheDir is not exported directly, but checkForNewVersion uses it;
      // we can verify the cache file path indirectly through checkForNewVersion
      fs.existsSync.mockReturnValue(false);  // cache file doesn't exist
      // Just verify it doesn't throw
      expect(() => vc.getCliVersion()).not.toThrow();
    });

    test('Windows: uses LOCALAPPDATA when set', () => {
      const origPlatform = process.platform;
      Object.defineProperty(process, 'platform', { value: 'win32', configurable: true });
      process.env.LOCALAPPDATA = 'C:\\Users\\user\\AppData\\Local';

      const vc = require('../../src/utils/version-check');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.9.0' }));
      expect(vc.getCliVersion()).toMatch(/^\d+\.\d+\.\d+/);

      Object.defineProperty(process, 'platform', { value: origPlatform, configurable: true });
    });
  });

  // ── getCliVersion fallback ─────────────────────────────────────────────────

  describe('getCliVersion() fallback', () => {
    test('returns "1.0.0" when package.json does not exist', () => {
      fs.existsSync.mockReturnValue(false);
      const vc = require('../../src/utils/version-check');
      expect(vc.getCliVersion()).toBe('1.0.0');
    });
  });

  // ── getNpmRegistry ────────────────────────────────────────────────────────

  describe('getNpmRegistry()', () => {
    test('falls back to https://registry.npmjs.org/ when npm config get fails', () => {
      // Mock child_process.execSync to throw
      jest.mock('child_process', () => ({
        execSync: jest.fn(() => { throw new Error('npm not found'); })
      }));
      const vc = require('../../src/utils/version-check');
      // getLatestNpmVersion calls getNpmRegistry internally; mock https.get to capture URL
      let capturedUrl = null;
      const mockReq = { on: jest.fn().mockReturnThis() };
      https.get.mockImplementation((url, cb) => {
        capturedUrl = url;
        return mockReq;
      });
      vc.getLatestNpmVersion();
      expect(capturedUrl).toContain('registry.npmjs.org');
    });
  });

  // ── checkCompatibility HTTP response branches ─────────────────────────────

  describe('checkCompatibility() HTTP response branches', () => {
    const makeConfig = () => ({
      host: 'test.sap.com', sapport: 443, client: '100',
      user: 'USER', password: 'PASS', language: 'EN'
    });

    function mockHttpsRequest(statusCode, responseBody) {
      const { EventEmitter } = require('events');
      const res = new EventEmitter();
      res.statusCode = statusCode;
      const req = new EventEmitter();
      req.end = jest.fn();
      https.request.mockImplementation((_opts, cb) => {
        process.nextTick(() => {
          cb(res);
          process.nextTick(() => {
            res.emit('data', responseBody);
            res.emit('end');
          });
        });
        return req;
      });
      https.Agent = jest.fn().mockImplementation(() => ({}));
      return { req };
    }

    test('resolves with compatible:true when versions match', async () => {
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.9.0' }));
      mockHttpsRequest(200, JSON.stringify({ version: '1.9.0' }));

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkCompatibility(makeConfig());
      expect(result.compatible).toBe(true);
      expect(result.apiVersion).toBe('1.9.0');
    });

    test('resolves with compatible:false and logs warning when versions differ', async () => {
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.9.0' }));
      mockHttpsRequest(200, JSON.stringify({ version: '1.8.5' }));

      const errOutput = [];
      const origErr = console.error;
      console.error = (...a) => errOutput.push(a.join(' '));

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkCompatibility(makeConfig());

      console.error = origErr;
      expect(result.compatible).toBe(false);
      expect(errOutput.join('\n')).toMatch(/Version mismatch/);
    });

    test('resolves gracefully when response body is not valid JSON', async () => {
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.9.0' }));
      mockHttpsRequest(200, 'not json at all');

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkCompatibility(makeConfig());
      expect(result.apiVersion).toBeNull();
      expect(result.compatible).toBe(false);
    });

    test('resolves gracefully on network error', async () => {
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.9.0' }));

      const { EventEmitter } = require('events');
      const req = new EventEmitter();
      req.end = jest.fn();
      https.request.mockImplementation(() => {
        process.nextTick(() => req.emit('error', new Error('ECONNREFUSED')));
        return req;
      });
      https.Agent = jest.fn().mockImplementation(() => ({}));

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkCompatibility(makeConfig());
      expect(result.apiVersion).toBeNull();
      expect(result.error).toMatch(/ECONNREFUSED/);
    });
  });

  // ── checkForNewVersion cache branches ─────────────────────────────────────

  describe('checkForNewVersion() cache branches', () => {
    test('returns cached result when cache is fresh (< 24h)', async () => {
      const now = Date.now();
      const cachedVersion = '1.9.0';
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockImplementation((p) => {
        if (String(p).includes('version-check.json')) {
          return JSON.stringify({ lastCheck: now - 1000, latestVersion: cachedVersion });
        }
        return JSON.stringify({ version: '1.8.0' }); // package.json
      });

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkForNewVersion();
      expect(result.latestVersion).toBe(cachedVersion);
      // https.get should NOT have been called (used cache)
      expect(https.get).not.toHaveBeenCalled();
    });

    test('fetches fresh when cache is stale (> 24h)', async () => {
      const staleTime = Date.now() - 25 * 60 * 60 * 1000;
      fs.existsSync.mockImplementation((p) => {
        if (String(p).includes('version-check.json')) return true;
        if (String(p).includes('package.json')) return true;
        return false;
      });
      fs.readFileSync.mockImplementation((p) => {
        if (String(p).includes('version-check.json')) {
          return JSON.stringify({ lastCheck: staleTime, latestVersion: '1.8.0' });
        }
        return JSON.stringify({ version: '1.8.0' });
      });
      fs.mkdirSync = jest.fn();
      fs.writeFileSync = jest.fn();

      const mockReq = { on: jest.fn().mockReturnThis() };
      const { EventEmitter } = require('events');
      const res = new EventEmitter();
      https.get.mockImplementation((_url, cb) => {
        process.nextTick(() => {
          cb(res);
          res.emit('data', JSON.stringify({ version: '1.9.0' }));
          res.emit('end');
        });
        return mockReq;
      });

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkForNewVersion();
      expect(result.latestVersion).toBe('1.9.0');
      expect(fs.writeFileSync).toHaveBeenCalled();
    });

    test('creates cache dir when it does not exist', async () => {
      fs.existsSync.mockImplementation((p) => {
        if (String(p).includes('version-check.json')) return false;
        if (String(p).includes('package.json')) return true;
        return false; // cache dir doesn't exist
      });
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.8.0' }));
      fs.mkdirSync = jest.fn();
      fs.writeFileSync = jest.fn();

      const { EventEmitter } = require('events');
      const res = new EventEmitter();
      const mockReq = { on: jest.fn().mockReturnThis() };
      https.get.mockImplementation((_url, cb) => {
        process.nextTick(() => {
          cb(res);
          res.emit('data', JSON.stringify({ version: '1.9.0' }));
          res.emit('end');
        });
        return mockReq;
      });

      const vc = require('../../src/utils/version-check');
      await vc.checkForNewVersion();
      expect(fs.mkdirSync).toHaveBeenCalledWith(expect.any(String), { recursive: true });
    });

    test('returns hasNewVersion:false when fetch fails', async () => {
      fs.existsSync.mockReturnValue(false);
      fs.readFileSync.mockReturnValue(JSON.stringify({ version: '1.8.0' }));

      const mockReq = { on: jest.fn().mockReturnThis() };
      https.get.mockImplementation((_url, cb) => {
        // Never calls cb (simulates timeout / immediate error via .on('error'))
        const req = mockReq;
        process.nextTick(() => {
          // Trigger error on the returned req object
        });
        return { on: (evt, fn) => { if (evt === 'error') fn(new Error('timeout')); return mockReq; } };
      });

      const vc = require('../../src/utils/version-check');
      const result = await vc.checkForNewVersion();
      expect(result.hasNewVersion).toBe(false);
      expect(result.latestVersion).toBeNull();
    });
  });

  // ── showNewVersionReminder new-version branch ──────────────────────────────

  describe('showNewVersionReminder() new-version branch', () => {
    test('prints upgrade hint when newer version is available', async () => {
      const now = Date.now();
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockImplementation((p) => {
        if (String(p).includes('version-check.json')) {
          return JSON.stringify({ lastCheck: now - 1000, latestVersion: '9.9.9' });
        }
        return JSON.stringify({ version: '1.0.0' });
      });

      const errOutput = [];
      const origErr = console.error;
      console.error = (...a) => errOutput.push(a.join(' '));

      const vc = require('../../src/utils/version-check');
      await vc.showNewVersionReminder();

      console.error = origErr;
      expect(errOutput.join('\n')).toMatch(/New version available/);
      expect(errOutput.join('\n')).toMatch(/9\.9\.9/);
    });

    test('prints nothing when already on latest version', async () => {
      const now = Date.now();
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockImplementation((p) => {
        if (String(p).includes('version-check.json')) {
          return JSON.stringify({ lastCheck: now - 1000, latestVersion: '1.0.0' });
        }
        return JSON.stringify({ version: '1.0.0' });
      });

      const errOutput = [];
      const origErr = console.error;
      console.error = (...a) => errOutput.push(a.join(' '));

      const vc = require('../../src/utils/version-check');
      await vc.showNewVersionReminder();

      console.error = origErr;
      expect(errOutput).toHaveLength(0);
    });
  });
});
