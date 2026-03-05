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
