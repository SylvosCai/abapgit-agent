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
});
