/**
 * Unit tests for project-level safeguards configuration
 * Uses temporary files instead of mocking for reliability
 */

const fs = require('fs');
const path = require('path');

describe('Project Safeguards', () => {
  const originalCwd = process.cwd();
  const testDir = path.join(__dirname, 'temp-safeguards-test');
  const configPath = path.join(testDir, '.abapgit-agent.json');

  beforeEach(() => {
    // Create temp directory
    if (!fs.existsSync(testDir)) {
      fs.mkdirSync(testDir, { recursive: true });
    }
    // Change to test directory
    process.chdir(testDir);
    // Clear module cache to reload config
    jest.resetModules();
  });

  afterEach(() => {
    // Restore original directory
    process.chdir(originalCwd);
    // Clean up temp files
    if (fs.existsSync(configPath)) {
      fs.unlinkSync(configPath);
    }
    if (fs.existsSync(testDir)) {
      fs.rmdirSync(testDir);
    }
  });

  describe('getSafeguards', () => {
    test('returns default values when no project config exists', () => {
      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards).toEqual({
        requireFilesForPull: false,
        disablePull: false,
        disableRun: false,
        disableImport: false,
        requireImportMessage: false,
        importAllowedUsers: null,
        disableProbeClasses: false,
        reason: null
      });
    });

    test('loads safeguards from project config', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        project: { name: 'Test Project' },
        safeguards: {
          requireFilesForPull: true,
          disablePull: false,
          reason: 'Large project with 500+ objects'
        }
      }));

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards.requireFilesForPull).toBe(true);
      expect(safeguards.disablePull).toBe(false);
      expect(safeguards.reason).toBe('Large project with 500+ objects');
    });

    test('handles missing safeguards section gracefully', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        project: { name: 'Test Project' }
        // No safeguards section
      }));

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards.requireFilesForPull).toBe(false);
      expect(safeguards.disablePull).toBe(false);
    });

    test('handles invalid JSON gracefully', () => {
      const consoleWarnSpy = jest.spyOn(console, 'warn').mockImplementation();

      fs.writeFileSync(configPath, 'invalid json{');

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(consoleWarnSpy).toHaveBeenCalled();
      expect(safeguards.requireFilesForPull).toBe(false);

      consoleWarnSpy.mockRestore();
    });

    test('returns false values for missing boolean fields', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: {
          reason: 'Some reason'
          // requireFilesForPull and disablePull omitted
        }
      }));

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards.requireFilesForPull).toBe(false);
      expect(safeguards.disablePull).toBe(false);
      expect(safeguards.reason).toBe('Some reason');
    });

    test('handles explicit false values correctly', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: {
          requireFilesForPull: false,
          disablePull: true,
          reason: 'Test'
        }
      }));

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards.requireFilesForPull).toBe(false);
      expect(safeguards.disablePull).toBe(true);
    });

    test('disableProbeClasses defaults to false', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: { disablePull: false }
      }));

      const { getSafeguards } = require('../../src/config');
      expect(getSafeguards().disableProbeClasses).toBe(false);
    });

    test('disableProbeClasses reads true from config', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: {
          disableProbeClasses: true,
          reason: 'Production-adjacent system'
        }
      }));

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards.disableProbeClasses).toBe(true);
      expect(safeguards.reason).toBe('Production-adjacent system');
    });

    test('disableImport defaults to false', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: { disablePull: false }
      }));

      const { getSafeguards } = require('../../src/config');
      expect(getSafeguards().disableImport).toBe(false);
    });

    test('disableImport reads true from config', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: {
          disableImport: true,
          reason: 'One-time operation managed by release manager'
        }
      }));

      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();

      expect(safeguards.disableImport).toBe(true);
      expect(safeguards.reason).toBe('One-time operation managed by release manager');
    });

    test('requireImportMessage defaults to false', () => {
      fs.writeFileSync(configPath, JSON.stringify({ safeguards: {} }));
      const { getSafeguards } = require('../../src/config');
      expect(getSafeguards().requireImportMessage).toBe(false);
    });

    test('requireImportMessage reads true from config', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: { requireImportMessage: true, reason: 'All imports must be traceable' }
      }));
      const { getSafeguards } = require('../../src/config');
      const safeguards = getSafeguards();
      expect(safeguards.requireImportMessage).toBe(true);
      expect(safeguards.reason).toBe('All imports must be traceable');
    });

    test('importAllowedUsers defaults to null', () => {
      fs.writeFileSync(configPath, JSON.stringify({ safeguards: {} }));
      const { getSafeguards } = require('../../src/config');
      expect(getSafeguards().importAllowedUsers).toBeNull();
    });

    test('importAllowedUsers normalizes string to uppercase array', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: { disableImport: true, importAllowedUsers: 'i045696' }
      }));
      const { getSafeguards } = require('../../src/config');
      expect(getSafeguards().importAllowedUsers).toEqual(['I045696']);
    });

    test('importAllowedUsers normalizes array to uppercase', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: { disableImport: true, importAllowedUsers: ['i045696', 'john'] }
      }));
      const { getSafeguards } = require('../../src/config');
      expect(getSafeguards().importAllowedUsers).toEqual(['I045696', 'JOHN']);
    });
  });

  describe('getProjectInfo', () => {
    test('returns project info when config exists', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        project: {
          name: 'My ABAP Project',
          description: 'Project description'
        }
      }));

      const { getProjectInfo } = require('../../src/config');
      const projectInfo = getProjectInfo();

      expect(projectInfo).toEqual({
        name: 'My ABAP Project',
        description: 'Project description'
      });
    });

    test('returns null when no project config exists', () => {
      const { getProjectInfo } = require('../../src/config');
      const projectInfo = getProjectInfo();

      expect(projectInfo).toBeNull();
    });

    test('returns null when project section is missing', () => {
      fs.writeFileSync(configPath, JSON.stringify({
        safeguards: {
          requireFilesForPull: true
        }
        // No project section
      }));

      const { getProjectInfo } = require('../../src/config');
      const projectInfo = getProjectInfo();

      expect(projectInfo).toBeNull();
    });
  });
});
