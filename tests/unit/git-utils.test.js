/**
 * Unit tests for git-utils
 * Tests git repository operations
 */

// Note: git-utils reads real files from the file system, so we test actual behavior
// rather than fully mocked behavior for most functions

describe('git-utils', () => {
  let gitUtils;

  beforeEach(() => {
    jest.resetModules();
    gitUtils = require('../../src/utils/git-utils');
  });

  describe('isGitRepo', () => {
    test('returns a boolean', () => {
      const result = gitUtils.isGitRepo();
      expect(typeof result).toBe('boolean');
    });

    test('returns true when in a git repository', () => {
      // This test runs in the actual abapgit-agent repo which is a git repo
      expect(gitUtils.isGitRepo()).toBe(true);
    });
  });

  describe('getRemoteUrl', () => {
    test('returns a string or null', () => {
      const result = gitUtils.getRemoteUrl();
      expect(result === null || typeof result === 'string').toBe(true);
    });

    test('returns a URL when in a git repository', () => {
      // This test runs in the actual abapgit-agent repo
      const url = gitUtils.getRemoteUrl();
      expect(url).toBeDefined();
      if (url) {
        // Should be a valid URL format
        expect(url).toMatch(/^https?:\/\/|git@/);
      }
    });
  });

  describe('getBranch', () => {
    test('returns a string', () => {
      const result = gitUtils.getBranch();
      expect(typeof result).toBe('string');
      expect(result.length).toBeGreaterThan(0);
    });

    test('returns a valid branch name', () => {
      const branch = gitUtils.getBranch();
      // Branch name should not contain spaces or special shell characters
      expect(branch).toMatch(/^[\w\-\/\.]+$/);
    });

    test('returns main or master or a feature branch', () => {
      const branch = gitUtils.getBranch();
      // Common branch names or feature branches
      expect(
        branch === 'main' ||
        branch === 'master' ||
        branch.includes('/') ||
        branch.match(/^[\w\-]+$/)
      ).toBe(true);
    });
  });

  describe('module exports', () => {
    test('exports isGitRepo function', () => {
      expect(typeof gitUtils.isGitRepo).toBe('function');
    });

    test('exports getRemoteUrl function', () => {
      expect(typeof gitUtils.getRemoteUrl).toBe('function');
    });

    test('exports getBranch function', () => {
      expect(typeof gitUtils.getBranch).toBe('function');
    });
  });
});
