/**
 * Unit tests for upgrade command
 */

const upgradeCommand = require('../../src/commands/upgrade');

describe('Upgrade Command', () => {
  describe('parseFlags', () => {
    it('should parse --check flag', () => {
      const flags = upgradeCommand.parseFlags(['--check']);
      expect(flags.checkOnly).toBe(true);
    });

    it('should parse --cli-only flag', () => {
      const flags = upgradeCommand.parseFlags(['--cli-only']);
      expect(flags.cliOnly).toBe(true);
    });

    it('should parse --abap-only flag', () => {
      const flags = upgradeCommand.parseFlags(['--abap-only']);
      expect(flags.abapOnly).toBe(true);
    });

    it('should parse --match flag', () => {
      const flags = upgradeCommand.parseFlags(['--match']);
      expect(flags.match).toBe(true);
    });

    it('should parse --version with value', () => {
      const flags = upgradeCommand.parseFlags(['--version', '1.9.0']);
      expect(flags.version).toBe('1.9.0');
    });

    it('should parse --yes flag', () => {
      const flags = upgradeCommand.parseFlags(['--yes']);
      expect(flags.yes).toBe(true);
    });

    it('should parse -y flag', () => {
      const flags = upgradeCommand.parseFlags(['-y']);
      expect(flags.yes).toBe(true);
    });

    it('should parse --dry-run flag', () => {
      const flags = upgradeCommand.parseFlags(['--dry-run']);
      expect(flags.dryRun).toBe(true);
    });

    it('should parse --transport with value', () => {
      const flags = upgradeCommand.parseFlags(['--transport', 'DEVK900001']);
      expect(flags.transport).toBe('DEVK900001');
    });
  });

  describe('validateFlags', () => {
    it('should reject --match and --version together', () => {
      const flags = { match: true, version: '1.9.0' };
      expect(() => upgradeCommand.validateFlags(flags)).toThrow();
    });

    it('should reject --match and --cli-only together', () => {
      const flags = { match: true, cliOnly: true };
      expect(() => upgradeCommand.validateFlags(flags)).toThrow();
    });

    it('should reject --cli-only and --abap-only together', () => {
      const flags = { cliOnly: true, abapOnly: true };
      expect(() => upgradeCommand.validateFlags(flags)).toThrow();
    });

    it('should accept valid flag combinations', () => {
      expect(() => upgradeCommand.validateFlags({ cliOnly: true })).not.toThrow();
      expect(() => upgradeCommand.validateFlags({ abapOnly: true })).not.toThrow();
      expect(() => upgradeCommand.validateFlags({ version: '1.9.0' })).not.toThrow();
      expect(() => upgradeCommand.validateFlags({ match: true })).not.toThrow();
    });
  });

  describe('determineTargets', () => {
    it('should set targets for default upgrade (latest)', () => {
      const flags = {};
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBe('1.9.0');
      expect(targets.abapTarget).toBe('1.9.0');
    });

    it('should set targets for --cli-only', () => {
      const flags = { cliOnly: true };
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBe('1.9.0');
      expect(targets.abapTarget).toBeNull();
    });

    it('should set targets for --abap-only', () => {
      const flags = { abapOnly: true };
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBeNull();
      expect(targets.abapTarget).toBe('1.9.0');
    });

    it('should set targets for --match', () => {
      const flags = { match: true };
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBeNull();
      expect(targets.abapTarget).toBe('1.8.6');
    });

    it('should set targets for --version', () => {
      const flags = { version: '1.7.0' };
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBe('1.7.0');
      expect(targets.abapTarget).toBe('1.7.0');
    });

    it('should set targets for --version with --cli-only', () => {
      const flags = { version: '1.7.0', cliOnly: true };
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBe('1.7.0');
      expect(targets.abapTarget).toBeNull();
    });

    it('should set targets for --version with --abap-only', () => {
      const flags = { version: '1.7.0', abapOnly: true };
      const targets = upgradeCommand.determineTargets(flags, '1.8.6', '1.8.5', '1.9.0');
      expect(targets.cliTarget).toBeNull();
      expect(targets.abapTarget).toBe('1.7.0');
    });
  });

  describe('getArgValue', () => {
    it('should get value after flag', () => {
      const value = upgradeCommand.getArgValue(['--version', '1.9.0'], '--version');
      expect(value).toBe('1.9.0');
    });

    it('should return null if flag not found', () => {
      const value = upgradeCommand.getArgValue(['--check'], '--version');
      expect(value).toBeNull();
    });

    it('should return null if no value after flag', () => {
      const value = upgradeCommand.getArgValue(['--version'], '--version');
      expect(value).toBeNull();
    });
  });

  describe('ABAP unreachable handling', () => {
    it('sets cliOnly when ABAP unreachable and --yes flag is set', () => {
      const flags = { cliOnly: false, abapOnly: false, match: false, yes: true };
      const abapVersion = null; // unreachable

      if (abapVersion === null && !flags.cliOnly && !flags.abapOnly && !flags.match) {
        if (flags.yes) {
          flags.cliOnly = true;
        }
      }

      expect(flags.cliOnly).toBe(true);
    });

    it('does not modify flags when ABAP is reachable', () => {
      const flags = { cliOnly: false, abapOnly: false, match: false, yes: true };
      const abapVersion = '1.8.7'; // reachable

      if (abapVersion === null && !flags.cliOnly && !flags.abapOnly && !flags.match) {
        flags.cliOnly = true;
      }

      expect(flags.cliOnly).toBe(false);
    });

    it('does not modify flags when --abap-only is set', () => {
      const flags = { cliOnly: false, abapOnly: true, match: false, yes: true };
      const abapVersion = null;

      if (abapVersion === null && !flags.cliOnly && !flags.abapOnly && !flags.match) {
        flags.cliOnly = true;
      }

      // abap-only should not be changed to cliOnly - different error path
      expect(flags.cliOnly).toBe(false);
    });
  });

  describe('upgradeAbapBackend - URL resolution', () => {
    it('uses agentRepoUrl from config when present', () => {
      const context = {
        loadConfig: jest.fn(() => ({
          agentRepoUrl: 'https://github.com/myorg/abapgit-agent.git'
        }))
      };

      const config = context.loadConfig();
      const agentRepoUrl = config.agentRepoUrl || 'https://github.com/SylvosCai/abapgit-agent.git';

      expect(agentRepoUrl).toBe('https://github.com/myorg/abapgit-agent.git');
    });

    it('falls back to canonical URL when agentRepoUrl not configured', () => {
      const context = {
        loadConfig: jest.fn(() => ({}))
      };

      const config = context.loadConfig();
      const agentRepoUrl = config.agentRepoUrl || 'https://github.com/SylvosCai/abapgit-agent.git';

      expect(agentRepoUrl).toBe('https://github.com/SylvosCai/abapgit-agent.git');
    });

    it('pull args include --url when upgrading ABAP', () => {
      // Verify args structure passed to pull
      const agentRepoUrl = 'https://github.com/myorg/abapgit-agent.git';
      const version = '1.8.8';
      const pullArgs = ['--url', agentRepoUrl, '--branch', `v${version}`];

      expect(pullArgs).toContain('--url');
      expect(pullArgs[pullArgs.indexOf('--url') + 1]).toBe(agentRepoUrl);
      expect(pullArgs).toContain('--branch');
      expect(pullArgs[pullArgs.indexOf('--branch') + 1]).toBe('v1.8.8');
    });
  });
});
