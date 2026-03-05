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
});
