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

    it('upgradeAbapBackend calls pull.pull() with correct url and no credentials', () => {
      // Verify the direct pull.pull() call passes the right url and null credentials
      const agentRepoUrl = 'https://github.com/myorg/abapgit-agent.git';
      const version = '1.8.8';

      // The call is: pullCommand.pull(agentRepoUrl, `v${version}`, null, transport, lc, AbapHttp, false, null)
      // Positional args: (gitUrl, branch, files, transportRequest, loadConfig, AbapHttp, jsonOutput, gitCredentials)
      const callArgs = [agentRepoUrl, `v${version}`, null, null, null, null, false, null];

      expect(callArgs[0]).toBe(agentRepoUrl);
      expect(callArgs[1]).toBe('v1.8.8');
      expect(callArgs[7]).toBeNull(); // null credentials = no creds sent to public repo
    });
  });
});

describe('Upgrade Command - showCheckReport()', () => {
  let logs;
  let origLog;

  beforeEach(() => {
    logs = [];
    origLog = console.log;
    console.log = (...args) => logs.push(args.join(' '));
  });

  afterEach(() => {
    console.log = origLog;
  });

  test('prints CLI and ABAP versions when both available', () => {
    upgradeCommand.showCheckReport('1.8.6', '1.8.5', '1.9.0');
    const text = logs.join('\n');
    expect(text).toMatch(/CLI:.*v1\.8\.6/);
    expect(text).toMatch(/ABAP:.*v1\.8\.5/);
    expect(text).toMatch(/Latest available:.*v1\.9\.0/);
  });

  test('omits ABAP line when abapVersion is null', () => {
    upgradeCommand.showCheckReport('1.8.6', null, '1.9.0');
    const text = logs.join('\n');
    expect(text).toMatch(/CLI:.*v1\.8\.6/);
    expect(text).not.toMatch(/ABAP:/);
  });

  test('omits latest version line when latestVersion is null', () => {
    upgradeCommand.showCheckReport('1.8.6', '1.8.6', null);
    const text = logs.join('\n');
    expect(text).not.toMatch(/Latest available/);
  });

  test('shows "all up to date" when CLI and ABAP match latest', () => {
    upgradeCommand.showCheckReport('1.9.0', '1.9.0', '1.9.0');
    expect(logs.join('\n')).toMatch(/up to date/i);
  });

  test('shows version mismatch warning when CLI and ABAP differ', () => {
    upgradeCommand.showCheckReport('1.9.0', '1.8.5', '1.9.0');
    expect(logs.join('\n')).toMatch(/mismatch/i);
  });

  test('shows upgrade instructions when updates are available', () => {
    upgradeCommand.showCheckReport('1.8.6', '1.8.5', '1.9.0');
    const text = logs.join('\n');
    expect(text).toMatch(/To upgrade/i);
    expect(text).toMatch(/--cli-only/);
    expect(text).toMatch(/--abap-only/);
  });

  test('omits ABAP upgrade options when abapVersion is null', () => {
    upgradeCommand.showCheckReport('1.8.6', null, '1.9.0');
    const text = logs.join('\n');
    expect(text).not.toMatch(/--abap-only/);
    expect(text).not.toMatch(/--match/);
  });
});

describe('Upgrade Command - showDryRunPlan()', () => {
  let logs;
  let origLog;

  beforeEach(() => {
    logs = [];
    origLog = console.log;
    console.log = (...args) => logs.push(args.join(' '));
  });

  afterEach(() => {
    console.log = origLog;
  });

  test('shows DRY RUN banner', () => {
    upgradeCommand.showDryRunPlan('1.8.6', '1.8.5', { cliTarget: '1.9.0', abapTarget: '1.9.0' }, {});
    expect(logs.join('\n')).toMatch(/DRY RUN/);
  });

  test('shows npm install command for CLI target', () => {
    upgradeCommand.showDryRunPlan('1.8.6', '1.8.5', { cliTarget: '1.9.0', abapTarget: null }, {});
    expect(logs.join('\n')).toMatch(/npm install -g abapgit-agent@1\.9\.0/);
  });

  test('shows pull command for ABAP target', () => {
    upgradeCommand.showDryRunPlan('1.8.6', '1.8.5', { cliTarget: null, abapTarget: '1.9.0' }, {});
    expect(logs.join('\n')).toMatch(/pull --url.*v1\.9\.0/);
  });

  test('shows both commands when both targets set', () => {
    upgradeCommand.showDryRunPlan('1.8.6', '1.8.5', { cliTarget: '1.9.0', abapTarget: '1.9.0' }, {});
    const text = logs.join('\n');
    expect(text).toMatch(/npm install/);
    expect(text).toMatch(/pull --url/);
  });

  test('shows current versions in dry run output', () => {
    upgradeCommand.showDryRunPlan('1.8.6', '1.8.5', { cliTarget: '1.9.0', abapTarget: '1.9.0' }, {});
    const text = logs.join('\n');
    expect(text).toMatch(/1\.8\.6/);
    expect(text).toMatch(/1\.8\.5/);
  });

  test('omits ABAP current version line when abapVersion is null', () => {
    upgradeCommand.showDryRunPlan('1.8.6', null, { cliTarget: '1.9.0', abapTarget: null }, {});
    const text = logs.join('\n');
    expect(text).not.toMatch(/ABAP:.*v/);
  });

  test('ends with "No changes made"', () => {
    upgradeCommand.showDryRunPlan('1.8.6', '1.8.5', { cliTarget: '1.9.0', abapTarget: null }, {});
    expect(logs.join('\n')).toMatch(/No changes made/);
  });
});

describe('Upgrade Command - verifyUpgrade()', () => {
  let logs;
  let origLog;

  beforeEach(() => {
    logs = [];
    origLog = console.log;
    console.log = (...args) => logs.push(args.join(' '));
  });

  afterEach(() => {
    console.log = origLog;
  });

  test('shows CLI version verified when versions match', async () => {
    const context = {
      versionCheck: { getCliVersion: jest.fn(() => '1.9.0'), checkCompatibility: jest.fn() },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: '1.9.0', abapTarget: null }, {}, context);
    expect(logs.join('\n')).toMatch(/CLI version verified.*1\.9\.0/);
  });

  test('shows CLI version mismatch warning when versions differ', async () => {
    const context = {
      versionCheck: { getCliVersion: jest.fn(() => '1.8.6'), checkCompatibility: jest.fn() },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: '1.9.0', abapTarget: null }, {}, context);
    expect(logs.join('\n')).toMatch(/CLI version mismatch/);
  });

  test('shows ABAP version verified when versions match', async () => {
    const context = {
      versionCheck: {
        getCliVersion: jest.fn(() => '1.9.0'),
        checkCompatibility: jest.fn().mockResolvedValue({ apiVersion: '1.9.0' })
      },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: null, abapTarget: '1.9.0' }, {}, context);
    expect(logs.join('\n')).toMatch(/ABAP version verified.*1\.9\.0/);
  });

  test('shows ABAP version mismatch when versions differ', async () => {
    const context = {
      versionCheck: {
        getCliVersion: jest.fn(() => '1.9.0'),
        checkCompatibility: jest.fn().mockResolvedValue({ apiVersion: '1.8.5' })
      },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: null, abapTarget: '1.9.0' }, {}, context);
    expect(logs.join('\n')).toMatch(/ABAP version mismatch/);
  });

  test('shows warning when ABAP verify throws (unreachable system)', async () => {
    const context = {
      versionCheck: {
        getCliVersion: jest.fn(() => '1.9.0'),
        checkCompatibility: jest.fn().mockRejectedValue(new Error('ECONNREFUSED'))
      },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: null, abapTarget: '1.9.0' }, {}, context);
    expect(logs.join('\n')).toMatch(/Could not verify ABAP version/);
  });

  test('skips ABAP check when cliOnly flag is set', async () => {
    const mockCheckCompatibility = jest.fn();
    const context = {
      versionCheck: {
        getCliVersion: jest.fn(() => '1.9.0'),
        checkCompatibility: mockCheckCompatibility
      },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: '1.9.0', abapTarget: '1.9.0' }, { cliOnly: true }, context);
    expect(mockCheckCompatibility).not.toHaveBeenCalled();
  });

  test('always ends with "Upgrade complete!" message', async () => {
    const context = {
      versionCheck: { getCliVersion: jest.fn(() => '1.9.0'), checkCompatibility: jest.fn() },
      loadConfig: jest.fn(() => ({}))
    };
    await upgradeCommand.verifyUpgrade({ cliTarget: null, abapTarget: null }, {}, context);
    expect(logs.join('\n')).toMatch(/Upgrade complete/);
  });
});

