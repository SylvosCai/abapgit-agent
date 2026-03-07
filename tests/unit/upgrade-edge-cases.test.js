/**
 * Edge case tests for upgrade command
 * Tests error handling, validation, and edge cases
 */

const { execSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const CLI_PATH = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');

const MOCK_CONFIG = {
  host: 'mock-host.example.com',
  sapport: 443,
  client: '100',
  user: 'TEST_USER',
  password: 'test_password',
  language: 'EN',
  gitUsername: 'test',
  gitPassword: 'test'
};

/**
 * Creates a temp directory with a mock .abapGitAgent and runs the CLI from there.
 * Never touches the real project's .abapGitAgent.
 */
const runCommandWithMockConfig = (args) => {
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-agent-test-'));
  try {
    fs.writeFileSync(path.join(tmpDir, '.abapGitAgent'), JSON.stringify(MOCK_CONFIG, null, 2));
    try {
      return execSync(`node ${CLI_PATH} ${args}`, {
        encoding: 'utf8',
        stdio: ['pipe', 'pipe', 'pipe'],
        cwd: tmpDir
      });
    } catch (e) {
      return e.stdout || e.stderr || e.message;
    }
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
};

describe('Upgrade Command - Edge Cases', () => {
  // Helper to run CLI command
  const runCommand = (args) => {
    try {
      return execSync(`node ${CLI_PATH} ${args}`, {
        encoding: 'utf8',
        stdio: ['pipe', 'pipe', 'pipe']
      });
    } catch (e) {
      return e.stdout || e.stderr || e.message;
    }
  };

  describe('Flag Validation', () => {
    test('rejects --cli-only and --abap-only together', () => {
      const output = runCommand('upgrade --cli-only --abap-only');
      expect(output).toContain('Cannot use --cli-only and --abap-only together');
    });

    test('rejects --match and --version together', () => {
      const output = runCommand('upgrade --match --version 1.8.5');
      expect(output).toContain('Cannot use --match and --version together');
    });

    test('rejects --match and --cli-only together', () => {
      const output = runCommand('upgrade --match --cli-only');
      expect(output).toContain('Cannot use --match with --cli-only');
    });
  });

  describe('Version Validation', () => {
    test('validates version exists in npm registry', () => {
      const output = runCommand('upgrade --version 99.99.99 --cli-only --dry-run');
      expect(output).toContain('Version 99.99.99 not found in npm registry');
    }, 15000);

    test('accepts valid version', () => {
      const output = runCommand('upgrade --version 1.8.6 --cli-only --dry-run');
      expect(output).toContain('Target versions:');
      expect(output).toContain('v1.8.6');
    }, 15000);
  });

  describe('ABAP Config Handling', () => {
    test('--check works without ABAP config', () => {
      const output = runCommand('upgrade --check');
      expect(output).toContain('Current versions:');
      expect(output).toContain('CLI:');
    });

    test('--cli-only works without ABAP config', () => {
      const output = runCommand('upgrade --cli-only --dry-run');
      expect(output).toContain('DRY RUN');
      expect(output).not.toContain('ABAP:');
    });
  });

  describe('Dry-run Mode', () => {
    test('dry-run shows plan without executing (CLI-only, no config needed)', () => {
      const output = runCommand('upgrade --cli-only --dry-run');
      expect(output).toContain('DRY RUN - No changes will be made');
      expect(output).toContain('Would execute:');
      expect(output).toContain('No changes made.');
    });

    test('dry-run with --version shows specific version (CLI-only, no config needed)', () => {
      const output = runCommand('upgrade --version 1.8.5 --cli-only --dry-run');
      expect(output).toContain('Target versions:');
      expect(output).toContain('v1.8.5');
    }, 15000);

    test('dry-run with --cli-only excludes ABAP', () => {
      const output = runCommand('upgrade --cli-only --dry-run');
      expect(output).toContain('npm install');
      expect(output).not.toContain('pull --branch');
    });

    test('dry-run with --abap-only excludes CLI (with mocked config)', () => {
      const output = runCommandWithMockConfig('upgrade --abap-only --dry-run');
      expect(output).toContain('pull --url');
      expect(output).toContain('--branch');
      expect(output).not.toContain('npm install');
    });
  });

  describe('Check Mode', () => {
    test('check mode shows current and latest versions', () => {
      const output = runCommand('upgrade --check');
      expect(output).toContain('Current versions:');
      expect(output).toContain('Latest available:');
    });

    test('check mode with --version flag is ignored (check shows latest)', () => {
      const output = runCommand('upgrade --check --version 1.8.5');
      expect(output).toContain('Current versions:');
      // --version is ignored in check mode
    });
  });

  describe('Transport Request', () => {
    test('accepts transport request with --abap-only (with mocked config)', () => {
      const output = runCommandWithMockConfig('upgrade --abap-only --transport DEVK900001 --dry-run');
      expect(output).toContain('DRY RUN');
      expect(output).toContain('pull --url');
      expect(output).toContain('--branch');
    });
  });

  describe('Yes Flag', () => {
    test('--yes skips confirmation prompt (CLI-only, no config needed)', () => {
      const output = runCommand('upgrade --yes --cli-only --dry-run');
      expect(output).not.toContain('Do you want to continue?');
      expect(output).toContain('DRY RUN');
    });

    test('-y shorthand works (CLI-only, no config needed)', () => {
      const output = runCommand('upgrade -y --cli-only --dry-run');
      expect(output).not.toContain('Do you want to continue?');
      expect(output).toContain('DRY RUN');
    });
  });

  describe('Match Flag', () => {
    test('--match requires ABAP config (shows error without config)', () => {
      const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-agent-test-'));
      try {
        const output = (() => {
          try {
            return execSync(`node ${CLI_PATH} upgrade --match --dry-run`, {
              encoding: 'utf8',
              stdio: ['pipe', 'pipe', 'pipe'],
              cwd: tmpDir
            });
          } catch (e) {
            return e.stdout || e.stderr || e.message;
          }
        })();
        expect(output).toContain('.abapGitAgent config file not found');
        expect(output).toContain('ABAP upgrade requires configuration');
      } finally {
        fs.rmSync(tmpDir, { recursive: true, force: true });
      }
    });

    test('--match targets ABAP with CLI version (with mocked config)', () => {
      const output = runCommandWithMockConfig('upgrade --match --dry-run');
      expect(output).toContain('Target versions:');
      expect(output).toContain('ABAP:');

      // In Target versions section, CLI should not appear (only ABAP is targeted)
      const targetSection = output.split('Target versions:')[1].split('Would execute:')[0];
      expect(targetSection).not.toContain('CLI:');
    });
  });
});
