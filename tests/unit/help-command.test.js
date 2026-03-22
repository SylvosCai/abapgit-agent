/**
 * Unit tests for help command
 * Tests that help output includes version and expected content
 */

describe('Help Command', () => {
  let consoleOutput;
  let originalConsoleLog;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    console.log = (...args) => consoleOutput.push(args.join(' '));
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    jest.resetModules();
  });

  test('includes version number in output', async () => {
    const helpCommand = require('../../src/commands/help');
    const versionCheck = require('../../src/utils/version-check');
    const version = versionCheck.getCliVersion();

    await helpCommand.execute([], { versionCheck });

    const output = consoleOutput.join('\n');
    expect(output).toContain(`v${version}`);
  });

  test('includes "ABAP Git Agent" in output', async () => {
    const helpCommand = require('../../src/commands/help');
    const versionCheck = require('../../src/utils/version-check');

    await helpCommand.execute([], { versionCheck });

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/ABAP Git Agent/);
  });

  test('lists key commands in output', async () => {
    const helpCommand = require('../../src/commands/help');
    const versionCheck = require('../../src/utils/version-check');

    await helpCommand.execute([], { versionCheck });

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/pull/);
    expect(output).toMatch(/syntax/);
    expect(output).toMatch(/init/);
  });
});
