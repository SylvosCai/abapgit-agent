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

  test('lists every registered command in the Commands section', async () => {
    const commands = require('../../src/commands');
    const versionCheck = require('../../src/utils/version-check');

    await commands.help.execute([], { versionCheck });

    const section = consoleOutput.join('\n').split('Commands:\n')[1].split('\nExamples:')[0];
    const listed = [...section.matchAll(/^  ([a-z][a-z0-9-]*)(?:\s|$)/gm)].map(match => match[1]);
    expect([...new Set(listed)].sort()).toEqual(Object.keys(commands).sort());
    expect(section).toContain('Subcommands: list, import, read, create, update, copy, drop, transport');
  });

  test('includes safe SO10 examples in the Examples section', async () => {
    const helpCommand = require('../../src/commands/help');
    const versionCheck = require('../../src/utils/version-check');

    await helpCommand.execute([], { versionCheck });

    const examples = consoleOutput.join('\n').split('\nExamples:\n')[1];
    expect(examples).toContain('abapgit-agent so10 list --prefix ZAPP_ --language EN');
    expect(examples).toContain('abapgit-agent so10 read ZAPP_NOTICE --language EN');
    expect(examples).toContain('abapgit-agent so10 import --prefix ZAPP_ --language EN --dry-run');
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
