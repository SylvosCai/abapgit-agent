/**
 * Unit tests for exclude command
 */

const mockExecSync = jest.fn();
jest.mock('child_process', () => ({ execSync: mockExecSync }));

const mockGetExcludePatterns = jest.fn();
const mockIsExcluded = jest.fn();
jest.mock('../../src/config', () => ({
  getExcludePatterns: mockGetExcludePatterns,
  isExcluded: mockIsExcluded
}));

const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Exclude Command', () => {
  let excludeCommand;
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  beforeEach(() => {
    jest.resetModules();
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));
    mockGetExcludePatterns.mockReset();
    mockIsExcluded.mockReset();
    mockExecSync.mockReset();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('module has correct name', () => {
    const cmd = require('../../src/commands/exclude');
    expect(cmd.name).toBe('exclude');
  });

  test('does not require ABAP config', () => {
    const cmd = require('../../src/commands/exclude');
    expect(cmd.requiresAbapConfig).toBe(false);
  });

  test('prints help with --help flag', async () => {
    const cmd = require('../../src/commands/exclude');
    await cmd.execute(['--help']);
    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Usage/);
    expect(output).toMatch(/exclude/);
  });

  test('prints message when no patterns configured', async () => {
    mockGetExcludePatterns.mockReturnValue([]);
    const cmd = require('../../src/commands/exclude');
    await cmd.execute([]);
    const output = consoleOutput.join('\n');
    expect(output).toMatch(/No exclude patterns configured/);
  });

  test('prints message when no files match patterns', async () => {
    mockGetExcludePatterns.mockReturnValue(['zcl_*_dpc*.clas.*']);
    mockExecSync.mockReturnValue('abap/zcl_normal.clas.abap\nabap/zcl_other.intf.abap\n');
    mockIsExcluded.mockReturnValue(false);
    const cmd = require('../../src/commands/exclude');
    await cmd.execute([]);
    const output = consoleOutput.join('\n');
    expect(output).toMatch(/No tracked files match/);
  });

  test('removes matching files and commits', async () => {
    mockGetExcludePatterns.mockReturnValue(['zcl_*_dpc*.clas.*']);
    mockExecSync.mockImplementation((cmd) => {
      if (cmd === 'git ls-files') return 'abap/zcl_zsegw_dpc.clas.abap\nabap/zcl_normal.clas.abap\n';
      return '';
    });
    mockIsExcluded.mockImplementation((f) => f.includes('dpc'));
    const cmd = require('../../src/commands/exclude');
    await cmd.execute([]);
    const output = consoleOutput.join('\n');
    expect(output).toMatch(/zcl_zsegw_dpc\.clas\.abap/);
    expect(output).toMatch(/Removed 1 file/);
    expect(mockExecSync).toHaveBeenCalledWith(
      expect.stringMatching(/git rm --cached/),
      expect.anything()
    );
    expect(mockExecSync).toHaveBeenCalledWith(
      expect.stringMatching(/git commit/),
      expect.anything()
    );
  });

  test('exits on git ls-files failure', async () => {
    mockGetExcludePatterns.mockReturnValue(['*.fugr.*']);
    mockExecSync.mockImplementation(() => { throw new Error('not a repo'); });
    const cmd = require('../../src/commands/exclude');
    await expect(cmd.execute([])).rejects.toThrow('process.exit(1)');
    expect(consoleOutput.join('\n')).toMatch(/failed to list git-tracked files/);
  });
});
