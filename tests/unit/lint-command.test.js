/**
 * Unit tests for lint command
 * Tests file detection logic, config scoping, error handling, and output.
 */

// Mock child_process with stable function references that lint.js will capture
// via destructuring at require time.
const mockExecSync = jest.fn();
const mockSpawnSync = jest.fn();
const mockFsExistsSync = jest.fn();
const mockFsReadFileSync = jest.fn();
const mockFsWriteFileSync = jest.fn();
const mockFsUnlinkSync = jest.fn();

jest.mock('child_process', () => ({
  execSync: (...args) => mockExecSync(...args),
  spawnSync: (...args) => mockSpawnSync(...args),
}));

jest.mock('fs', () => ({
  existsSync: (...args) => mockFsExistsSync(...args),
  readFileSync: (...args) => mockFsReadFileSync(...args),
  writeFileSync: (...args) => mockFsWriteFileSync(...args),
  unlinkSync: (...args) => mockFsUnlinkSync(...args),
}));

// Mock process.exit
jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

// Require lint once — it captures the mock functions above via destructuring
const lint = require('../../src/commands/lint');

const originalEnv = process.env;

beforeEach(() => {
  jest.clearAllMocks();
  process.env = { ...originalEnv };
  delete process.env.CHANGE_TARGET;
  process.exitCode = undefined;
});

afterEach(() => {
  process.env = originalEnv;
});

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function setupConfigExists(content = '{"global":{"files":"/src/**/*.abap"},"rules":{}}') {
  mockFsExistsSync.mockReturnValue(true);
  mockFsReadFileSync.mockReturnValue(content);
}

function setupConfigMissing() {
  mockFsExistsSync.mockReturnValue(false);
}

// ---------------------------------------------------------------------------
// Metadata
// ---------------------------------------------------------------------------

describe('lint command — metadata', () => {
  test('has correct name', () => {
    expect(lint.name).toBe('lint');
  });

  test('does not require ABAP config', () => {
    expect(lint.requiresAbapConfig).toBe(false);
  });

  test('has execute function', () => {
    expect(typeof lint.execute).toBe('function');
  });
});

// ---------------------------------------------------------------------------
// Missing .abaplint.json
// ---------------------------------------------------------------------------

describe('lint command — missing config', () => {
  let logs, errors;

  beforeEach(() => {
    logs = []; errors = [];
    jest.spyOn(console, 'log').mockImplementation((...a) => logs.push(a.join(' ')));
    jest.spyOn(console, 'error').mockImplementation((...a) => errors.push(a.join(' ')));
  });

  test('exits with error when .abaplint.json not found', () => {
    setupConfigMissing();
    mockExecSync.mockReturnValue('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    expect(() => lint.execute([])).toThrow('process.exit(1)');
    expect(errors.join(' ')).toMatch(/abaplint config not found/);
  });

  test('shows hint to run from project root', () => {
    setupConfigMissing();
    mockExecSync.mockReturnValue('src/zcl_foo.clas.abap\n');

    expect(() => lint.execute([])).toThrow('process.exit(1)');
    expect(errors.join(' ')).toMatch(/project root/);
  });

  test('accepts custom config path via --config', () => {
    // Custom config exists, default does not
    mockFsExistsSync.mockImplementation((p) => p === 'custom/.abaplint.json');
    mockFsReadFileSync.mockReturnValue('{"global":{"files":"/src/**/*.abap"},"rules":{}}');
    mockExecSync.mockReturnValue('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    expect(() => lint.execute(['--config', 'custom/.abaplint.json'])).not.toThrow();
  });
});

// ---------------------------------------------------------------------------
// No changed files
// ---------------------------------------------------------------------------

describe('lint command — no changed files', () => {
  let logs;

  beforeEach(() => {
    logs = [];
    jest.spyOn(console, 'log').mockImplementation((...a) => logs.push(a.join(' ')));
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  test('prints nothing-to-lint message and returns without running abaplint', () => {
    setupConfigExists();
    mockExecSync.mockReturnValue('');

    lint.execute([]);

    expect(logs.join('\n')).toMatch(/No changed .abap files found/);
    expect(mockSpawnSync).not.toHaveBeenCalled();
  });

  test('filters out non-ABAP files from git diff output', () => {
    setupConfigExists();
    mockExecSync.mockReturnValue('src/foo.js\nsrc/bar.xml\n');

    lint.execute([]);

    expect(logs.join('\n')).toMatch(/No changed .abap files found/);
    expect(mockSpawnSync).not.toHaveBeenCalled();
  });
});

// ---------------------------------------------------------------------------
// filterAbapFiles — file pattern validation
// ---------------------------------------------------------------------------

describe('lint command — filterAbapFiles patterns', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  function lintedFiles(filesArg) {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', filesArg]);

    if (mockSpawnSync.mock.calls.length === 0) return [];
    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    return writtenConfig.global.files;
  }

  test('accepts .clas.abap files', () => {
    const files = lintedFiles('src/zcl_foo.clas.abap');
    expect(files).toContain('/src/zcl_foo.clas.abap');
  });

  test('accepts .intf.abap files', () => {
    const files = lintedFiles('src/zif_foo.intf.abap');
    expect(files).toContain('/src/zif_foo.intf.abap');
  });

  test('accepts .clas.testclasses.abap files', () => {
    const files = lintedFiles('src/zcl_foo.clas.testclasses.abap');
    expect(files).toContain('/src/zcl_foo.clas.testclasses.abap');
  });

  test('accepts .prog.abap files', () => {
    const files = lintedFiles('src/zmy_prog.prog.abap');
    expect(files).toContain('/src/zmy_prog.prog.abap');
  });

  test('rejects plain .abap files from git diff (no type segment)', () => {
    setupConfigExists();
    // git diff returns a plain .abap file (only 2 dot-separated parts)
    mockExecSync.mockReturnValue('plain.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute([]);

    // filterAbapFiles removes plain.abap (only 2 parts) → nothing to lint → spawnSync not called
    expect(mockSpawnSync).not.toHaveBeenCalled();
  });
});

// ---------------------------------------------------------------------------
// Config scoping
// ---------------------------------------------------------------------------

describe('lint command — config scoping', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  test('overrides global.files with resolved file list', () => {
    const originalConfig = {
      global: { files: '/src/**/*.abap' },
      rules: { keyword_case: { style: 'upper' } }
    };
    setupConfigExists(JSON.stringify(originalConfig));
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap,src/zif_bar.intf.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toEqual(['/src/zcl_foo.clas.abap', '/src/zif_bar.intf.abap']);
    expect(writtenConfig.rules).toEqual(originalConfig.rules);
  });

  test('writes scoped config to .abaplint-local.json', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(mockFsWriteFileSync.mock.calls[0][0]).toBe('.abaplint-local.json');
  });

  test('prefixes each file path with /', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files[0]).toBe('/src/zcl_foo.clas.abap');
  });
});

// ---------------------------------------------------------------------------
// Temp file cleanup
// ---------------------------------------------------------------------------

describe('lint command — temp file cleanup', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  test('deletes .abaplint-local.json after successful run', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(mockFsUnlinkSync).toHaveBeenCalledWith('.abaplint-local.json');
  });

  test('deletes .abaplint-local.json even when abaplint reports issues (non-zero exit)', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 1 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(mockFsUnlinkSync).toHaveBeenCalledWith('.abaplint-local.json');
  });
});

// ---------------------------------------------------------------------------
// Exit code propagation
// ---------------------------------------------------------------------------

describe('lint command — exit code', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  test('sets process.exitCode when abaplint exits non-zero', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 2 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(process.exitCode).toBe(2);
  });

  test('does not set process.exitCode when abaplint exits 0', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(process.exitCode).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// Output format flags
// ---------------------------------------------------------------------------

describe('lint command — output format flags', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  test('passes --outformat to abaplint command', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap', '--outformat', 'checkstyle']);

    const cmd = mockSpawnSync.mock.calls[0][0];
    expect(cmd).toContain('--outformat checkstyle');
  });

  test('passes --outfile to abaplint command', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap', '--outformat', 'checkstyle', '--outfile', 'reports/results.xml']);

    const cmd = mockSpawnSync.mock.calls[0][0];
    expect(cmd).toContain('--outfile reports/results.xml');
  });

  test('does not include format flags when not specified', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    const cmd = mockSpawnSync.mock.calls[0][0];
    expect(cmd).not.toContain('--outformat');
    expect(cmd).not.toContain('--outfile');
  });

  test('suppresses file list output when --outfile is set', () => {
    const logs = [];
    jest.spyOn(console, 'log').mockImplementation((...a) => logs.push(a.join(' ')));
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap', '--outformat', 'checkstyle', '--outfile', 'out.xml']);

    expect(logs.join('\n')).not.toMatch(/Linting \d+ file/);
  });
});

// ---------------------------------------------------------------------------
// File detection — git diff scenarios
// ---------------------------------------------------------------------------

describe('lint command — file detection via git diff', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  test('uses CHANGE_TARGET env var for PR builds', () => {
    process.env.CHANGE_TARGET = 'main';
    setupConfigExists();
    mockExecSync.mockReturnValue('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute([]);

    const diffCmd = mockExecSync.mock.calls[0][0];
    expect(diffCmd).toContain('origin/main');
    expect(diffCmd).toContain('HEAD');
  });

  test('uses --base argument when provided', () => {
    setupConfigExists();
    mockExecSync.mockReturnValue('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--base', 'develop']);

    const diffCmd = mockExecSync.mock.calls[0][0];
    expect(diffCmd).toContain('develop');
    expect(diffCmd).toContain('HEAD');
  });

  test('falls back to HEAD~1..HEAD when no base and no CHANGE_TARGET', () => {
    setupConfigExists();
    // First call (uncommitted changes) returns empty; second call (HEAD~1) returns file
    mockExecSync
      .mockReturnValueOnce('')
      .mockReturnValueOnce('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute([]);

    const calls = mockExecSync.mock.calls.map(c => c[0]);
    expect(calls.some(cmd => cmd.includes('HEAD~1'))).toBe(true);
  });

  test('uses uncommitted changes when present (no base, no CHANGE_TARGET)', () => {
    setupConfigExists();
    // First call (uncommitted) returns a file — should use this, not fall back
    mockExecSync.mockReturnValueOnce('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute([]);

    // spawnSync called means a file was found and linting ran
    expect(mockSpawnSync).toHaveBeenCalled();
    // HEAD~1 diff should NOT have been called
    const calls = mockExecSync.mock.calls.map(c => c[0]);
    expect(calls.some(cmd => cmd.includes('HEAD~1'))).toBe(false);
  });

  test('--files bypasses git diff entirely', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(mockExecSync).not.toHaveBeenCalled();
  });

  test('--files filters out non-.abap entries', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap,src/bar.xml,src/baz.js']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toEqual(['/src/zcl_foo.clas.abap']);
  });
});

// ---------------------------------------------------------------------------
// Output — linting N file(s) message
// ---------------------------------------------------------------------------

describe('lint command — console output', () => {
  test('prints file list when linting', () => {
    const logs = [];
    jest.spyOn(console, 'log').mockImplementation((...a) => logs.push(a.join(' ')));
    jest.spyOn(console, 'error').mockImplementation(() => {});
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap,src/zif_bar.intf.abap']);

    const output = logs.join('\n');
    expect(output).toMatch(/Linting 2 file\(s\)/);
    expect(output).toContain('src/zcl_foo.clas.abap');
    expect(output).toContain('src/zif_bar.intf.abap');
  });
});
