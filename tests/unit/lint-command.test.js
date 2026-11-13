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
const mockFsReaddirSync = jest.fn();

jest.mock('child_process', () => ({
  execSync: (...args) => mockExecSync(...args),
  spawnSync: (...args) => mockSpawnSync(...args),
}));

jest.mock('fs', () => ({
  existsSync: (...args) => mockFsExistsSync(...args),
  readFileSync: (...args) => mockFsReadFileSync(...args),
  writeFileSync: (...args) => mockFsWriteFileSync(...args),
  unlinkSync: (...args) => mockFsUnlinkSync(...args),
  readdirSync: (...args) => mockFsReaddirSync(...args),
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
  process.exitCode = undefined;
});

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function setupConfigExists(content = '{"global":{"files":"/src/**/*.abap"},"rules":{}}') {
  mockFsExistsSync.mockReturnValue(true);
  mockFsReadFileSync.mockReturnValue(content);
  // readdirSync returns empty by default — no dep files to index
  mockFsReaddirSync.mockReturnValue([]);
}

function setupConfigMissing() {
  mockFsExistsSync.mockReturnValue(false);
  mockFsReaddirSync.mockReturnValue([]);
}

/**
 * Set up fs mocks from a list of known file paths.
 * - existsSync returns true only for listed paths
 * - readdirSync returns Dirent-like objects for files under a given dir
 */
function mockFileSystem(knownFiles) {
  mockFsExistsSync.mockImplementation((p) => knownFiles.includes(p));
  mockFsReaddirSync.mockImplementation((dir, opts) => {
    // Return flat Dirent-like entries for files directly under `dir`
    // (buildFileIndex only recurses into directories, so we return no subdirs here
    //  — all test files are in a single flat directory)
    return knownFiles
      .filter(f => f.startsWith(dir + '/') && !f.slice(dir.length + 1).includes('/'))
      .map(f => ({
        name: f.slice(dir.length + 1),
        isDirectory: () => false,
      }));
  });
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
    // existsSync: false for config, true for the changed .abap file (so it passes the filter)
    mockFsExistsSync.mockImplementation((p) => p.endsWith('.abap'));
    mockFsReaddirSync.mockReturnValue([]);
    mockExecSync.mockReturnValue('src/zcl_foo.clas.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    expect(() => lint.execute([])).toThrow('process.exit(1)');
    expect(errors.join(' ')).toMatch(/abaplint config not found/);
  });

  test('shows hint to run from project root', () => {
    mockFsExistsSync.mockImplementation((p) => p.endsWith('.abap'));
    mockFsReaddirSync.mockReturnValue([]);
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

    expect(logs.join('\n')).toMatch(/No changed lintable ABAP\/CDS\/RAP sources found/);
    expect(mockSpawnSync).not.toHaveBeenCalled();
  });

  test('filters out non-ABAP files from git diff output', () => {
    setupConfigExists();
    mockExecSync.mockReturnValue('src/foo.js\nsrc/bar.xml\n');

    lint.execute([]);

    expect(logs.join('\n')).toMatch(/No changed lintable ABAP\/CDS\/RAP sources found/);
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
    // Only config file exists — plain.abap does not exist on disk
    mockFsExistsSync.mockImplementation((p) => p === '.abaplint.json');
    mockFsReadFileSync.mockReturnValue('{"global":{"files":"/src/**/*.abap"},"rules":{}}');
    mockFsReaddirSync.mockReturnValue([]);
    // git diff returns a plain .abap file (only 2 dot-separated parts)
    mockExecSync.mockReturnValue('plain.abap\n');
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute([]);

    // isLintable passes plain.abap (ends with .abap) but existsSync returns false → filtered out
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
    // existsSync returns false for XML companions so the file list stays predictable
    mockFsExistsSync.mockImplementation((p) =>
      p === '.abaplint.json' || p.endsWith('.abap')
    );
    mockFsReadFileSync.mockReturnValue(JSON.stringify(originalConfig));
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
    // Simulate abaplint returning XML with an error entry
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json') return '{"global":{"files":"/src/**/*.abap"},"rules":{}}';
      if (p === '.abaplint-raw.xml') return `<?xml version="1.0"?><checkstyle version="8.0"><file name="src/zcl_foo.clas.abap"><error line="1" severity="error" message="msg" source="abaplint"/></file></checkstyle>`;
      return '';
    });
    mockSpawnSync.mockReturnValue({ status: 2 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    expect(process.exitCode).toBe(1);
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
    // Checkstyle path: abaplint runs to a temp file; our code then filters and writes --outfile.
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json') return '{"global":{"files":"/src/**/*.abap"},"rules":{}}';
      if (p === '.abaplint-raw.xml') return '<checkstyle version="8.0"></checkstyle>';
      return '';
    });

    lint.execute(['--files', 'src/zcl_foo.clas.abap', '--outformat', 'checkstyle', '--outfile', 'reports/results.xml']);

    const cmd = mockSpawnSync.mock.calls[0][0];
    // abaplint is invoked with the temp file, not the user's --outfile
    expect(cmd).toContain('--outfile .abaplint-raw.xml');
    // our code writes the filtered result to the user's outfile
    const writeCall = mockFsWriteFileSync.mock.calls.find(c => c[0] === 'reports/results.xml');
    expect(writeCall).toBeDefined();
  });

  test('does not include format flags when not specified', () => {
    setupConfigExists();
    mockSpawnSync.mockReturnValue({ status: 0 });

    lint.execute(['--files', 'src/zcl_foo.clas.abap']);

    // Interactive mode always runs abaplint via checkstyle internally (to filter output
    // to changed files only), then prints as text. The internal command always uses
    // --outformat checkstyle with the temp file, never the user-facing --outfile.
    const cmd = mockSpawnSync.mock.calls[0][0];
    expect(cmd).toContain('--outformat checkstyle');
    expect(cmd).toContain('--outfile .abaplint-raw.xml');
    expect(cmd).not.toContain('--outfile reports/');
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
    // existsSync returns false for XML companions to keep the file list predictable
    mockFsExistsSync.mockImplementation((p) =>
      p === '.abaplint.json' || p.endsWith('.abap')
    );
    mockFsReadFileSync.mockReturnValue('{"global":{"files":"/src/**/*.abap"},"rules":{}}');
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

// ---------------------------------------------------------------------------
// resolveDependencies — dynamic dependency resolution
// ---------------------------------------------------------------------------

describe('lint command — dependency resolution', () => {
  const abapDir = 'abap';
  const config = { global: { files: '/abap/**/*.abap' }, rules: {} };

  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
    mockSpawnSync.mockReturnValue({ status: 0 });
  });

  test('includes XML companion of changed file when it exists', () => {
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zcl_foo.clas.xml'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : 'CLASS zcl_foo DEFINITION. ENDCLASS.'
    );

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zcl_foo.clas.abap');
    expect(writtenConfig.global.files).toContain('/abap/zcl_foo.clas.xml');
  });

  test('does not add XML companion when it does not exist', () => {
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : 'CLASS zcl_foo DEFINITION. ENDCLASS.'
    );

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).not.toContain('/abap/zcl_foo.clas.xml');
  });

  test('resolves INTERFACES statement to .intf.abap + .intf.xml', () => {
    const source = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_bar.\nENDCLASS.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zif_bar.intf.abap', 'abap/zif_bar.intf.xml'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zif_bar.intf.abap');
    expect(writtenConfig.global.files).toContain('/abap/zif_bar.intf.xml');
  });

  test('resolves INHERITING FROM to superclass .clas.abap + .clas.xml', () => {
    const source = 'CLASS zcl_child DEFINITION INHERITING FROM zcl_parent. ENDCLASS.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_child.clas.abap', 'abap/zcl_parent.clas.abap', 'abap/zcl_parent.clas.xml'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    lint.execute(['--files', 'abap/zcl_child.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zcl_parent.clas.abap');
    expect(writtenConfig.global.files).toContain('/abap/zcl_parent.clas.xml');
  });

  test('resolves TYPE REF TO to interface file when it exists', () => {
    const source = 'DATA lo_obj TYPE REF TO zif_agent.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zif_agent.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zif_agent.intf.abap');
  });

  test('resolves TYPE REF TO to class file when interface does not exist but class does', () => {
    const source = 'DATA lo_obj TYPE REF TO zcl_helper.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zcl_helper.clas.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zcl_helper.clas.abap');
    expect(writtenConfig.global.files).not.toContain('/abap/zcl_helper.intf.abap');
  });

  test('silently skips referenced objects that do not exist on disk', () => {
    const source = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_missing.\nENDCLASS.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    expect(() => lint.execute(['--files', 'abap/zcl_foo.clas.abap'])).not.toThrow();
    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).not.toContain('/abap/zif_missing.intf.abap');
  });

  test('deduplicates dependency files when multiple changed files share the same dep', () => {
    const source = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_shared.\nENDCLASS.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_a.clas.abap', 'abap/zcl_b.clas.abap', 'abap/zif_shared.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    lint.execute(['--files', 'abap/zcl_a.clas.abap,abap/zcl_b.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    const depCount = writtenConfig.global.files.filter(f => f === '/abap/zif_shared.intf.abap').length;
    expect(depCount).toBe(1);
  });

  test('includes all changed files in global.files alongside resolved deps', () => {
    const source = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_bar.\nENDCLASS.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zif_bar.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) =>
      p === '.abaplint.json' ? JSON.stringify(config) : source
    );

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zcl_foo.clas.abap');
    expect(writtenConfig.global.files).toContain('/abap/zif_bar.intf.abap');
  });

  test('resolves dependencies transitively (level-2 deps pulled in)', () => {
    // zcl_foo INTERFACES zif_bar → zif_bar itself has TYPE REF TO zif_baz
    // zif_baz must appear even though zcl_foo never references it directly
    const clsSource  = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_bar.\nENDCLASS.';
    const intfSource = 'INTERFACE zif_bar PUBLIC.\n  METHODS run IMPORTING io_x TYPE REF TO zif_baz.\nENDINTERFACE.';
    const bazSource  = 'INTERFACE zif_baz PUBLIC. ENDINTERFACE.';

    const knownFiles = [
      '.abaplint.json',
      'abap/zcl_foo.clas.abap',
      'abap/zif_bar.intf.abap',
      'abap/zif_baz.intf.abap',
    ];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json')        return JSON.stringify(config);
      if (p === 'abap/zcl_foo.clas.abap') return clsSource;
      if (p === 'abap/zif_bar.intf.abap') return intfSource;
      if (p === 'abap/zif_baz.intf.abap') return bazSource;
      return '';
    });

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zcl_foo.clas.abap');
    expect(writtenConfig.global.files).toContain('/abap/zif_bar.intf.abap');
    expect(writtenConfig.global.files).toContain('/abap/zif_baz.intf.abap');
  });

  test('handles circular dependencies without infinite loop', () => {
    // zif_a TYPE REF TO zif_b, zif_b TYPE REF TO zif_a
    const aSource = 'INTERFACE zif_a PUBLIC.\n  METHODS m IMPORTING io TYPE REF TO zif_b.\nENDINTERFACE.';
    const bSource = 'INTERFACE zif_b PUBLIC.\n  METHODS m IMPORTING io TYPE REF TO zif_a.\nENDINTERFACE.';

    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zif_a.intf.abap', 'abap/zif_b.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json')        return JSON.stringify(config);
      if (p === 'abap/zcl_foo.clas.abap') return 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_a.\nENDCLASS.';
      if (p === 'abap/zif_a.intf.abap')  return aSource;
      if (p === 'abap/zif_b.intf.abap')  return bSource;
      return '';
    });

    expect(() => lint.execute(['--files', 'abap/zcl_foo.clas.abap'])).not.toThrow();
    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zif_a.intf.abap');
    expect(writtenConfig.global.files).toContain('/abap/zif_b.intf.abap');
  });

  test('includes concrete implementation class alongside resolved interface (zif_→zcl_)', () => {
    // When zif_foo is resolved, also include zcl_foo if it exists —
    // needed so rules like unused_variables can fully type-check method bodies.
    const clsSource  = 'CLASS zcl_bar DEFINITION.\n  INTERFACES zif_foo.\nENDCLASS.';
    const intfSource = 'INTERFACE zif_foo PUBLIC. ENDINTERFACE.';
    const implSource = 'CLASS zcl_foo DEFINITION. ENDCLASS.';

    const knownFiles = [
      '.abaplint.json',
      'abap/zcl_bar.clas.abap',
      'abap/zif_foo.intf.abap',
      'abap/zcl_foo.clas.abap',  // concrete impl of zif_foo
    ];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json')        return JSON.stringify(config);
      if (p === 'abap/zcl_bar.clas.abap') return clsSource;
      if (p === 'abap/zif_foo.intf.abap') return intfSource;
      if (p === 'abap/zcl_foo.clas.abap') return implSource;
      return '';
    });

    lint.execute(['--files', 'abap/zcl_bar.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/abap/zif_foo.intf.abap');
    expect(writtenConfig.global.files).toContain('/abap/zcl_foo.clas.abap');
  });

  test('skips concrete impl lookup when no matching zcl_ class exists', () => {
    const clsSource  = 'CLASS zcl_bar DEFINITION.\n  INTERFACES zif_external.\nENDCLASS.';
    const intfSource = 'INTERFACE zif_external PUBLIC. ENDINTERFACE.';

    const knownFiles = ['.abaplint.json', 'abap/zcl_bar.clas.abap', 'abap/zif_external.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json')           return JSON.stringify(config);
      if (p === 'abap/zcl_bar.clas.abap')  return clsSource;
      if (p === 'abap/zif_external.intf.abap') return intfSource;
      return '';
    });

    expect(() => lint.execute(['--files', 'abap/zcl_bar.clas.abap'])).not.toThrow();
    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    // zcl_external does not exist — should not appear
    expect(writtenConfig.global.files).not.toContain('/abap/zcl_external.clas.abap');
    expect(writtenConfig.global.files).toContain('/abap/zif_external.intf.abap');
  });

  test('resolves deps in nested subdirectories (non-flat project layout)', () => {
    // Simulates a project like aud-abap where files live in deep subdirs
    // e.g. src/module/pkg/cl_foo.clas.abap  INHERITING FROM  cl_base.clas.abap
    // cl_base lives in a different subdir: src/shared/cl_base.clas.abap
    const clsSource = 'CLASS cl_child DEFINITION INHERITING FROM cl_base. ENDCLASS.';
    const baseSource = 'CLASS cl_base DEFINITION. ENDCLASS.';

    // readdirSync will be called for 'src', 'src/module', 'src/shared'
    mockFsExistsSync.mockImplementation((p) => [
      '.abaplint.json',
      'src/module/cl_child.clas.abap',
      'src/shared/cl_base.clas.abap',
    ].includes(p));
    mockFsReaddirSync.mockImplementation((dir) => {
      if (dir === 'src') return [
        { name: 'module', isDirectory: () => true },
        { name: 'shared', isDirectory: () => true },
      ];
      if (dir === 'src/module') return [
        { name: 'cl_child.clas.abap', isDirectory: () => false },
      ];
      if (dir === 'src/shared') return [
        { name: 'cl_base.clas.abap', isDirectory: () => false },
      ];
      return [];
    });
    const nestedConfig = { global: { files: '/src/**/*.abap' }, rules: {} };
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json')             return JSON.stringify(nestedConfig);
      if (p === 'src/module/cl_child.clas.abap') return clsSource;
      if (p === 'src/shared/cl_base.clas.abap')  return baseSource;
      return '';
    });

    lint.execute(['--files', 'src/module/cl_child.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    expect(writtenConfig.global.files).toContain('/src/module/cl_child.clas.abap');
    expect(writtenConfig.global.files).toContain('/src/shared/cl_base.clas.abap');
  });

  test('includes dependency files in global.files for cross-reference resolution', () => {
    const source = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_bar.\nENDCLASS.';
    const intfSource = 'INTERFACE zif_bar PUBLIC. ENDINTERFACE.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zif_bar.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json') return JSON.stringify(config);
      if (p === 'abap/zcl_foo.clas.abap') return source;
      if (p === 'abap/zif_bar.intf.abap') return intfSource;
      return '';
    });

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    // changed file is in global.files and NOT excluded
    expect(writtenConfig.global.files).toContain('/abap/zcl_foo.clas.abap');
    expect(writtenConfig.global.exclude || []).not.toContain('/abap/zcl_foo.clas.abap');
    // dep is in global.files for cross-reference resolution and NOT excluded
    expect(writtenConfig.global.files).toContain('/abap/zif_bar.intf.abap');
    expect(writtenConfig.global.exclude || []).not.toContain('/abap/zif_bar.intf.abap');
  });

  test('preserves existing global.exclude entries without adding dep files', () => {
    const configWithExclude = { global: { files: '/abap/**/*.abap', exclude: ['/abap/generated/**'] }, rules: {} };
    const source = 'CLASS zcl_foo DEFINITION.\n  INTERFACES zif_bar.\nENDCLASS.';
    const knownFiles = ['.abaplint.json', 'abap/zcl_foo.clas.abap', 'abap/zif_bar.intf.abap'];
    mockFileSystem(knownFiles);
    mockFsReadFileSync.mockImplementation((p) => {
      if (p === '.abaplint.json') return JSON.stringify(configWithExclude);
      if (p === 'abap/zcl_foo.clas.abap') return source;
      if (p === 'abap/zif_bar.intf.abap') return 'INTERFACE zif_bar PUBLIC. ENDINTERFACE.';
      return '';
    });

    lint.execute(['--files', 'abap/zcl_foo.clas.abap']);

    const writtenConfig = JSON.parse(mockFsWriteFileSync.mock.calls[0][1]);
    // existing exclude entry is preserved
    expect(writtenConfig.global.exclude).toContain('/abap/generated/**');
    // dep file is NOT added to exclude (must be parseable for cross-reference)
    expect(writtenConfig.global.exclude).not.toContain('/abap/zif_bar.intf.abap');
  });
});

// Exercise the real command: RAP detection, XML companions, parser rules, and runner failures.
describe('lint command — RAP sources', () => {
  const bdef = 'src/zbehavior.bdef.asbdef';
  const ddlx = 'src/zannotations.ddlx.asddlxs';
  const config = { global: { files: '/src/**/*.abap' }, rules: {} };

  function setupRap(cfg = config) {
    const files = [bdef, ddlx, 'src/zbehavior.bdef.xml', 'src/zannotations.ddlx.xml'];
    mockFileSystem(['.abaplint.json', '.abaplint-raw.xml', ...files]);
    mockFsReadFileSync.mockImplementation(p => p === '.abaplint.json'
      ? JSON.stringify(cfg) : p === '.abaplint-raw.xml' ? '<checkstyle version="8.0"></checkstyle>' : '');
    mockSpawnSync.mockReturnValue({ status: 0 });
  }

  function scopedConfig() {
    return JSON.parse(mockFsWriteFileSync.mock.calls.find(([p]) => p === '.abaplint-local.json')[1]);
  }

  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
    setupRap();
  });

  test.each([
    [bdef, '/src/zbehavior.bdef.xml', 'bdef_parser_error'],
    [ddlx, '/src/zannotations.ddlx.xml', 'cds_parser_error'],
  ])('lints %s with its XML companion and parser rule', (file, xml, rule) => {
    lint.execute(['--files', file]);
    expect(scopedConfig().global.files).toEqual([`/${file}`, xml]);
    expect(scopedConfig().rules[rule]).toBe(true);
    expect(process.exitCode).toBeUndefined();
  });

  test('keeps explicit parser-rule overrides', () => {
    setupRap({ ...config, rules: { bdef_parser_error: false, cds_parser_error: false } });
    lint.execute(['--files', `${bdef},${ddlx}`]);
    expect(scopedConfig().rules).toEqual({ bdef_parser_error: false, cds_parser_error: false });
  });

  test.each(['base', 'PR', 'uncommitted', 'last commit'])('detects RAP-only changes for %s', mode => {
    if (mode === 'PR') process.env.CHANGE_TARGET = 'main';
    const diff = `${bdef}\n${ddlx}\nsrc/deleted.bdef.asbdef\nsrc/zservice.srvd.srvdsrv`;
    if (mode === 'last commit') mockExecSync.mockReturnValueOnce('').mockReturnValueOnce(diff);
    else mockExecSync.mockReturnValue(diff);
    lint.execute(mode === 'base' ? ['--base', 'main'] : []);
    expect(scopedConfig().global.files).toContain(`/${bdef}`);
    expect(scopedConfig().global.files).toContain(`/${ddlx}`);
    expect(scopedConfig().global.files).not.toContain('/src/deleted.bdef.asbdef');
    expect(scopedConfig().global.files).not.toContain('/src/zservice.srvd.srvdsrv');
    for (const [cmd] of mockExecSync.mock.calls) {
      expect(cmd).toContain("'*.asbdef'");
      expect(cmd).toContain("'*.asddlxs'");
    }
  });

  test.each(['src/zservice.srvd.srvdsrv', 'src/zservice.srvd.assrvd', 'src/zrole.dcls.asdcls',
    'src/wrong.srvd.asbdef', 'src/wrong.bdef.asddlxs'])('does not lint unsupported source %s', file => {
    mockFsExistsSync.mockReturnValue(true);
    lint.execute(['--files', file]);
    expect(mockSpawnSync).not.toHaveBeenCalled();
  });

  test('includes uppercase native source and its indexed XML', () => {
    mockFileSystem(['.abaplint.json', '.abaplint-raw.xml', 'src/ZBEHAVIOR.BDEF.ASBDEF', 'src/zbehavior.bdef.xml']);
    lint.execute(['--files', 'src/ZBEHAVIOR.BDEF.ASBDEF']);
    expect(scopedConfig().global.files).toContain('/src/zbehavior.bdef.xml');
    expect(scopedConfig().rules.bdef_parser_error).toBe(true);
  });

  test.each([
    { status: 2, stderr: Buffer.from('Unknown rule: bdef_parser_error') },
    { status: null, signal: 'SIGTERM' },
    { status: null, error: new Error('spawn failed') },
  ])('fails closed when abaplint cannot run: %j', result => {
    mockSpawnSync.mockReturnValue(result);
    lint.execute(['--files', bdef]);
    expect(process.exitCode).toBe(1);
    expect(mockFsUnlinkSync).toHaveBeenCalledWith('.abaplint-local.json');
    expect(console.error).toHaveBeenCalled();
  });

  test('fails closed when abaplint leaves no report, even with exit 0', () => {
    mockFsExistsSync.mockImplementation(p => p !== '.abaplint-raw.xml');
    lint.execute(['--files', bdef]);
    expect(process.exitCode).toBe(1);
    expect(console.error).toHaveBeenCalled();
  });

  test('retains RAP parser diagnostics and exits 1', () => {
    mockSpawnSync.mockReturnValue({ status: 1 });
    mockFsReadFileSync.mockImplementation(p => p === '.abaplint.json' ? JSON.stringify(config)
      : p === '.abaplint-raw.xml'
        ? `<checkstyle version="8.0"><file name="${bdef}"><error line="1" severity="error" message="Parse error" source="bdef_parser_error"/></file></checkstyle>` : '');
    lint.execute(['--files', bdef, '--outformat', 'checkstyle', '--outfile', 'report.xml']);
    expect(mockFsWriteFileSync.mock.calls.find(([p]) => p === 'report.xml')[1]).toContain('bdef_parser_error');
    expect(process.exitCode).toBe(1);
  });
});
