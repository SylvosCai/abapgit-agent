/**
 * Unit tests for guide command
 */

const fs = require('fs');
const path = require('path');

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

// Mock console methods
const originalError = console.error;
const originalLog = console.log;
let consoleErrorOutput = [];
let consoleLogOutput = [];

beforeEach(() => {
  consoleErrorOutput = [];
  consoleLogOutput = [];
  console.error = jest.fn((...args) => consoleErrorOutput.push(args.join(' ')));
  console.log = jest.fn((...args) => consoleLogOutput.push(args.join(' ')));
});

afterEach(() => {
  console.error = originalError;
  console.log = originalLog;
  jest.clearAllMocks();
  jest.resetModules();
});

describe('guide command', () => {
  let guideCommand;

  beforeEach(() => {
    jest.resetModules();
    guideCommand = require('../../src/commands/guide');
  });

  test('has correct metadata', () => {
    expect(guideCommand.name).toBe('guide');
    expect(guideCommand.requiresAbapConfig).toBe(false);
    expect(typeof guideCommand.execute).toBe('function');
  });

  test('--path prints the path to bundled CLAUDE.md', async () => {
    await guideCommand.execute(['--path']);

    const output = consoleLogOutput.join('\n');
    expect(output).toContain('CLAUDE.md');
    expect(output).toContain('abap');
    // Should be an absolute path that exists
    const printedPath = consoleLogOutput[0];
    expect(fs.existsSync(printedPath)).toBe(true);
  });

  test('--json outputs valid JSON with path and content', async () => {
    await guideCommand.execute(['--json']);

    const output = consoleLogOutput.join('');
    const parsed = JSON.parse(output);
    expect(parsed).toHaveProperty('path');
    expect(parsed).toHaveProperty('content');
    expect(typeof parsed.path).toBe('string');
    expect(typeof parsed.content).toBe('string');
    expect(parsed.content.length).toBeGreaterThan(0);
    expect(fs.existsSync(parsed.path)).toBe(true);
  });

  test('default mode prints guide content to stdout', async () => {
    await guideCommand.execute([]);

    const output = consoleLogOutput.join('\n');
    // Should be non-empty content from CLAUDE.md
    expect(output.length).toBeGreaterThan(100);
  });

  test('bundled CLAUDE.md exists on disk', () => {
    const candidates = [
      path.join(__dirname, '..', '..', 'abap', 'CLAUDE.md'),
      path.join(__dirname, '..', '..', '..', 'abap', 'CLAUDE.md')
    ];
    const found = candidates.some(p => fs.existsSync(p));
    expect(found).toBe(true);
  });

  test('exits with error when guide file not found', async () => {
    // Override fs.existsSync to simulate missing file
    const originalExists = fs.existsSync;
    jest.spyOn(fs, 'existsSync').mockReturnValue(false);

    try {
      await expect(guideCommand.execute([])).rejects.toThrow('process.exit(1)');
      expect(consoleErrorOutput.join(' ')).toContain('not found');
    } finally {
      fs.existsSync = originalExists;
    }
  });
});

describe('guide command — init creates slim CLAUDE.md stub', () => {
  const { execSync } = require('child_process');
  const os = require('os');
  let testDir;

  beforeEach(() => {
    testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-guide-test-'));
  });

  afterEach(() => {
    if (testDir && fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  test('init creates slim CLAUDE.md (not the full guide)', () => {
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    const claudeMdPath = path.join(testDir, 'CLAUDE.md');
    expect(fs.existsSync(claudeMdPath)).toBe(true);

    const content = fs.readFileSync(claudeMdPath, 'utf8');
    // Slim stub should reference the guide command
    expect(content).toContain('abapgit-agent guide');
    // Slim stub should NOT contain the full development guide content
    // (the full guide has this distinctive heading)
    expect(content).not.toContain('## Step 1');
  });

  test('init creates only objects.local.md in guidelines/ (not 18 files)', () => {
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    const guidelinesPath = path.join(testDir, 'guidelines');
    expect(fs.existsSync(guidelinesPath)).toBe(true);

    const files = fs.readdirSync(guidelinesPath).filter(f => f.endsWith('.md'));
    // Should only have objects.local.md — no copies of the 18 standard files
    expect(files).toEqual(['objects.local.md']);
  });

  test('init skips CLAUDE.md creation when it already exists', () => {
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    const claudeMdPath = path.join(testDir, 'CLAUDE.md');
    const existingContent = '# My Existing Claude Instructions\nDo not overwrite me.';
    fs.writeFileSync(claudeMdPath, existingContent);

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // File must not be overwritten
    expect(fs.readFileSync(claudeMdPath, 'utf8')).toBe(existingContent);
  });
});

describe('guide --migrate', () => {
  const os = require('os');
  let testDir;
  let guideCommand;

  beforeEach(() => {
    testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-migrate-test-'));
    jest.resetModules();
    guideCommand = require('../../src/commands/guide');
    // Override cwd to testDir for all migrate tests
    jest.spyOn(process, 'cwd').mockReturnValue(testDir);
  });

  afterEach(() => {
    jest.restoreAllMocks();
    jest.resetModules();
    if (testDir && fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  function writeGuidelinesDir(files) {
    const dir = path.join(testDir, 'guidelines');
    fs.mkdirSync(dir, { recursive: true });
    for (const [name, content] of Object.entries(files)) {
      fs.writeFileSync(path.join(dir, name), content);
    }
    return dir;
  }

  function writeClaudeMd(content) {
    fs.writeFileSync(path.join(testDir, 'CLAUDE.md'), content);
  }

  test('--dry-run prints preview but makes no changes', async () => {
    const bundledNames = guideCommand._getBundledGuidelineNames();
    const [firstBundled] = [...bundledNames];
    writeGuidelinesDir({ [firstBundled]: '# Standard file', 'objects.local.md': '# Local' });
    writeClaudeMd('# Claude Code Instructions\nFull guide content here.');

    await guideCommand.execute(['--migrate', '--dry-run', '--yes']);

    // Nothing should be deleted
    expect(fs.existsSync(path.join(testDir, 'guidelines', firstBundled))).toBe(true);
    expect(fs.existsSync(path.join(testDir, 'guidelines', 'objects.local.md'))).toBe(true);
    // CLAUDE.md should not be changed
    expect(fs.readFileSync(path.join(testDir, 'CLAUDE.md'), 'utf8')).toContain('Full guide content here.');

    const output = consoleLogOutput.join('\n');
    expect(output).toContain('Dry run');
  });

  test('removes standard bundled files, keeps *.local.md', async () => {
    const bundledNames = guideCommand._getBundledGuidelineNames();
    const [fileA, fileB] = [...bundledNames];
    writeGuidelinesDir({
      [fileA]: '# Standard A',
      [fileB]: '# Standard B',
      'objects.local.md': '# My naming conventions'
    });

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.existsSync(path.join(testDir, 'guidelines', fileA))).toBe(false);
    expect(fs.existsSync(path.join(testDir, 'guidelines', fileB))).toBe(false);
    expect(fs.existsSync(path.join(testDir, 'guidelines', 'objects.local.md'))).toBe(true);
  });

  test('removes guidelines/ directory when no project files remain', async () => {
    const bundledNames = guideCommand._getBundledGuidelineNames();
    const [firstBundled] = [...bundledNames];
    writeGuidelinesDir({ [firstBundled]: '# Standard file' });

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.existsSync(path.join(testDir, 'guidelines'))).toBe(false);
  });

  test('keeps guidelines/ directory when project files remain', async () => {
    const bundledNames = guideCommand._getBundledGuidelineNames();
    const [firstBundled] = [...bundledNames];
    writeGuidelinesDir({
      [firstBundled]: '# Standard file',
      'objects.local.md': '# Keep me'
    });

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.existsSync(path.join(testDir, 'guidelines'))).toBe(true);
    expect(fs.existsSync(path.join(testDir, 'guidelines', 'objects.local.md'))).toBe(true);
  });

  test('replaces full CLAUDE.md with slim stub', async () => {
    writeClaudeMd('# Claude Code Instructions\nThis is the old full guide. Very long content.');

    await guideCommand.execute(['--migrate', '--yes']);

    const newContent = fs.readFileSync(path.join(testDir, 'CLAUDE.md'), 'utf8');
    expect(newContent).toContain('abapgit-agent guide');
    expect(newContent).not.toContain('This is the old full guide');
  });

  test('leaves CLAUDE.md untouched when it already has slim stub', async () => {
    const slimContent = '# ABAP Development\nabapgit-agent guide\nCustom addition.';
    writeClaudeMd(slimContent);

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.readFileSync(path.join(testDir, 'CLAUDE.md'), 'utf8')).toBe(slimContent);
    const output = consoleLogOutput.join('\n');
    expect(output).toContain('Already clean');
  });

  test('leaves CLAUDE.md untouched when it has custom content', async () => {
    const customContent = '# My Project\nCustom instructions with no full-guide marker.';
    writeClaudeMd(customContent);

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.readFileSync(path.join(testDir, 'CLAUDE.md'), 'utf8')).toBe(customContent);
  });

  test('reports nothing to do when already clean', async () => {
    // Only objects.local.md, slim CLAUDE.md
    writeGuidelinesDir({ 'objects.local.md': '# Local only' });
    writeClaudeMd('# ABAP Development\nabapgit-agent guide');

    await guideCommand.execute(['--migrate', '--yes']);

    const output = consoleLogOutput.join('\n');
    expect(output).toContain('Already clean');
  });

  test('does not remove non-.md files from guidelines/', async () => {
    const bundledNames = guideCommand._getBundledGuidelineNames();
    const [firstBundled] = [...bundledNames];
    const dir = path.join(testDir, 'guidelines');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, firstBundled), '# Standard');
    fs.writeFileSync(path.join(dir, 'keep.txt'), 'not a md file');

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.existsSync(path.join(dir, 'keep.txt'))).toBe(true);
  });

  test('replaces full copilot-instructions.md with slim stub', async () => {
    const githubDir = path.join(testDir, '.github');
    fs.mkdirSync(githubDir, { recursive: true });
    const fullContent = '# ABAP Development with abapGit\n\nThis is the old full Copilot guide. Very long content.';
    fs.writeFileSync(path.join(githubDir, 'copilot-instructions.md'), fullContent);

    await guideCommand.execute(['--migrate', '--yes']);

    const newContent = fs.readFileSync(path.join(githubDir, 'copilot-instructions.md'), 'utf8');
    expect(newContent).toContain('abapgit-agent guide');
    expect(newContent).not.toContain('This is the old full Copilot guide');
  });

  test('leaves copilot-instructions.md untouched when already slim', async () => {
    const githubDir = path.join(testDir, '.github');
    fs.mkdirSync(githubDir, { recursive: true });
    const slimContent = '# ABAP Development\nRun abapgit-agent guide for the full guide.';
    fs.writeFileSync(path.join(githubDir, 'copilot-instructions.md'), slimContent);

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.readFileSync(path.join(githubDir, 'copilot-instructions.md'), 'utf8')).toBe(slimContent);
    const output = consoleLogOutput.join('\n');
    expect(output).toContain('Already clean');
  });

  test('leaves copilot-instructions.md untouched when it has custom content', async () => {
    const githubDir = path.join(testDir, '.github');
    fs.mkdirSync(githubDir, { recursive: true });
    const customContent = '# My Custom Copilot Instructions\nProject-specific content only.';
    fs.writeFileSync(path.join(githubDir, 'copilot-instructions.md'), customContent);

    await guideCommand.execute(['--migrate', '--yes']);

    expect(fs.readFileSync(path.join(githubDir, 'copilot-instructions.md'), 'utf8')).toBe(customContent);
  });

  test('migrates both CLAUDE.md and copilot-instructions.md in one pass', async () => {
    writeClaudeMd('# Claude Code Instructions\nFull ABAP guide content.');
    const githubDir = path.join(testDir, '.github');
    fs.mkdirSync(githubDir, { recursive: true });
    fs.writeFileSync(path.join(githubDir, 'copilot-instructions.md'),
      '# ABAP Development with abapGit\nFull Copilot guide content.');

    await guideCommand.execute(['--migrate', '--yes']);

    const claudeContent = fs.readFileSync(path.join(testDir, 'CLAUDE.md'), 'utf8');
    const copilotContent = fs.readFileSync(path.join(githubDir, 'copilot-instructions.md'), 'utf8');

    expect(claudeContent).toContain('abapgit-agent guide');
    expect(claudeContent).not.toContain('Full ABAP guide content');
    expect(copilotContent).toContain('abapgit-agent guide');
    expect(copilotContent).not.toContain('Full Copilot guide content');

    const output = consoleLogOutput.join('\n');
    expect(output).toContain('✅ Replaced CLAUDE.md with slim stub');
    expect(output).toContain('✅ Replaced .github/copilot-instructions.md with slim stub');
  });
});
