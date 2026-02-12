/**
 * Integration tests for init command
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

describe('Init Command Integration', () => {
  let testDir;

  beforeEach(() => {
    // Create a temporary test directory
    testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-agent-test-'));
  });

  afterEach(() => {
    // Cleanup
    if (testDir && fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  test('init creates .abapGitAgent with package and folder', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command
    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /myabap --package ZTEST_PACKAGE`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify .abapGitAgent was created
    const configPath = path.join(testDir, '.abapGitAgent');
    expect(fs.existsSync(configPath)).toBe(true);

    // Verify contents
    const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    expect(config.package).toBe('ZTEST_PACKAGE');
    expect(config.folder).toBe('/myabap');
  });

  test('init uses default /src folder', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command without --folder
    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --package ZTEST_PACKAGE`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify .abapGitAgent was created
    const configPath = path.join(testDir, '.abapGitAgent');
    const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    expect(config.folder).toBe('/src');
  });

  test('init creates folder and .gitkeep', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command
    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify folder was created
    const folderPath = path.join(testDir, 'src');
    expect(fs.existsSync(folderPath)).toBe(true);

    // Verify .gitkeep was created
    const gitkeepPath = path.join(folderPath, '.gitkeep');
    expect(fs.existsSync(gitkeepPath)).toBe(true);
  });

  test('init fails without --package', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command without --package - should fail
    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    expect(() => {
      execSync(`node "${binPath}" init --folder /src`, {
        cwd: testDir,
        stdio: 'pipe'
      });
    }).toThrow();
  });

  test('init fails without git repo', () => {
    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    expect(() => {
      execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
        cwd: testDir,
        stdio: 'pipe'
      });
    }).toThrow();
  });

  test('init fails without git remote', () => {
    // Setup git repo without remote
    execSync('git init', { cwd: testDir, stdio: 'pipe' });

    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    expect(() => {
      execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
        cwd: testDir,
        stdio: 'pipe'
      });
    }).toThrow();
  });

  test('init fails if .abapGitAgent already exists', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Create .abapGitAgent first
    const configPath = path.join(testDir, '.abapGitAgent');
    fs.writeFileSync(configPath, '{"host": "test"}');

    const binPath = path.join(__dirname, '..', 'bin', 'abapgit-agent');
    expect(() => {
      execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
        cwd: testDir,
        stdio: 'pipe'
      });
    }).toThrow();
  });
});
