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
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
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
    expect(config.folder).toBe('/myabap/');
  });

  test('init uses default /src/ folder', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command without --folder
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --package ZTEST_PACKAGE`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify .abapGitAgent was created
    const configPath = path.join(testDir, '.abapGitAgent');
    const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    expect(config.folder).toBe('/src/');
  });

  test('init creates folder and .gitkeep', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
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
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    expect(() => {
      execSync(`node "${binPath}" init --folder /src`, {
        cwd: testDir,
        stdio: 'pipe'
      });
    }).toThrow();
  });

  test('init fails without git repo', () => {
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
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

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    expect(() => {
      execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
        cwd: testDir,
        stdio: 'pipe'
      });
    }).toThrow();
  });

  test('init merges with existing .abapGitAgent', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Create .abapGitAgent with existing credentials
    const configPath = path.join(testDir, '.abapGitAgent');
    fs.writeFileSync(configPath, JSON.stringify({
      host: "existing-host.com",
      user: "MYUSER",
      password: "mypassword",
      package: "ZOLD_PACKAGE",
      folder: "/old/"
    }, null, 2));

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZNEW_PACKAGE`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify config was merged (not replaced)
    const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    expect(config.host).toBe('existing-host.com'); // Preserved
    expect(config.user).toBe('MYUSER'); // Preserved
    expect(config.password).toBe('mypassword'); // Preserved
    expect(config.package).toBe('ZNEW_PACKAGE'); // Updated
    expect(config.folder).toBe('/src/'); // Updated
  });

  test('init creates .gitignore with sensitive files', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Run init command
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify .gitignore was created
    const gitignorePath = path.join(testDir, '.gitignore');
    expect(fs.existsSync(gitignorePath)).toBe(true);

    const content = fs.readFileSync(gitignorePath, 'utf8');
    expect(content).toContain('.abapGitAgent');
  });

  test('init updates existing .gitignore with sensitive files', () => {
    // Setup git repo
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Create existing .gitignore
    const gitignorePath = path.join(testDir, '.gitignore');
    fs.writeFileSync(gitignorePath, 'node_modules/\n');

    // Run init command
    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // Verify .gitignore was updated
    const content = fs.readFileSync(gitignorePath, 'utf8');
    expect(content).toContain('node_modules/');
    expect(content).toContain('.abapGitAgent');
  });

  test('init creates .abapgit.xml with correct STARTING_FOLDER and FOLDER_LOGIC', () => {
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST --folder-logic FULL`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    const xmlPath = path.join(testDir, '.abapgit.xml');
    expect(fs.existsSync(xmlPath)).toBe(true);

    const content = fs.readFileSync(xmlPath, 'utf8');
    expect(content).toContain('<STARTING_FOLDER>/src/</STARTING_FOLDER>');
    expect(content).toContain('<FOLDER_LOGIC>FULL</FOLDER_LOGIC>');
    expect(content).toContain('<MASTER_LANGUAGE>E</MASTER_LANGUAGE>');
  });

  test('init uses default FOLDER_LOGIC PREFIX when not specified', () => {
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /abap --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    const xmlPath = path.join(testDir, '.abapgit.xml');
    const content = fs.readFileSync(xmlPath, 'utf8');
    expect(content).toContain('<STARTING_FOLDER>/abap/</STARTING_FOLDER>');
    expect(content).toContain('<FOLDER_LOGIC>PREFIX</FOLDER_LOGIC>');
  });

  test('init skips .abapgit.xml creation when file already exists', () => {
    execSync('git init', { cwd: testDir, stdio: 'pipe' });
    execSync('git remote add origin https://github.com/test/repo.git', { cwd: testDir, stdio: 'pipe' });

    // Pre-create .abapgit.xml with custom content
    const xmlPath = path.join(testDir, '.abapgit.xml');
    const existingContent = '<?xml version="1.0"?><custom>existing</custom>';
    fs.writeFileSync(xmlPath, existingContent);

    const binPath = path.join(__dirname, '..', '..', 'bin', 'abapgit-agent');
    execSync(`node "${binPath}" init --folder /src --package ZTEST`, {
      cwd: testDir,
      stdio: 'pipe'
    });

    // File must not be overwritten
    expect(fs.readFileSync(xmlPath, 'utf8')).toBe(existingContent);
  });
});
