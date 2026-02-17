/**
 * Release script - Creates release for github.com
 *
 * Usage: npm run release
 *
 * This script:
 * 1. Reads version from package.json
 * 2. Updates the ABAP health resource with the new version
 * 3. Creates a git commit with the version update
 * 4. Pushes to github.com to trigger GitHub Actions
 * 5. GitHub Actions will publish to npm and create GitHub release
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const packageJsonPath = path.join(__dirname, '..', 'package.json');
const abapHealthPath = path.join(__dirname, '..', 'abap', 'zcl_abgagt_resource_health.clas.abap');
const repoRoot = path.join(__dirname, '..');

// Read version from package.json
const pkg = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
const version = pkg.version;

// Check if there's a git tag for this version
const tags = execSync('git tag --list', { cwd: repoRoot, encoding: 'utf8' });
const versionTag = `v${version}`;
if (!tags.includes(versionTag)) {
  console.log(`No git tag found for version ${version}`);
  console.log('');
  console.log('Please run one of the following first:');
  console.log('  npm version patch    # e.g., 1.4.0 -> 1.4.1');
  console.log('  npm version minor    # e.g., 1.4.0 -> 1.5.0');
  console.log('  npm version major    # e.g., 1.4.0 -> 2.0.0');
  console.log('');
  process.exit(1);
}

console.log(`Current version: ${version} (tag: ${versionTag})`);
console.log('');

// Check if there's a remote for github.com
let remoteName = 'origin';
try {
  const remotes = execSync('git remote -v', { cwd: repoRoot, encoding: 'utf8' });
  if (remotes.includes('github.com') && !remotes.includes('github.tools.sap')) {
    remoteName = 'origin';
  } else if (remotes.includes('public') && remotes.includes('github.com')) {
    remoteName = 'public';
  }
  console.log(`Using remote: ${remoteName}`);
} catch (e) {
  console.log('Could not determine remote, using origin');
}

console.log('');

// Read ABAP health resource file
let abapContent = fs.readFileSync(abapHealthPath, 'utf8');

// Update version in ABAP file (replace existing version)
const oldVersionMatch = abapContent.match(/version":"(\d+\.\d+\.\d+)"/);
if (oldVersionMatch) {
  const oldVersion = oldVersionMatch[1];
  abapContent = abapContent.replace(
    `version":"${oldVersion}"`,
    `version":"${version}"`
  );

  // Write updated content
  fs.writeFileSync(abapHealthPath, abapContent);
  console.log(`Updated ABAP version: ${oldVersion} -> ${version}`);
  console.log('');
} else {
  console.error('Could not find version in ABAP file');
  process.exit(1);
}

// Check git status
const status = execSync('git status --porcelain', { cwd: repoRoot, encoding: 'utf8' });

if (status.trim()) {
  // Stage and commit
  try {
    execSync('git add abap/zcl_abgagt_resource_health.clas.abap package.json', { cwd: repoRoot });
    execSync(`git commit -m "chore: update ABAP version to ${version}"`, { cwd: repoRoot });
    console.log('Created git commit for version update');
    console.log('');
  } catch (e) {
    console.log('No changes to commit or commit failed');
  }
} else {
  console.log('No changes to commit (version already up to date)');
  console.log('');
}

// Push to trigger GitHub Actions
console.log('Pushing to github.com to trigger release...');
console.log('');

try {
  // Push master and tags to github.com
  execSync(`git push ${remoteName} master --follow-tags`, { cwd: repoRoot });
  console.log('Pushed to github.com successfully!');
  console.log('');
} catch (e) {
  console.log('Push failed, please push manually');
  console.log('');
}

console.log('Release workflow triggered!');
console.log('');
console.log('The GitHub Actions workflow will:');
console.log('1. Run tests');
console.log('2. Publish to npm');
console.log('3. Create GitHub release with release notes');
console.log('');
console.log('Next step for ABAP system:');
console.log('  abapgit-agent pull --files abap/zcl_abgagt_resource_health.clas.abap');
