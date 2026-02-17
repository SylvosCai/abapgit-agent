/**
 * Unrelease script - Remove a release
 *
 * Usage: npm run unrelease [version]
 *
 * This script:
 * 1. Takes a version as argument (default: current version from package.json)
 * 2. Deletes the GitHub release
 * 3. Deletes the git tag
 * 4. Removes release notes from RELEASE_NOTES.md
 * 5. Unpublishes from npm
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const packageJsonPath = path.join(__dirname, '..', 'package.json');
const releaseNotesPath = path.join(__dirname, '..', 'RELEASE_NOTES.md');
const repoRoot = path.join(__dirname, '..');

// Get version from argument or package.json
let version = process.argv[2];

if (!version) {
  const pkg = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
  version = pkg.version;
}

// Ensure version has 'v' prefix for tag
const versionTag = version.startsWith('v') ? version : `v${version}`;
const versionNoV = version.startsWith('v') ? version.slice(1) : version;

console.log(`Unreleasing version: ${versionNoV} (tag: ${versionTag})`);
console.log('');

// Check if there's a remote for github.com
let remoteName = 'origin';
try {
  const remotes = execSync('git remote -v', { cwd: repoRoot, encoding: 'utf8' });
  if (remotes.includes('public') && remotes.includes('github.com')) {
    remoteName = 'public';
  }
  console.log(`Using remote: ${remoteName}`);
} catch (e) {
  console.log('Could not determine remote, using origin');
}

console.log('');

// Step 1: Delete GitHub release
console.log('Deleting GitHub release...');
try {
  execSync(`gh release delete ${versionTag} --repo SylvosCai/abapgit-agent --yes`, { cwd: repoRoot, encoding: 'utf8' });
  console.log('✅ GitHub release deleted');
} catch (e) {
  console.log('⚠️  GitHub release not found or already deleted');
}
console.log('');

// Step 2: Delete git tag locally and remotely
console.log('Deleting git tag...');
try {
  execSync(`git tag -d ${versionTag}`, { cwd: repoRoot, encoding: 'utf8' });
  console.log('✅ Local tag deleted');
} catch (e) {
  console.log('⚠️  Local tag not found');
}

try {
  execSync(`git push ${remoteName} --delete ${versionTag}`, { cwd: repoRoot, encoding: 'utf8' });
  console.log('✅ Remote tag deleted');
} catch (e) {
  console.log('⚠️  Remote tag not found or already deleted');
}
console.log('');

// Step 3: Remove release notes from RELEASE_NOTES.md
console.log('Removing release notes from RELEASE_NOTES.md...');
if (fs.existsSync(releaseNotesPath)) {
  let content = fs.readFileSync(releaseNotesPath, 'utf8');

  // Check if version exists in release notes
  const versionHeader = `## v${versionNoV}`;
  if (content.includes(versionHeader)) {
    // Remove the version section (from ## vX.X.X to next --- or end)
    const lines = content.split('\n');
    const newLines = [];
    let inVersionSection = false;
    let foundVersion = false;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];

      if (line.startsWith(versionHeader)) {
        inVersionSection = true;
        foundVersion = true;
        continue;
      }

      if (inVersionSection) {
        // Stop at next --- separator or next ## version header
        if (line.startsWith('---')) {
          inVersionSection = false;
          newLines.push(line);
        } else if (line.startsWith('## v')) {
          inVersionSection = false;
          newLines.push(line);
        }
        // Otherwise skip this line (part of the version section)
      } else {
        newLines.push(line);
      }
    }

    if (foundVersion) {
      fs.writeFileSync(releaseNotesPath, newLines.join('\n').trim() + '\n');
      console.log('✅ Release notes removed');
    } else {
      console.log('⚠️  Version not found in RELEASE_NOTES.md');
    }
  } else {
    console.log('⚠️  Version not found in RELEASE_NOTES.md');
  }
} else {
  console.log('⚠️  RELEASE_NOTES.md not found');
}
console.log('');

// Step 4: Unpublish from npm
console.log('Unpublishing from npm...');
try {
  execSync(`npm unpublish abapgit-agent@${versionNoV}`, { cwd: repoRoot, encoding: 'utf8' });
  console.log('✅ npm package unpublished');
} catch (e) {
  console.log('⚠️  npm unpublish failed (may need to wait or use --force)');
}
console.log('');

console.log('Done!');
