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

      // Match exactly the version header (e.g., ## v1.4.1)
      if (line.trim() === versionHeader) {
        inVersionSection = true;
        foundVersion = true;
        continue;
      }

      if (inVersionSection) {
        // Stop at next --- separator or exact version header
        if (line.startsWith('---')) {
          inVersionSection = false;
          newLines.push(line);
        } else if (line.trim().startsWith('## v')) {
          // Check if it's another exact version header (e.g., ## v1.4.0)
          const trimmed = line.trim();
          if (trimmed.match(/^## v\d+\.\d+\.\d+$/)) {
            inVersionSection = false;
            newLines.push(line);
          }
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

// Step 4: Restore version in package.json and abap health resource
console.log('Restoring version in local files...');

// Restore package.json to previous version (find previous tag)
const allTags = execSync('git tag --sort=-v:refname', { cwd: repoRoot, encoding: 'utf8' });
const tagList = allTags.trim().split('\n').filter(t => t.startsWith('v') && t !== versionTag);
const previousTag = tagList[0];

if (previousTag) {
  try {
    execSync(`git show ${previousTag}:package.json > ${packageJsonPath}`, { cwd: repoRoot, encoding: 'utf8' });
    console.log('✅ package.json restored to previous version');
  } catch (e) {
    console.log('⚠️  Could not restore package.json');
  }

  try {
    execSync(`git show ${previousTag}:abap/zcl_abgagt_resource_health.clas.abap > abap/zcl_abgagt_resource_health.clas.abap`, { cwd: repoRoot, encoding: 'utf8' });
    console.log('✅ ABAP health resource restored to previous version');
  } catch (e) {
    console.log('⚠️  Could not restore ABAP health resource');
  }
} else {
  console.log('⚠️  No previous tag found, cannot restore version');
}
console.log('');

// Step 6: Remove version bump and release commits if they are the top 2 commits
console.log('Checking for release commits to remove...');
try {
  // Get top 2 commit messages
  const topCommits = execSync('git log -2 --format="%s"', { cwd: repoRoot, encoding: 'utf8' }).trim().split('\n');

  if (topCommits.length >= 2) {
    const firstCommit = topCommits[0];  // Most recent (HEAD)
    const secondCommit = topCommits[1]; // Second most recent

    // Check if they match the expected pattern
    const isVersionBump = firstCommit.match(/^v?\d+\.\d+\.\d+$/);
    const isReleaseCommit = secondCommit && secondCommit.match(/^chore: release v\d+\.\d+\.\d+$/);

    if (isVersionBump && isReleaseCommit) {
      // Check if these commits are already pushed to remote
      const remoteRef = execSync('git rev-parse origin/master 2>/dev/null', { cwd: repoRoot, encoding: 'utf8' }).trim();
      const secondCommitRef = execSync('git rev-parse HEAD~1', { cwd: repoRoot, encoding: 'utf8' }).trim();

      if (remoteRef === secondCommitRef) {
        // Both commits are already pushed - can't remove without force push
        console.log('⚠️  Release commits already pushed to remote. Skipping commit removal.');
      } else {
        // Remove both commits
        execSync('git reset --hard HEAD~2', { cwd: repoRoot });
        console.log('✅ Version bump and release commits removed (2 commits)');
      }
    } else {
      console.log('⚠️  Top commits do not match expected release pattern, skipping removal');
    }
  } else {
    console.log('⚠️  Not enough commits to check');
  }
} catch (e) {
  console.log('⚠️  Could not check/remove release commits:', e.message);
}
console.log('');

// Step 7: Remove .draft-release marker file if exists
const draftFilePath = path.join(repoRoot, '.draft-release');
if (fs.existsSync(draftFilePath)) {
  fs.unlinkSync(draftFilePath);
  console.log('✅ .draft-release marker file removed');
}
console.log('');

console.log('Done!');
