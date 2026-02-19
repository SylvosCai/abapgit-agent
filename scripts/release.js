/**
 * Release script - Creates release for github.com
 *
 * Usage: npm run release -- --patch | --minor | --major
 *
 * Options:
 *   --patch    Bump patch version (e.g., 1.4.0 -> 1.4.1)
 *   --minor    Bump minor version (e.g., 1.4.0 -> 1.5.0)
 *   --major    Bump major version (e.g., 1.4.0 -> 2.0.0)
 *   --dry-run  Test the release flow without pushing to github.com
 *
 * This script:
 * 1. Determines new version based on --patch/--minor/--major flag
 * 2. Generates release notes using Claude
 * 3. Updates RELEASE_NOTES.md with new version notes
 * 4. Updates the ABAP health resource with the new version
 * 5. Creates a git commit with all changes
 * 6. Runs npm version command to create the tag (LAST step)
 * 7. Pushes to github.com to trigger GitHub Actions (unless --dry-run)
 * 8. GitHub Actions will publish to npm and create GitHub release
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const packageJsonPath = path.join(__dirname, '..', 'package.json');
const abapHealthPath = path.join(__dirname, '..', 'abap', 'zcl_abgagt_resource_health.clas.abap');
const releaseNotesPath = path.join(__dirname, '..', 'RELEASE_NOTES.md');
const repoRoot = path.join(__dirname, '..');

// Parse arguments
const args = process.argv.slice(2);
const dryRun = args.includes('--dry-run');

// Get bump type (--patch, --minor, or --major)
let bumpType = null;
if (args.includes('--patch')) bumpType = 'patch';
else if (args.includes('--minor')) bumpType = 'minor';
else if (args.includes('--major')) bumpType = 'major';

if (!bumpType) {
  console.error('Error: Please specify --patch, --minor, or --major');
  console.error('');
  console.error('Usage:');
  console.error('  npm run release -- --patch    # e.g., 1.4.0 -> 1.4.1');
  console.error('  npm run release -- --minor    # e.g., 1.4.0 -> 1.5.0');
  console.error('  npm run release -- --major    # e.g., 1.4.0 -> 2.0.0');
  process.exit(1);
}

if (dryRun) {
  console.log('🔹 DRY RUN MODE - No actual release will be created\n');
}

// Read version from package.json
const pkg = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
const currentVersion = pkg.version;

// Calculate new version based on bump type
const [major, minor, patch] = currentVersion.split('.').map(Number);
let newVersion;
switch (bumpType) {
  case 'patch':
    newVersion = `${major}.${minor}.${patch + 1}`;
    break;
  case 'minor':
    newVersion = `${major}.${minor + 1}.0`;
    break;
  case 'major':
    newVersion = `${major + 1}.0.0`;
    break;
}

console.log(`Current version: ${currentVersion}`);
console.log(`New version: ${newVersion} (${bumpType} bump)`);
console.log('');

const versionTag = `v${newVersion}`;

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
    `version":"${newVersion}"`
  );

  // Write updated content
  fs.writeFileSync(abapHealthPath, abapContent);
  console.log(`📦 ABAP version: ${oldVersion} -> ${newVersion}`);
  if (dryRun) {
    console.log('   (file modified, not committed)');
  }
  console.log('');
} else {
  console.error('Could not find version in ABAP file');
  process.exit(1);
}

// Get commits since last tag for release notes
console.log('Generating release notes with Claude...');
console.log('');

let releaseNotesContent = '';

try {
  // Find previous tag
  const allTags = execSync('git tag --sort=-v:refname', { cwd: repoRoot, encoding: 'utf8' });
  const tagList = allTags.trim().split('\n').filter(t => t.startsWith('v'));
  const previousTag = tagList[0] || 'HEAD~10';

  // Get commits since last release
  const commits = execSync(`git log ${previousTag}..HEAD --oneline`, { cwd: repoRoot, encoding: 'utf8' });

  if (commits.trim()) {
    console.log(`Found ${commits.trim().split('\n').length} commits since last release`);
    console.log('');

    // Create Claude prompt - escape for shell
    const commitsEscaped = commits.replace(/"/g, '\\"').replace(/\n/g, '\\n');
    const prompt = `Generate concise release notes for version ${newVersion} of a Node.js CLI tool called abapgit-agent.

Commits since last release:
${commitsEscaped}

Instructions:
1. IGNORE commits that revert, undo, or remove previous changes
2. IGNORE commits that fix/improve the release process itself
3. Focus on actual USER-FACING features and fixes
4. Use 2-4 bullet points MAX per category
5. Keep each bullet brief (under 10 words)
6. START your response with "## v${newVersion}" and END with "---"
7. OUTPUT ONLY the release notes - do NOT add any intro text, explanation, or commentary
8. Use this exact format with blank lines between categories:
## v${newVersion}

### New Features

- Brief feature description

### Bug Fixes

- Brief fix description

### Improvements

- Brief improvement

### Documentation

- Brief doc update

---

OMIT any category that has no items.`;

    // Call Claude CLI - use stdin to avoid shell escaping issues
    try {
      releaseNotesContent = execSync('claude --print', {
        cwd: repoRoot,
        encoding: 'utf8',
        timeout: 120000,
        input: prompt
      });
      releaseNotesContent = releaseNotesContent.trim();
      console.log('Generated release notes:');
      console.log(releaseNotesContent);
      console.log('');
    } catch (e) {
      console.log('Claude CLI error:', e.message);
      console.log('Using fallback release notes');
      releaseNotesContent = `## v${newVersion}\n\nSee commit history for changes.`;
    }
  } else {
    console.log('No commits since last release');
    releaseNotesContent = `## v${newVersion}\n\nSee commit history for changes.`;
  }
} catch (e) {
  console.log('Could not generate release notes:', e.message);
  releaseNotesContent = `## v${newVersion}\n\nSee commit history for changes.`;
}

// Show release notes in dry-run mode
if (dryRun) {
  console.log('📝 Generated Release Notes:');
  console.log('─'.repeat(50));
  console.log(releaseNotesContent);
  console.log('─'.repeat(50));
  console.log('');
}

// Update RELEASE_NOTES.md - read from git to avoid local modifications affecting check
let existingContent = '';
try {
  existingContent = execSync(`git show HEAD:RELEASE_NOTES.md`, { cwd: repoRoot, encoding: 'utf8' });
} catch (e) {
  // File doesn't exist in git yet, use empty content
  existingContent = '';
}

if (existingContent) {
  // Check if version already exists in the committed file (not local modifications)
  // Match both "## v1.0.0" and "## Release Notes for v1.0.0" formats
  const versionRegex = new RegExp(`## (Release Notes for )?v${newVersion.replace(/\./g, '\\.')}`);
  if (versionRegex.test(existingContent)) {
    console.log(`Release notes for v${newVersion} already exist in git`);
  } else {
    let newContent;

    // Check if there's a "# Release Notes" header - insert after it if present
    if (existingContent.startsWith('# Release Notes')) {
      // Find the position after "# Release Notes" and any following content
      // Match both "## v1.0.0" and "## Release Notes for v1.0.0" formats
      const lines = existingContent.split('\n');
      let insertIndex = 0;
      for (let i = 0; i < lines.length; i++) {
        if (lines[i].match(/^## (Release Notes for )?v\d+\.\d+\.\d+/)) {
          insertIndex = i;
          break;
        }
      }
      // Insert new content before the first version header
      const before = lines.slice(0, insertIndex).join('\n');
      const after = lines.slice(insertIndex).join('\n');
      newContent = before + '\n\n' + releaseNotesContent + '\n\n---\n\n' + after;
    } else {
      // Add new version at the top
      newContent = releaseNotesContent + '\n\n---\n\n' + existingContent;
    }

    fs.writeFileSync(releaseNotesPath, newContent);
    if (dryRun) {
      console.log('📄 RELEASE_NOTES.md: would add new version at top');
    } else {
      console.log(`Updated RELEASE_NOTES.md with v${newVersion}`);
    }
  }
} else {
  // Create new RELEASE_NOTES.md
  fs.writeFileSync(releaseNotesPath, releaseNotesContent);
  console.log(`Created RELEASE_NOTES.md with v${newVersion}`);
}
console.log('');

// Check git status and commit (skip in dry-run)
const status = execSync('git status --porcelain', { cwd: repoRoot, encoding: 'utf8' });

if (status.trim()) {
  if (dryRun) {
    console.log('🔹 DRY RUN - Would create commit with changes:');
    console.log(status);
    console.log('');
  } else {
    // Stage and commit (NOTE: don't include package.json - npm version will handle it)
    try {
      execSync('git add abap/zcl_abgagt_resource_health.clas.abap RELEASE_NOTES.md', { cwd: repoRoot });
      execSync(`git commit -m "chore: release v${newVersion}"`, { cwd: repoRoot });
      console.log('Created git commit for version update');
      console.log('');
    } catch (e) {
      console.log('No changes to commit or commit failed');
    }
  }
} else {
  console.log('No changes to commit');
  console.log('');
}

// Run npm version command to create tag (must be done AFTER commit)
if (dryRun) {
  console.log(`🔹 DRY RUN - Would run: npm version ${bumpType}`);
  console.log('');
} else {
  console.log(`Running npm version ${bumpType} to create tag...`);
  try {
    execSync(`npm version ${bumpType} --no-git-tag-version`, { cwd: repoRoot, encoding: 'utf8' });
    console.log(`✅ Created version ${newVersion} in package.json`);
    console.log('');
  } catch (e) {
    console.log('⚠️ npm version failed:', e.message);
    console.log('');
  }
}

// Push to trigger GitHub Actions
if (dryRun) {
  console.log('🔹 DRY RUN - Skipping push to github.com');
  console.log('');
  console.log('To actually release, run:');
  console.log(`  git push ${remoteName} master --follow-tags`);
  console.log('');
} else {
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
  console.log('3. Create GitHub release with Claude-generated notes');
  console.log('');
  console.log('Next step for ABAP system:');
  console.log('  abapgit-agent pull --files abap/zcl_abgagt_resource_health.clas.abap');
}
