/**
 * Release script - Creates release for github.com
 *
 * Usage: npm run release
 *
 * This script:
 * 1. Reads version from package.json
 * 2. Updates the ABAP health resource with the new version
 * 3. Uses Claude CLI to generate release notes from commits
 * 4. Updates RELEASE_NOTES.md with new version notes
 * 5. Pushes to github.com to trigger GitHub Actions
 * 6. GitHub Actions will publish to npm and create GitHub release
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const packageJsonPath = path.join(__dirname, '..', 'package.json');
const abapHealthPath = path.join(__dirname, '..', 'abap', 'zcl_abgagt_resource_health.clas.abap');
const releaseNotesPath = path.join(__dirname, '..', 'RELEASE_NOTES.md');
const repoRoot = path.join(__dirname, '..');

// Read version from package.json
const pkg = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
const version = pkg.version;

console.log(`Current version: ${version}`);
console.log('');

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

// Get commits since last tag for release notes
console.log('Generating release notes with Claude...');
console.log('');

let releaseNotesContent = '';

try {
  // Find previous tag
  const allTags = execSync('git tag --sort=-v:refname', { cwd: repoRoot, encoding: 'utf8' });
  const tagList = allTags.trim().split('\n').filter(t => t.startsWith('v'));
  const previousTag = tagList[1] || 'HEAD~10';

  // Get commits since last release
  const commits = execSync(`git log ${previousTag}..HEAD --oneline`, { cwd: repoRoot, encoding: 'utf8' });

  if (commits.trim()) {
    console.log(`Found ${commits.trim().split('\n').length} commits since last release`);
    console.log('');

    // Create Claude prompt - escape for shell
    const commitsEscaped = commits.replace(/"/g, '\\"').replace(/\n/g, '\\n');
    const prompt = `Generate release notes for version ${version} of this project based on these commits:\\n\\n${commitsEscaped}\\n\\nFormat as:\\n\\n## v${version}\\n\\n### New Features\\n- (list new features)\\n\\n### Bug Fixes\\n- (list bug fixes)\\n\\n### Improvements\\n- (list improvements)\\n\\n### Documentation\\n- (list documentation changes)\\n\\nIf there are no commits in a category, omit that category. Keep it concise.`;

    // Call Claude CLI
    try {
      releaseNotesContent = execSync(`claude --print "${prompt}"`, { cwd: repoRoot, encoding: 'utf8', timeout: 60000 });
      releaseNotesContent = releaseNotesContent.trim();
      console.log('Generated release notes:');
      console.log(releaseNotesContent);
      console.log('');
    } catch (e) {
      console.log('Claude CLI not available, using fallback');
      releaseNotesContent = `## v${version}\n\nSee commit history for changes.`;
    }
  } else {
    console.log('No commits since last release');
    releaseNotesContent = `## v${version}\n\nSee commit history for changes.`;
  }
} catch (e) {
  console.log('Could not generate release notes:', e.message);
  releaseNotesContent = `## v${version}\n\nSee commit history for changes.`;
}

// Update RELEASE_NOTES.md
if (fs.existsSync(releaseNotesPath)) {
  const existingContent = fs.readFileSync(releaseNotesPath, 'utf8');

  // Check if version already exists
  if (existingContent.includes(`## v${version}`)) {
    console.log(`Release notes for v${version} already exist`);
  } else {
    // Add new version at the top, before existing content
    const newContent = releaseNotesContent + '\n\n---\n\n' + existingContent;
    fs.writeFileSync(releaseNotesPath, newContent);
    console.log(`Updated RELEASE_NOTES.md with v${version}`);
  }
} else {
  // Create new RELEASE_NOTES.md
  fs.writeFileSync(releaseNotesPath, releaseNotesContent);
  console.log(`Created RELEASE_NOTES.md with v${version}`);
}
console.log('');

// Check git status
const status = execSync('git status --porcelain', { cwd: repoRoot, encoding: 'utf8' });

if (status.trim()) {
  // Stage and commit
  try {
    execSync('git add abap/zcl_abgagt_resource_health.clas.abap package.json RELEASE_NOTES.md', { cwd: repoRoot });
    execSync(`git commit -m "chore: release v${version}"`, { cwd: repoRoot });
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
console.log('3. Create GitHub release with Claude-generated notes');
console.log('');
console.log('Next step for ABAP system:');
console.log('  abapgit-agent pull --files abap/zcl_abgagt_resource_health.clas.abap');
