/**
 * Git-related utilities
 */
const pathModule = require('path');
const fs = require('fs');
const { execSync } = require('child_process');

/**
 * Get git remote URL from .git/config
 * @returns {string|null} Git remote URL or null if not found
 */
function getRemoteUrl() {
  const gitConfigPath = pathModule.join(process.cwd(), '.git', 'config');

  if (!fs.existsSync(gitConfigPath)) {
    return null;
  }

  const content = fs.readFileSync(gitConfigPath, 'utf8');
  const remoteMatch = content.match(/\[remote "origin"\]/);

  if (!remoteMatch) {
    return null;
  }

  const urlMatch = content.match(/	url = (.+)/);
  return urlMatch ? urlMatch[1].trim() : null;
}

/**
 * Get current git branch from .git/HEAD
 * @returns {string} Current branch name or 'main' as default
 */
function getBranch() {
  const headPath = pathModule.join(process.cwd(), '.git', 'HEAD');

  if (!fs.existsSync(headPath)) {
    return 'main';
  }

  const content = fs.readFileSync(headPath, 'utf8').trim();
  const match = content.match(/ref: refs\/heads\/(.+)/);
  if (match) return match[1];

  // Detached HEAD (e.g. Jenkins PR checkout) — use CHANGE_BRANCH env var
  // which Jenkins sets to the actual PR head branch name.
  if (process.env.CHANGE_BRANCH) return process.env.CHANGE_BRANCH;

  return 'main';
}

/**
 * Check if current directory is a git repository
 * @returns {boolean} True if .git directory exists
 */
function isGitRepo() {
  const gitPath = pathModule.join(process.cwd(), '.git');
  return fs.existsSync(gitPath);
}

/**
 * Get the default/trunk branch name (main, master, develop, etc.)
 * @param {string|null} configuredBranch - Branch name from config (overrides auto-detection)
 * @returns {string} Default branch name
 */
function getDefaultBranch(configuredBranch = null) {
  // If explicitly configured, use that
  if (configuredBranch) {
    return configuredBranch;
  }

  // Method 1: Check remote HEAD (most accurate)
  try {
    const result = execSync('git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null', {
      cwd: process.cwd(),
      encoding: 'utf8'
    }).trim();

    // Result looks like: refs/remotes/origin/main
    const match = result.match(/refs\/remotes\/origin\/(.+)/);
    if (match && match[1]) {
      return match[1];
    }
  } catch (e) {
    // Ignore error, try next method
  }

  // Method 2: Check common branch names in remote
  try {
    const branches = execSync('git branch -r 2>/dev/null', {
      cwd: process.cwd(),
      encoding: 'utf8'
    });

    // Check in order of preference: main > master > develop
    if (branches.includes('origin/main')) return 'main';
    if (branches.includes('origin/master')) return 'master';
    if (branches.includes('origin/develop')) return 'develop';
  } catch (e) {
    // Ignore error, use fallback
  }

  // Method 3: Fallback to 'main' (modern default)
  return 'main';
}

module.exports = {
  getRemoteUrl,
  getBranch,
  isGitRepo,
  getDefaultBranch
};
