/**
 * Git-related utilities
 */
const pathModule = require('path');
const fs = require('fs');

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
  return match ? match[1] : 'main';
}

/**
 * Check if current directory is a git repository
 * @returns {boolean} True if .git directory exists
 */
function isGitRepo() {
  const gitPath = pathModule.join(process.cwd(), '.git');
  return fs.existsSync(gitPath);
}

module.exports = {
  getRemoteUrl,
  getBranch,
  isGitRepo
};
