/**
 * Test Repository URL Resolution
 *
 * Resolves test repo URLs from .abapGitAgent config (testRepos section) so that
 * developers can point to their own forks or public mirrors instead of the
 * default github.tools.sap URLs.
 *
 * Configuration (optional — add to .abapGitAgent):
 *
 *   "testRepos": {
 *     "pull":      "https://github.com/your-org/abgagt-pull-test.git",
 *     "drop":      "https://github.com/your-org/abgagt-drop-test.git",
 *     "customize": "https://github.com/your-org/abgagt-customize-test.git",
 *     "lifecycle": "https://github.com/your-org/abgagt-lifecycle-test.git",
 *     "run":       "https://github.com/your-org/abgagt-run-test.git",
 *     "syntax":    "https://github.com/your-org/abgagt-syntax-test.git"
 *   }
 *
 * If a key is absent the default (github.tools.sap) URL is used, so existing
 * setups require no config change.
 */

'use strict';

const fs   = require('fs');
const path = require('path');

const DEFAULTS = {
  pull:      'https://github.com/SylvosCai/abgagt-pull-test.git',
  drop:      'https://github.com/SylvosCai/abgagt-drop-test.git',
  customize: 'https://github.com/SylvosCai/abgagt-customize-test.git',
  debug:     'https://github.com/SylvosCai/abgagt-debug-test.git',
  lifecycle: 'https://github.com/SylvosCai/abgagt-lifecycle-test.git',
  run:       'https://github.com/SylvosCai/abgagt-run-test.git',
  syntax:    'https://github.com/SylvosCai/abgagt-syntax-test.git',
  unit:      'https://github.com/SylvosCai/abgagt-unit-test.git',
  pullType:  'https://github.com/SylvosCai/abgagt-pull-type-test.git',
  exclude:   'https://github.com/SylvosCai/abgagt-exclude-test.git',
};

/**
 * Load the testRepos section from the nearest .abapGitAgent file, walking up
 * from the integration test directory to the project root.
 */
function loadTestRepos() {
  // Walk up from this file's directory until we find .abapGitAgent
  let dir = __dirname;
  for (let i = 0; i < 5; i++) {
    const candidate = path.join(dir, '.abapGitAgent');
    if (fs.existsSync(candidate)) {
      try {
        const cfg = JSON.parse(fs.readFileSync(candidate, 'utf8'));
        return cfg.testRepos || {};
      } catch {
        return {};
      }
    }
    const parent = path.dirname(dir);
    if (parent === dir) break;
    dir = parent;
  }
  return {};
}

const _overrides = loadTestRepos();

/**
 * Returns the URL for the given test repo key, preferring any override
 * configured in .abapGitAgent over the built-in default.
 *
 * @param {'pull'|'drop'|'customize'|'lifecycle'|'debug'|'run'|'syntax'|'unit'|'pullType'|'exclude'} key
 * @returns {string}
 */
function getTestRepoUrl(key) {
  return (_overrides[key]) || DEFAULTS[key];
}

module.exports = { getTestRepoUrl };
