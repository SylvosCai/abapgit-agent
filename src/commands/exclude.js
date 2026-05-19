/**
 * exclude command — Remove git-tracked files matching exclude.patterns from git,
 * and/or auto-exclude files whose object type is not supported by abapgit-agent.
 * ABAP objects in the system are never touched.
 */

'use strict';

const { execSync } = require('child_process');
const { getExcludePatterns, isExcluded, getUnsupportedTypesFlag, isUnsupportedType } = require('../config');

module.exports = {
  name: 'exclude',
  description: 'Remove git-tracked files matching exclude.patterns from git (ABAP objects untouched)',
  requiresAbapConfig: false,

  async execute(args) {
    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent exclude

Reads exclude config from .abapgit-agent.json, finds all matching git-tracked
files, removes them from git tracking (git rm --cached), and commits the removal.
ABAP objects in the system are never touched.

Configuration (.abapgit-agent.json):
  {
    "exclude": {
      "unsupportedTypes": true,
      "patterns": [
        "zcl_*_dpc*.clas.*",
        "zcl_*_mpc*.clas.*"
      ]
    }
  }

  unsupportedTypes: true  — auto-exclude any file whose object type is not
    supported by abapgit-agent (CLAS, INTF, PROG, FUGR, TABL, DTEL, TTYP,
    DOMA, DDLS, DCLS, MSAG, STRU, ENHO, SUSO). Useful after import to clean
    up types abapGit cannot activate.

  patterns  — glob patterns matched against the file basename (* = any chars).
    Use when you also want to exclude specific supported-type files (e.g. DPC).
`);
      return;
    }

    const patterns = getExcludePatterns();
    const excludeUnsupported = getUnsupportedTypesFlag();

    if (patterns.length === 0 && !excludeUnsupported) {
      console.log('  No exclude patterns configured in .abapgit-agent.json.');
      console.log('  Add an "exclude.patterns" array or set "exclude.unsupportedTypes": true.');
      return;
    }

    console.log(`\n  Checking exclude config from .abapgit-agent.json...`);
    if (patterns.length > 0) {
      console.log(`  Found ${patterns.length} pattern(s): ${patterns.join(', ')}`);
    }
    if (excludeUnsupported) {
      console.log(`  Auto-excluding unsupported object types: true`);
    }
    console.log('');

    // Get all git-tracked files
    let trackedFiles;
    try {
      trackedFiles = execSync('git ls-files', { encoding: 'utf8' })
        .split('\n')
        .map(f => f.trim())
        .filter(Boolean);
    } catch (e) {
      console.error('❌ Error: failed to list git-tracked files. Are you in a git repository?');
      process.exit(1);
    }

    const toRemove = trackedFiles.filter(f =>
      isExcluded(f, patterns) || (excludeUnsupported && isUnsupportedType(f))
    );

    if (toRemove.length === 0) {
      console.log('  No tracked files match the exclude config. Nothing to do.');
      return;
    }

    console.log(`  Files to remove from git:`);
    toRemove.forEach(f => console.log(`     ${f}`));
    console.log('');

    // git rm --cached
    try {
      const fileArgs = toRemove.map(f => `"${f}"`).join(' ');
      execSync(`git rm --cached ${fileArgs}`, { encoding: 'utf8' });
    } catch (e) {
      console.error(`❌ Error running git rm --cached: ${e.message}`);
      process.exit(1);
    }

    // commit
    try {
      execSync(
        'git commit -m "chore: exclude ABAP objects per .abapgit-agent.json"',
        { encoding: 'utf8' }
      );
    } catch (e) {
      console.error(`❌ Error creating git commit: ${e.message}`);
      process.exit(1);
    }

    console.log(`✅ Removed ${toRemove.length} file(s) from git. ABAP objects remain untouched.`);
    console.log(`   Committed as: chore: exclude ABAP objects per .abapgit-agent.json`);
  }
};
