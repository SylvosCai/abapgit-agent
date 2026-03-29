'use strict';

/**
 * lint command - Run abaplint on changed ABAP files
 *
 * Detects files changed relative to a base branch (or HEAD~1),
 * creates a scoped abaplint config for just those files, and runs the check.
 *
 * Usage:
 *   abapgit-agent lint
 *   abapgit-agent lint --config .abaplint.json
 *   abapgit-agent lint --base main
 *   abapgit-agent lint --files src/foo.clas.abap,src/foo.clas.testclasses.abap
 *   abapgit-agent lint --outformat checkstyle --outfile reports/abaplint-results.xml
 */

const fs = require('fs');
const path = require('path');
const { execSync, spawnSync } = require('child_process');

module.exports = {
  name: 'lint',
  description: 'Run abaplint on changed ABAP files',
  requiresAbapConfig: false,

  execute(args) {
    const configPath  = argValue(args, '--config')    || '.abaplint.json';
    const baseBranch  = argValue(args, '--base');
    const filesArg    = argValue(args, '--files');
    const outformat   = argValue(args, '--outformat');
    const outfile     = argValue(args, '--outfile');

    // ── Resolve changed files ─────────────────────────────────────────────────
    let abapFiles;
    if (filesArg) {
      abapFiles = filesArg.split(',').map(f => f.trim()).filter(f => f.endsWith('.abap'));
    } else {
      abapFiles = detectChangedAbapFiles(baseBranch);
    }

    if (abapFiles.length === 0) {
      console.log('No changed .abap files found — nothing to lint.');
      return;
    }

    if (!outfile) {
      console.log(`\nLinting ${abapFiles.length} file(s):`);
      abapFiles.forEach(f => console.log(`  ${f}`));
      console.log('');
    }

    // ── Load and scope the abaplint config ────────────────────────────────────
    if (!fs.existsSync(configPath)) {
      console.error(`Error: abaplint config not found: ${configPath}`);
      console.error('Run from the project root, or pass --config <path>.');
      process.exit(1);
    }

    const cfg = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    cfg.global.files = abapFiles.map(f => `/${f}`);

    const scopedConfig = '.abaplint-local.json';
    fs.writeFileSync(scopedConfig, JSON.stringify(cfg, null, 2));

    // ── Run abaplint ──────────────────────────────────────────────────────────
    try {
      const formatArgs = outformat ? `--outformat ${outformat}` : '';
      const fileArgs   = outfile   ? `--outfile ${outfile}`     : '';
      const result = spawnSync(
        `npx @abaplint/cli@latest ${scopedConfig} ${formatArgs} ${fileArgs}`,
        { stdio: 'inherit', shell: true }
      );
      if (result.status !== 0) {
        process.exitCode = result.status;
      }
    } finally {
      fs.unlinkSync(scopedConfig);
    }
  }
};

// ── Helpers ───────────────────────────────────────────────────────────────────

function argValue(args, flag) {
  const idx = args.indexOf(flag);
  return idx !== -1 && idx + 1 < args.length ? args[idx + 1] : null;
}

/**
 * Detect changed .abap files using git diff.
 * - If on a PR branch (CHANGE_TARGET set, e.g. in CI): diffs against that target.
 * - If --base is given: diffs against that branch.
 * - Otherwise: diffs HEAD~1..HEAD (last commit).
 */
function detectChangedAbapFiles(baseBranch) {
  const base = baseBranch
    || (process.env.CHANGE_TARGET ? `origin/${process.env.CHANGE_TARGET}` : null);

  let diffCmd;
  if (base) {
    diffCmd = `git diff --name-only ${base}...HEAD -- '*.abap'`;
  } else {
    // Fall back to uncommitted changes first, then last commit
    const uncommitted = runGit('git diff --name-only HEAD -- *.abap').filter(Boolean);
    if (uncommitted.length > 0) return filterAbapFiles(uncommitted);
    diffCmd = `git diff --name-only HEAD~1 HEAD -- '*.abap'`;
  }

  return filterAbapFiles(runGit(diffCmd));
}

function runGit(cmd) {
  try {
    return execSync(cmd, { encoding: 'utf8' }).trim().split('\n').filter(Boolean);
  } catch {
    return [];
  }
}

/**
 * Keep only files that look like ABAP source files
 * (name.type.abap or name.type.subtype.abap).
 */
function filterAbapFiles(files) {
  return files.filter(f => {
    const parts = path.basename(f).split('.');
    return (parts.length === 3 || parts.length === 4) &&
           parts[parts.length - 1].toLowerCase() === 'abap';
  });
}
