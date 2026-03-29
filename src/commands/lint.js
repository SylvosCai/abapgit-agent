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

    // Scope to changed files + their direct dependencies (interfaces, superclasses)
    // so abaplint can resolve cross-references without including the whole repo.
    const abapDir = cfg.global.files.replace(/\/\*\*.*$/, '').replace(/^\//, '') || 'abap';
    const depFiles = resolveDependencies(abapFiles, abapDir);
    const allFiles = [...new Set([...abapFiles, ...depFiles])];
    cfg.global.files = allFiles.map(f => `/${f}`);

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
 * Resolve direct dependencies of the given ABAP files by scanning their source
 * for interface/superclass/type references and mapping them to local files.
 *
 * Handles:
 *   INTERFACES <name>.              → <name>.intf.abap + <name>.intf.xml
 *   INHERITING FROM <name>          → <name>.clas.abap + <name>.clas.xml
 *   TYPE REF TO <name>              → <name>.intf.abap or <name>.clas.abap (whichever exists)
 *
 * Only resolves one level deep — enough for abaplint to check the changed files.
 * XML companion files are always included alongside their .abap counterpart
 * so xml_consistency checks can run.
 */
function resolveDependencies(abapFiles, abapDir) {
  const deps    = new Set();
  const visited = new Set(abapFiles); // don't re-scan changed files as deps

  // Patterns to extract referenced object names from ABAP source
  const patterns = [
    /^\s*INTERFACES\s+(\w+)\s*\./gim,
    /INHERITING\s+FROM\s+(\w+)/gim,
    /TYPE\s+REF\s+TO\s+(\w+)/gim,
  ];

  // BFS queue — seed with the changed files, then expand transitively
  const queue = [...abapFiles];

  while (queue.length > 0) {
    const file = queue.shift();

    let source;
    try { source = fs.readFileSync(file, 'utf8'); } catch { continue; }

    for (const pattern of patterns) {
      pattern.lastIndex = 0;
      let match;
      while ((match = pattern.exec(source)) !== null) {
        const name = match[1].toLowerCase();
        for (const suffix of [`${name}.intf`, `${name}.clas`]) {
          const abapFile = path.join(abapDir, `${suffix}.abap`);
          const xmlFile  = path.join(abapDir, `${suffix}.xml`);
          if (fs.existsSync(abapFile)) {
            deps.add(abapFile);
            if (fs.existsSync(xmlFile)) deps.add(xmlFile);
            // Recurse into this dep if not yet visited
            if (!visited.has(abapFile)) {
              visited.add(abapFile);
              queue.push(abapFile);
            }
            // For interfaces, also include the canonical concrete implementation
            // (zif_foo → zcl_foo) so rules like unused_variables can fully type-check.
            if (suffix.endsWith('.intf')) {
              const implName   = name.replace(/^zif_/, 'zcl_');
              const implFile   = path.join(abapDir, `${implName}.clas.abap`);
              const implXml    = path.join(abapDir, `${implName}.clas.xml`);
              if (fs.existsSync(implFile)) {
                deps.add(implFile);
                if (fs.existsSync(implXml)) deps.add(implXml);
                if (!visited.has(implFile)) {
                  visited.add(implFile);
                  queue.push(implFile);
                }
              }
            }
            break; // intf matched — don't also try clas
          }
        }
      }
    }

    // Always include the XML companion of each scanned file
    const xmlCompanion = file.replace(/\.abap$/, '.xml');
    if (fs.existsSync(xmlCompanion)) deps.add(xmlCompanion);
  }

  return [...deps];
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
