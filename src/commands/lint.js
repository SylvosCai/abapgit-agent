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
    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent lint [--files <file1,...>] [--base <branch>] [--config <file>]
                     [--outformat <format>] [--outfile <file>]

Description:
  Run abaplint static analysis on changed ABAP files. Requires .abaplint.json in repo root.
  By default detects files changed vs HEAD~1 (or vs --base branch).

Parameters:
  --files <file1,...>   Check specific files instead of auto-detecting changed files.
  --base <branch>       Diff against this branch instead of HEAD~1 (e.g. main).
  --config <file>       abaplint config file (default: .abaplint.json).
  --outformat <format>  Output format: checkstyle (default: text).
  --outfile <file>      Write output to file instead of stdout.

Examples:
  abapgit-agent lint
  abapgit-agent lint --base main
  abapgit-agent lint --files src/zcl_foo.clas.abap
  abapgit-agent lint --outformat checkstyle --outfile reports/abaplint.xml
`);
      return;
    }

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
    const fileIndex = buildFileIndex(abapDir);
    const depFiles = resolveDependencies(abapFiles, fileIndex);
    const allFiles = [...new Set([...abapFiles, ...depFiles])];
    cfg.global.files = allFiles.map(f => `/${f}`);

    const scopedConfig = '.abaplint-local.json';
    fs.writeFileSync(scopedConfig, JSON.stringify(cfg, null, 2));

    // ── Run abaplint ──────────────────────────────────────────────────────────
    // Dep files are included in the scoped config so abaplint can resolve
    // cross-references (e.g. implement_methods needs the interface source).
    // When producing checkstyle output (CI mode), post-filter the XML to only
    // keep <file> blocks for the originally changed files — suppressing any
    // pre-existing issues in dependency files that were not part of this change.
    try {
      if (outformat === 'checkstyle') {
        // Run to a temp file, filter, then write to the final destination.
        const tempOut = '.abaplint-raw.xml';
        const abapFilesSet = new Set(abapFiles.map(f => path.resolve(f)));
        try {
          const result = spawnSync(
            `npx @abaplint/cli@latest ${scopedConfig} --outformat checkstyle --outfile ${tempOut}`,
            { stdio: 'pipe', shell: true }
          );
          const raw = fs.existsSync(tempOut) ? fs.readFileSync(tempOut, 'utf8') : '<checkstyle version="8.0"/>';
          const filtered = filterCheckstyleToFiles(raw, abapFilesSet);
          if (outfile) {
            fs.writeFileSync(outfile, filtered);
          } else {
            process.stdout.write(filtered);
          }
          const issueCount = (filtered.match(/<error /g) || []).length;
          if (issueCount > 0) process.exitCode = 1;
        } finally {
          if (fs.existsSync(tempOut)) fs.unlinkSync(tempOut);
        }
      } else {
        // Interactive: inherit stdio so abaplint's human-readable output flows through.
        const result = spawnSync(
          `npx @abaplint/cli@latest ${scopedConfig}`,
          { stdio: 'inherit', shell: true }
        );
        if (result.status !== 0) process.exitCode = result.status;
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
 * Build a map of basename → full path for all .abap and .xml files
 * found recursively under abapDir. Used for dependency resolution so
 * that projects with nested folder structures (e.g. src/module/pkg/foo.clas.abap)
 * are handled correctly — not just flat abap/ layouts.
 */
function buildFileIndex(abapDir) {
  const index = new Map(); // basename (lowercase) → full path
  function walk(dir) {
    let entries;
    try { entries = fs.readdirSync(dir, { withFileTypes: true }); } catch { return; }
    for (const entry of entries) {
      const full = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        walk(full);
      } else if (entry.name.endsWith('.abap') || entry.name.endsWith('.xml')) {
        index.set(entry.name.toLowerCase(), full);
      }
    }
  }
  walk(abapDir);
  return index;
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
 * Uses a filename index built from a recursive walk of abapDir so that
 * deeply nested project structures are handled correctly.
 * XML companion files are always included alongside their .abap counterpart
 * so xml_consistency checks can run.
 */
function resolveDependencies(abapFiles, fileIndex) {
  const deps    = new Set();
  const visited = new Set(abapFiles); // don't re-scan changed files as deps

  // Patterns to extract referenced object names from ABAP source
  const patterns = [
    /^\s*INTERFACES:?\s+(\w+)\s*\./gim,
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
          const abapFile = fileIndex.get(`${suffix}.abap`);
          const xmlFile  = fileIndex.get(`${suffix}.xml`);
          if (abapFile) {
            deps.add(abapFile);
            if (xmlFile) deps.add(xmlFile);
            // Recurse into this dep if not yet visited
            if (!visited.has(abapFile)) {
              visited.add(abapFile);
              queue.push(abapFile);
            }
            // For interfaces, also include the canonical concrete implementation
            // (zif_foo → zcl_foo) so rules like unused_variables can fully type-check.
            if (suffix.endsWith('.intf')) {
              const implName  = name.replace(/^zif_/, 'zcl_');
              const implFile  = fileIndex.get(`${implName}.clas.abap`);
              const implXml   = fileIndex.get(`${implName}.clas.xml`);
              if (implFile) {
                deps.add(implFile);
                if (implXml) deps.add(implXml);
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
    const xmlBasename = path.basename(file).replace(/\.abap$/, '.xml').toLowerCase();
    const xmlCompanion = fileIndex.get(xmlBasename);
    if (xmlCompanion) deps.add(xmlCompanion);
  }

  return [...deps];
}

/**
 * Filter a checkstyle XML string to only include <file> blocks whose name
 * attribute resolves to one of the files in the given Set of absolute paths.
 * The outer <checkstyle> wrapper is preserved; the version attribute is kept.
 */
function filterCheckstyleToFiles(xml, abapFilesSet) {
  // Extract the opening <checkstyle ...> tag (preserves version= attribute).
  const headerMatch = xml.match(/^[\s\S]*?(<checkstyle[^>]*>)/);
  const header = headerMatch ? headerMatch[1] : '<checkstyle version="8.0">';

  // Match each <file name="...">...</file> block (including self-closing).
  const fileBlockRe = /<file\s+name="([^"]*)"[\s\S]*?<\/file>|<file\s+name="([^"]*)"\s*\/>/g;
  const kept = [];
  let match;
  while ((match = fileBlockRe.exec(xml)) !== null) {
    const filePath = match[1] || match[2];
    if (abapFilesSet.has(path.resolve(filePath))) {
      kept.push(match[0]);
    }
  }

  return `<?xml version="1.0" encoding="UTF-8"?>\n${header}\n${kept.join('\n')}${kept.length ? '\n' : ''}</checkstyle>\n`;
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
