---
layout: default
title: lint - Static Analysis
nav_order: 6
parent: Development Commands
---

# lint Command

## Overview

Run abaplint static analysis on changed ABAP files. Scopes the check to only the files that
changed (relative to a base branch or the last commit) so feedback is fast even in large repos.

Does **not** require an ABAP connection — analyzes local source files. `npx` may need network access to download abaplint.

### RAP Lint Boundaries

Native BDEF/DDLX changes use parser/static checks and XML companions; SRVD has no source parser. abaplint **2.120.103** was verified, not established as a universal minimum. Omitted parser rules are enabled for selected types while explicit `false` remains respected. Engine failures/missing reports fail rather than fabricate a clean result. This is not native SAP semantic validation. See the [support matrix](../commands.md#rap-source-support).

## Command

```bash
# Lint changed files (auto-detected from git)
abapgit-agent lint

# Lint changed files relative to a specific base branch
abapgit-agent lint --base main

# Lint specific files explicitly
abapgit-agent lint --files src/zcl_my_class.clas.abap,src/zcl_my_class.clas.testclasses.abap

# Use a custom abaplint config file
abapgit-agent lint --config path/to/.abaplint.json

# Output results in CheckStyle XML format (for CI reporting)
abapgit-agent lint --outformat checkstyle --outfile reports/abaplint-results.xml
```

## Prerequisites

- `.abaplint.json` must exist in the project root (or pass `--config`)
- `npx` must be available (Node.js installed)
- No ABAP connection required
- BDEF parsing requires an abaplint version with `bdef_parser_error` (verified with **2.120.103**). The command uses `@latest`; an incompatible cached version or missing report fails visibly.

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `--config` | No | `.abaplint.json` | Path to abaplint config file |
| `--base` | No | auto-detected | Base branch for git diff (e.g. `main`) |
| `--files` | No | auto-detected | Comma-separated list of `.abap`, `.asddls`, `.bdef.asbdef` or `.ddlx.asddlxs` sources |
| `--outformat` | No | — | Output format (e.g. `checkstyle`) |
| `--outfile` | No | — | Write output to this file instead of stdout |

---

## File Detection

When `--files` is not given, changed files are detected automatically:

| Context | Detection Method |
|---------|-----------------|
| CI with `CHANGE_TARGET` env var set (PR build) | `git diff origin/$CHANGE_TARGET...HEAD -- '*.abap' '*.asddls' '*.asbdef' '*.asddlxs'` |
| `--base <branch>` given | `git diff <branch>...HEAD -- '*.abap' '*.asddls' '*.asbdef' '*.asddlxs'` |
| Local with uncommitted changes | `git diff HEAD -- '*.abap' '*.asddls' '*.asbdef' '*.asddlxs'` |
| Local with no uncommitted changes | `git diff HEAD~1 HEAD -- '*.abap' '*.asddls' '*.asbdef' '*.asddlxs'` |

Detected files are filtered to those that exist on disk — deleted files that appear in git diff
output are excluded. Supported file types:
- `.abap` — all ABAP source files (CLAS, INTF, PROG, FUGR, ENHO, etc.)
- `.asddls` — CDS view / view entity sources (DDLS)
- `.bdef.asbdef` — RAP behavior definitions (BDEF)
- `.ddlx.asddlxs` — CDS metadata extensions (DDLX)

SRVD (`.srvdsrv`/`.assrvd`) and DCLS (`.asdcls`) sources are not parsed by abaplint and remain excluded. Source recognition is not native SAP semantic validation.

---

## How It Works

1. Resolve supported ABAP/DDLS/BDEF/DDLX sources to lint (from `--files` or git diff)
2. Load the abaplint config (`.abaplint.json` by default). For selected BDEF/DDLX sources, enable `bdef_parser_error`/`cds_parser_error` when omitted; explicit rule settings are respected.
3. Override `global.files` with selected sources, their XML companions and existing ABAP dependencies. This includes RAP sources even when the original glob only matched `.abap`.
4. Write the scoped config to a temporary `.abaplint-local.json`
5. Run `npx @abaplint/cli@latest` in checkstyle mode to a temp file, then filter output to only the originally changed files (dependency files are included for cross-reference resolution but their issues are suppressed)
6. Print results as human-readable text (or write checkstyle XML if `--outformat checkstyle`)
7. Delete the temporary config and raw output files
8. Exit with non-zero if any issues were found in changed files, or abaplint could not produce a usable report

---

## Output

### No Issues

```
Linting 2 file(s):
  src/zcl_my_class.clas.abap
  src/zcl_my_class.clas.testclasses.abap

(abaplint output — no issues reported)
```

### With Issues

```
Linting 1 file(s):
  src/zcl_my_class.clas.abap

src/zcl_my_class.clas.abap(42): keyword_case: Expected upper case keyword
src/zcl_my_class.clas.abap(87): prefer_inline: use inline declaration
```

### No Changed Files

```
No changed lintable ABAP/CDS/RAP sources found — nothing to lint.
```

### CheckStyle output (CI)

```bash
abapgit-agent lint --base main --outformat checkstyle --outfile reports/abaplint-results.xml
```

Produces a standard CheckStyle XML file consumable by Jenkins, GitHub Actions, etc.

---

## Error Handling

| Error | Message |
|-------|---------|
| Config not found | `Error: abaplint config not found: <path>` |
| No config at default path | `Error: abaplint config not found: .abaplint.json` — `Run from the project root, or pass --config <path>.` |
| abaplint crashes, uses an incompatible rule, or produces no report | `abaplint failed to produce a usable report: <diagnostic>` — exits non-zero |

---

## When to Use

Use `lint` as part of the development workflow after making changes, before committing:

```
syntax → lint → commit → push → pull --sync-xml
```

`lint` complements `syntax`:
- `syntax` — checks ABAP syntax by sending the file to the ABAP system (requires connection)
- `lint` — checks static analysis rules offline (no connection needed, catches style/quality issues)

`lint` is also used by CI pipelines to report abaplint findings per pull request.

---

## Example — Daily Workflow

```bash
# 1. Edit ABAP file
# 2. Check syntax against ABAP system
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# 3. Run static analysis offline
abapgit-agent lint

# 4. Commit and push
git add src/zcl_my_class.clas.abap
git commit -m "feat: ..."
git push

# 5. Pull and activate
abapgit-agent pull --files src/zcl_my_class.clas.abap --sync-xml
```

## Example — CI Pipeline

```bash
# Lint only files changed in this PR (CHANGE_TARGET is set automatically by Jenkins)
abapgit-agent lint --outformat checkstyle --outfile reports/abaplint-results.xml
```
