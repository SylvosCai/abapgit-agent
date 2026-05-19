---
layout: default
title: exclude - Exclude Files from Git
nav_order: 5
parent: Setup Commands
---

# exclude Command

Remove git-tracked files matching `exclude.patterns` from git tracking.
ABAP objects are never touched — only git tracking is affected.

## Command

```bash
abapgit-agent exclude
```

## What It Does

1. Reads `exclude.patterns` from `.abapgit-agent.json`
2. Runs `git ls-files` to list all tracked files
3. Filters files matching any of the glob patterns
4. Runs `git rm --cached` to untrack matching files
5. Commits the removal with message: `chore: exclude ABAP objects per .abapgit-agent.json`

The files remain on disk and in the ABAP system — only their git tracking is removed.

## When to Use

Use this command after configuring `exclude` in `.abapgit-agent.json` to clean up files
that were previously tracked but should no longer be:

- **`unsupportedTypes: true`** — catch-all after `import`: removes BADI, W3MI, XSLT, WDYN,
  and any other types abapGit cannot pull or activate
- **`patterns`** — target specific supported-type files by name pattern (e.g. SEGW-generated
  DPC/MPC classes that cause activation failures)

## Configuration

Add an `exclude` section to `.abapgit-agent.json`:

```json
{
  "exclude": {
    "unsupportedTypes": true,
    "patterns": [
      "zcl_*_dpc*.clas.*",
      "zcl_*_mpc*.clas.*"
    ]
  }
}
```

**`unsupportedTypes: true`** — automatically excludes any file whose object type (the middle
dot-segment of the filename, e.g. `badi` in `zfoo.badi.xml`) is not in the set of types
supported by abapgit-agent: CLAS, INTF, PROG, FUGR, TABL, DTEL, TTYP, DOMA, DDLS, DCLS,
MSAG, STRU, ENHO, SUSO. Use this after `import` to remove all types abapGit cannot activate.

**`patterns`** — explicit glob patterns matched against the **basename** of each file
(directory path is stripped). Standard glob semantics: `*` matches any sequence of characters.
Use this to exclude specific supported-type files (e.g. SEGW-generated DPC/MPC classes).

## Effect on Other Commands

Once patterns are configured:

- **`pull --files`** — files matching exclude patterns are skipped with a warning
- **`import`** — excluded files are filtered before staging/committing to git

## Example

```bash
# Configure exclusions in .abapgit-agent.json, then run:
abapgit-agent exclude

# Output:
#   Checking exclude patterns from .abapgit-agent.json...
#   Found 2 pattern(s): zcl_*_dpc*.clas.*, zcl_*_mpc*.clas.*
#
#   Files to remove from git:
#      abap/zcl_zsegw_dpc.clas.abap
#      abap/zcl_zsegw_dpc.clas.xml
#      abap/zcl_zsegw_mpc.clas.abap
#
# ✅ Removed 3 file(s) from git. ABAP objects remain untouched.
#    Committed as: chore: exclude ABAP objects per .abapgit-agent.json
```

## Prerequisite

- Current folder is a git repository
- `.abapgit-agent.json` exists with an `exclude.patterns` array
