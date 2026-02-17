## v1.4.9

### Bug Fixes
- Fixed release workflow to read release notes from file instead of generating them dynamically via Claude API

### Improvements
- Simplified release workflow by reading pre-generated release notes from RELEASE_NOTES.md

---

## v1.4.8

### Bug Fixes
- Use single-line release notes in GitHub Actions workflow (simplifies release process)

---

Looking at the commits provided:

- `74877ee` - Version tag
- `d82b110` - New feature: draft mode for releases
- `ba51ac4` - Previous release chore (v1.4.6)

## v1.4.7

### New Features
- Use draft mode for releases

### Bug Fixes
- (none)

### Improvements
- (none)

### Documentation
- (none)

---

This is a minor release with only one change related to the release process itself (draft mode for releases in GitHub Actions). There are no code changes, bug fixes, or improvements to the CLI tool in this release.

---

Based on the commits, here's the release notes for v1.4.6:

## v1.4.6

### Improvements
- Simplify release notes generation in GitHub Actions

---

## v1.4.5

### Bug Fixes
- Properly format multiline release notes in GitHub Actions

---

## v1.4.4

### New Features
- Generate release notes in GitHub Actions from commits

---

## v1.4.3

### Bug Fixes
- Extract only latest release notes in GitHub Actions

---

## v1.4.2

### New Features
- Use Claude CLI to generate release notes in release script

### Improvements
- Update ABAP version to 1.4.1

---

## v1.4.0

### New Features

#### preview Command
Preview data from ABAP tables or CDS views directly from the ABAP system:

```bash
# Preview table data (auto-detect type)
abapgit-agent preview --objects SFLIGHT

# Preview CDS view data
abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS

# Preview with row limit
abapgit-agent preview --objects SFLIGHT --limit 20

# Preview with WHERE clause filter
abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA'"

# Preview specific columns only
abapgit-agent preview --objects SFLIGHT --columns CARRID,CONNID,PRICE

# Vertical format (for wide tables)
abapgit-agent preview --objects SFLIGHT --vertical

# Compact mode (truncated values)
abapgit-agent preview --objects SFLIGHT --compact

# JSON output
abapgit-agent preview --objects SFLIGHT --json
```

### Documentation

- Added comprehensive docs/preview-command.md
- Updated API.md with /preview endpoint
- Updated IMPLEMENTATION_PLAN.md with tree, view, preview commands
- Simplified INSTALL.md ABAP components section
- Updated README.md with preview command examples
- Improved CLAUDE.md files with best practices

---

## v1.3.0

### New Features

- **view Command**: Now supports TTYP (Table Type) and DDLS (CDS View)

### Bug Fixes

- Fixed CDS view source display in view command
- Fixed interface exception handling

---

## v1.2.0

### New Features

#### tree Command
Display package hierarchy tree from ABAP system:
```bash
# Basic usage
abapgit-agent tree --package $MY_PACKAGE

# With object counts
abapgit-agent tree --package $MY_PACKAGE --include-objects

# Limit depth
abapgit-agent tree --package $MY_PACKAGE --depth 2

# JSON output for scripting
abapgit-agent tree --package $MY_PACKAGE --json
```

#### view Command
View ABAP object definitions directly from ABAP system without pulling from git:

```bash
# View class definition
abapgit-agent view --objects ZCL_MY_CLASS

# View interface
abapgit-agent view --objects ZIF_MY_INTERFACE

# View table structure
abapgit-agent view --objects SFLIGHT --type TABL

# View data element
abapgit-agent view --objects S_CARR_ID --type DTEL

# View multiple objects
abapgit-agent view --objects ZCL_CLASS1,ZIF_INTERFACE1,ZMY_TABLE

# Auto-detect type from TADIR
abapgit-agent view --objects SFLIGHT

# JSON output for scripting
abapgit-agent view --objects ZCL_MY_CLASS --json
```

**Supported Object Types:**
- CLAS - Global ABAP class
- INTF - Global interface
- TABL - Database table
- STRU - Structure type
- DTEL - Data element

### Improvements

- **Auto-detection**: view command now queries TADIR to automatically detect object types (no need to specify `--type` for most objects)
- **Lowercase support**: Object names and types are case-insensitive
- **Error handling**: Non-existent objects now display clear error messages
- **Cleaner output**: Removed AI metadata block from tree command output

### Bug Fixes

- Fixed non-existent object detection in view command
- Fixed source code display for class/interface definitions
- Fixed table and structure field display formatting
- Fixed data element output with domain information

### Documentation

- Added comprehensive docs/tree-command.md
- Added comprehensive docs/view-command.md
- Updated README.md with tree and view command examples
- Updated CLAUDE.md with view command usage for exploring ABAP objects

---

## v1.1.6

- Added `create` command to create online repository in ABAP system
- Added `import` command to import objects from ABAP package to git
- Added unit tests for create and import commands
- Added GitHub Actions workflow for automated npm releases
- Improved .gitignore handling in init command
- Updated commands documentation
