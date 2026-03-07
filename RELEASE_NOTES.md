---
title: Release Notes
---

## v1.8.9

### Bug Fixes

- Fix upgrade HTTP 401 and false pull success detection
- Fix upgrade pulling from wrong git remote
- Handle ABAP system unavailable during upgrade

### Improvements

- Write version warnings to stderr instead of stdout

---

---


## v1.8.8

### Bug Fixes

- Fix import command failures for large repositories

---

---


## v1.8.7

### New Features

- `upgrade` command with automatic version check
- Project-level safeguards configuration (`requireFilesForPull`, `disablePull`)

### Bug Fixes

- Fix mocked config for `--match` flag tests
- Add CLI-only and mocked config to all failing upgrade tests

---


## v1.8.6

### New Features

- Async import with background job execution and progress

---

---


## v1.8.5

### New Features

- STOB viewer support for CDS-generated structured objects

### Documentation

- Add STOB viewer documentation

---

---


## v1.8.4

### New Features

- JSON output support for pull, inspect, and unit commands
- `folder-logic` parameter for init command

### Bug Fixes

- HTTP error and JSON parse failure handling
- ABAP error handling in unit command JSON mode

### Documentation

- YouTube video embed in README

---

---


## v1.8.3

### New Features

- Workflow configuration for branch vs trunk development
- Init command now merges with existing config

### Bug Fixes

- Support DDLS files in pull --files filter
- Fix ref command argument parsing
- Fix branch/tag switching in pull command

### Improvements

- Better error messages in init command

### Documentation

- Add CDS view development guidelines

---

---


## v1.8.2

  New Features

  - DDLS (CDS View) support in syntax command
  - FIXPT support in syntax checkers for fixed-point arithmetic handling

  Documentation

  - Update CLAUDE.md and syntax-command.md with DDLS support
  - Add CDS selection patterns and FIXPT guidelines
  - Hide CLAUDE.md, Syntax Command Spec, and Implementation Plan from Jekyll navigation
  - Make README.md first item in Jekyll navigation

---

---


## v1.8.1

See commit history for changes.

---

## v1.8.0

### New Features

- Syntax command for pre-commit validation
- Testclasses auto-detection and syntax checking
- Session caching with automatic retry

### Bug Fixes

- Accurate line numbers in syntax errors
- Testclasses file handling corrections

### Improvements

- Exact filename shown in syntax errors
- In-memory session storage

### Documentation

- Syntax command documentation and examples

---

## v1.7.2

### New Features

- Add offset paging support to preview command
- Add --clone option to ref command for cloning repositories
- Add demo mode to test:cmd for showing command output

### Bug Fixes

- Fix ORDER BY dynamic clause syntax issues
- Handle invalid offset/limit values (negative numbers, NaN)
- Handle offset exceeding total in pagination display
- Fix pagination display in where command
- Include required files in npm package for init command

### Improvements

- Add offset paging support to where command

### Documentation

- Update preview-command.md with paging support
- Update abg-agent.md with offset and limit options for where command
- Update ref-command.md with --clone option

---

## v1.7.1

### New Features

- Added delete command to remove abapGit repositories
- Added status command to check if repo exists in ABAP system

### Bug Fixes

- Fixed path resolution using request header instead of m_path_translated
- Fixed status resource to use GET request instead of POST
- Fixed delete command to use correct repo service method
- Fixed folder format normalization in config with ABAP validation
- Fixed status check to handle lowercase response values

### Improvements

- Implemented dynamic routing in REST handler
- Refactored command extraction from URL for cleaner resource registration

### Documentation

- Added documentation for delete command
- Updated status command documentation with repo existence check

---

---

## v1.7.0

### New Features

- Add where command for where-used list functionality
- Add method name output to where command
- Add source include support to view command
- Add PROG type support to view command
- Add base resource class for common REST handling

### Bug Fixes

- Fix syntax errors in where command implementation
- Fix extended method name handling in include detection
- Fix include name padding and format issues
- Fix type conversion issues in inspect command

### Improvements

- Refactor all REST resources to inherit from base class
- Simplify view command and CLAS viewer implementation
- Refactor inspect command with dependency injection
- Add comprehensive unit test coverage

### Documentation

- Add view and where command usage guide
- Add abstract methods, FINAL class, and TYPE any guidelines
- Add interface type reference guidelines
- Document source include support for view command

---

---

## v1.6.1

### New Features

- Add coverage option to unit command
- Add coverage data retrieval to unit command
- Separate warnings and info messages in inspect output
- Add method name to inspect command output

### Bug Fixes

- Fix method name extraction from SOBJNAME
- Fix syntax warnings in preview and inspect commands
- Handle cx_dd_ddl_read exception in command_view
- Fix parent_package assertions in tree command tests

### Improvements

- Sort errors and warnings by method name and line number
- Simplify method name extraction from SOBJNAME
- Add dependency injection to command_import
- Extract coverage logic to separate method

### Documentation

- Update INSTALL.md and health-command.md
- Add exception handling guidelines for interfaces
- Update inspect command spec and documentation
- Add unit testable code guidelines
- Add coverage option to unit command documentation

---

## v1.6.0

### New Features

- Add `ref` command to search ABAP reference materials
- Add `list` command to list ABAP objects with filtering
- Add CLI aliases (`aga`, `abg`, `abgagt`) for convenience
- Add `--include-types` flag to tree command for object counts

### Bug Fixes

- Auto-convert ISO dates to ABAP format in WHERE clauses
- Support CDS entities in preview command
- Use proper ABAP SQL syntax with @ for host variables
- Fix preview command to set success=false on errors

### Improvements

- Make unit test step optional in workflow
- Add confirmation prompt for release command

### Documentation

- Add CDS syntax reference section to guidelines
- Add CDS Test Double Framework guidance for unit tests
- Clarify when to use inspect vs view commands

---

---

## Release Notes for v1.5.0

### Features
- Add `--variant` parameter to inspect command for Code Inspector variant selection
- Add transport request configuration option (`--transport`, config file, env var)
- Add `--dry-run` flag to test release flow without publishing

### Fixes
- Improve tree command error messages for shell variable issues
- Properly separate warnings and errors in inspect command output
- Fail with error when check variant not found in inspect command
- Fix get_ref method to use EXPORTING/EXCEPTIONS correctly

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

# With object type counts
abapgit-agent tree --package $MY_PACKAGE --include-types

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
