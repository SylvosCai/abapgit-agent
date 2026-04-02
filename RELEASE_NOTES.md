---
title: Release Notes
---

## v1.15.1

### Bug Fixes

- Fix FUGR breakpoint ADT URI routing to correct path
- Handle ADT canonical URI canonicalization for FUGR includes

### Improvements

- Debug integration tests verify exact line numbers end-to-end

---

---


## v1.15.0

### New Features

- `view` command supports DOMA, MSAG, FUGR, DCLS object types
- `view --full --fm <name>` shows function module source with line numbers
- `view --full --fm --lines` includes `debug set` breakpoint hint for FMs

### Bug Fixes

- Fix `debug set` routing for FUGR include names (L* prefix)
- Fix PROG breakpoint hint skipping multi-line `PARAMETERS:` blocks

### Documentation

- Add abapGit XML reference for DOMA, MSAG, DCLS object types
- Add dedicated FUGR function group abapGit guide

---

---


## v1.14.5

### Bug Fixes

- Fix breakpoint hints skipping ABAP comment lines in methods

### Documentation

- Add text table and foreign key guides to abapgit.md

---

---


## v1.14.4

### Bug Fixes

- Fix `ZABGAGT_OBJ_META` never updating after pull
- Fix local file read failure discarding all pull metadata
- Fix off-by-one in suffix extraction dropping all remote files

---

---


## v1.14.3

### Bug Fixes

- Fix abaplint dependency resolution in nested project layouts

---

---


## v1.14.2

### Bug Fixes

- Include concrete impl class when scoping abaplint to changed files

---

---


## v1.14.1

### Bug Fixes

- Transport list `--scope task` now includes all task types (S, X, R)

### Documentation

- Added PR creation transport instructions to branch workflow guide
- Transport hook check added as explicit gate before listing transports

---

---


## v1.14.0

### New Features

- New `lint` command for abaplint static analysis on changed files

### Documentation

- Added local abaplint workflow guide for changed files

---

---


## v1.13.7

### New Features

- Add Jenkinsfile with CI pipeline (abaplint, activate, unit, inspect)

### Improvements

- Enforce 18+ abaplint rules across all 179 ABAP source files
- Add `local_variable_names` and `method_parameter_names` naming rules

### Documentation

- Add abaplint guide with safe/unsafe `prefer_inline` patterns
- Clarify unit test writing: prefer `cl_abap_testdouble`, document XML flags
- Add abaplint naming conventions reference (`ref --topic abaplint-local`)

---

---


## v1.13.6

### New Features

- `--junit-output <file>` flag for `inspect` and `unit` commands

### Bug Fixes

- Show network error hint on ENOTFOUND/ECONNREFUSED instead of registration hint

---

---


## v1.13.5

### New Features

- Add `--version` / `-v` flag to CLI
- Show current version in help output

### Bug Fixes

- Fix version check for `http://` npm registry URLs

---

---


## v1.13.4

### New Features

- `--files` now accepts XML-only objects (TABL, STRU, DTEL, TTYP) via `.type.xml` files
- Added probe vs PoC concept split with `pocWorkspace` config support

### Bug Fixes

- Fixed `CX_SY_RANGE_OUT_OF_BOUNDS` crash caused by `.abapgit.xml` passing ABAP file filter
- Guard empty object name in `parse_file_to_object` before string access
- "Activation cancelled" no longer blocks `--sync-xml` diff for already-active objects

### Improvements

- Pull handles abapGit runtime exceptions per-object instead of aborting entire pull

### Documentation

- Updated ABAP guide with XML-only object templates (DTEL, TABL, STRU, TTYP)
- Added object creation workflow diagram covering namespace and PoC/probe branches
- Expanded naming and package assignment rules for SAP vs customer namespace projects

---

---


## v1.13.3

### New Features

- `--sync-xml`: post-pull XML sync via abapGit status diff

### Bug Fixes

- Normalize class XMLs to match abapGit serializer output
- Add UTF-8 BOM to all abapGit XML metadata files

### Documentation

- Add DD03P field-level rules to TABL serializer guidelines

---

---


## v1.13.2

### Bug Fixes

- Fixed stale endpoint URLs in `syntaxCheck` and `where` methods

### Improvements

- Refactored command registry into dedicated module
- Expanded unit test coverage for `pull.js` and `view.js`

### Documentation

- Added project architecture diagram to README

---

---


## v1.13.1

### Bug Fixes

- `--migrate` now always replaces CLAUDE.md and removes non-local guidelines

---

---


## v1.13.0

### New Features

- `guide` command: view bundled ABAP guidelines ([docs](https://sapcai.github.io/abapgit-agent/guide-command))
- `guide --migrate`: replace local guideline copies with slim stub

### Improvements

- `ref` command now reads guidelines from npm package (no local copies needed)
- `init` no longer copies guideline files; use `guide` command instead

---

---


## v1.12.1

### New Features

- `init`: auto-creates `.abapgit.xml` if missing
- Warns on `pull` when `.abapgit.xml` is absent

### Improvements

- Agent filter refactored for partial object download

### Documentation

- Debug workflow: critical rules added for Rule 11

---

---


## v1.12.0

### New Features

- `run` command supports `scratchWorkspace` and `disableProbeClasses`

### Improvements

- Pull output no longer shows internal job ID

---

---


## v1.11.4

### New Features

- `init --update` detects and removes old numbered guideline files

### Documentation

- Sync copilot-instructions.md with latest CLAUDE.md

---

---


## v1.11.3

### New Features

- Support `*.local.md` for project-specific naming conventions

### Documentation

- Fix SM37 job name pattern and abapGit URL in import docs
- Document `--update` option for init command
- Correct AUnit breakpoint behavior in debug docs
- Add `--full` and `--full --lines` examples to view docs

---

---


## v1.11.2

### New Features

- `view --full --lines`: dual line numbers for breakpoint debugging
- Added `--verbose` flag; HTTP error body details now surfaced

### Bug Fixes

- Debug `set`: validate breakpoint line numbers immediately on creation
- View: global line numbers now computed accurately client-side

### Documentation

- Updated `view` and `debug` command specs for `--full/--lines` split

---

---


## v1.11.1

### New Features

- Add optional `protocol` config field for HTTP support

---

---


## v1.11.0

### New Features

- Transport command: list, create, check, release
- Transport selector with hook support for pull
- Conflict detection mode configurable via project config

### Bug Fixes

- Reliable ABAP work process release in debug sessions
- Fix SYSTEM_EDIT false positive in conflict detector
- BRANCH_SWITCH conflict check is now user-aware

### Improvements

- RTTI provider extracted as injectable global interface

### Documentation

- Document partial download behaviour (fork vs original abapGit)
- Update conflict detection docs for idempotent pull

---

---


## v1.10.1

### New Features

- Detect conflicts during pull operations

### Bug Fixes

- Fix stale SAP_SESSIONID from duplicate cookies

---

---


## v1.10.0

### New Features

- Add `debug` command for interactive ABAP debugging via ADT REST API

---

---


## v1.9.0

### New Features

- Add `dump` command to query ST22 short dumps

---

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
