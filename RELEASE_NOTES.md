# Release Notes

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
