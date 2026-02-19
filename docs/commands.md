# Commands Overview

All available CLI commands for abapGit Agent.

## Command Reference

### ABAP Integration Commands (Require ABAP Connection)

| Command | Status | Description |
|---------|--------|-------------|
| [init](init-command.md) | ✅ | Initialize local configuration |
| [create](create-command.md) | ✅ | Create online repository in ABAP |
| [import](import-command.md) | ✅ | Import objects from ABAP package to git |
| [pull](pull-command.md) | ✅ | Pull and activate objects in ABAP |
| [inspect](inspect-command.md) | ✅ | Syntax check ABAP source files |
| [tree](tree-command.md) | ✅ | Display package hierarchy tree |
| [list](list-command.md) | ✅ | List ABAP objects in package |
| [unit](unit-command.md) | ✅ | Run AUnit tests |
| [view](view-command.md) | ✅ | View ABAP object source code from system |
| [preview](preview-command.md) | ✅ | Preview table/CDS view data |
| [health](health-command.md) | ✅ | Health check |

### Offline Commands (No ABAP Connection Required)

| Command | Status | Description |
|---------|--------|-------------|
| [ref](ref-command.md) | ✅ | Search ABAP cheat sheets offline |
| [status](status-command.md) | ✅ | Status check |

---

## Quick Start

### New Project Setup

```bash
# 1. Initialize local configuration
abapgit-agent init --folder /src/ --package ZMY_PACKAGE

# 2. Edit .abapGitAgent with credentials
vim .abapGitAgent

# 3. Commit and push new files to git
git add . && git commit -m "Initial commit" && git push origin main

# 4. Create online repository in ABAP
abapgit-agent create

# 5. Import objects from ABAP package to git
abapgit-agent import

# 6. Pull new objects imported from ABAP package
git pull origin main

# 7. Activate in ABAP
abapgit-agent pull
```

---

## Daily Development

```bash
# Pull changes from git and activate in ABAP (auto-detects git remote)
abapgit-agent pull

# Pull specific files only (faster for small changes)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# Syntax check before commit
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Run unit tests
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Display package hierarchy
abapgit-agent tree --package $MY_PACKAGE

# View object definitions (classes, interfaces, tables, data elements)
abapgit-agent view --objects ZCL_MY_CLASS
abapgit-agent view --objects SFLIGHT --type TABL

# Preview table/CDS view data
abapgit-agent preview --objects SFLIGHT
abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS

# Check configuration
abapgit-agent status

# Verify ABAP connection
abapgit-agent health
```

### Offline Reference

```bash
# Search for ABAP syntax patterns (no ABAP connection needed)
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"
abapgit-agent ref "CX_SY_"

# View specific topic documentation
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic unit-tests

# List all available topics
abapgit-agent ref --list-topics

# Get JSON output for scripting
abapgit-agent ref "VALUE #(" --json
```

---

## Command Workflow

```
┌─────────────────────────────────────────────────┐
│  init                                           │
│  └── Creates .abapGitAgent, CLAUDE.md, /src/   │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  git add && git commit && git push              │
│  └── Push initial files to git                  │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  create                                         │
│  └── Creates online repo in ABAP               │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  import                                         │
│  └── Stages, commits, pushes objects to git    │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  git pull                                       │
│  └── Pull imported objects from ABAP package   │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  pull                                          │
│  └── Activates objects in ABAP                  │
└─────────────────────────────────────────────────┘
```

---

## Common Options

### Global Parameters

All commands read configuration from:
- `.abapGitAgent` file in current directory
- Environment variables (`ABAP_HOST`, `ABAP_USER`, etc.)

### File Patterns

| Pattern | Description |
|---------|-------------|
| `zcl_my_class.clas.abap` | ABAP class source |
| `zif_my_intf.intf.abap` | ABAP interface source |
| `zcl_my_test.clas.testclasses.abap` | ABAP test class |
