---
layout: default
title: All Commands Overview
nav_order: 1
parent: Reference
---

# Commands Overview

All available CLI commands for abapGit Agent.

> **Shortcut Alias:** Use `abgagt` as a shorter alternative to `abapgit-agent`:
> ```bash
> abgagt pull
> abgagt ref "CORRESPONDING"
> abgagt status
> ```

## Command Reference

### ABAP Integration Commands (Require ABAP Connection)

| Command | Status | Description |
|---------|--------|-------------|
| [init](setup/init-command.md) | ✅ | Initialize local configuration (supports --update) |
| [create](setup/create-command.md) | ✅ | Create online repository in ABAP |
| [delete](setup/delete-command.md) | ✅ | Delete abapGit repository from ABAP |
| [drop](development/drop-command.md) | ✅ | Physically delete a single ABAP object from the system |
| [import](setup/import-command.md) | ✅ | Import objects from ABAP package to git (`--branch` to target a specific branch) |
| [pull](development/pull-command.md) | ✅ | Pull and activate objects in ABAP |
| [syntax](development/syntax-command.md) | ✅ | Check syntax of local source files (BEFORE commit) |
| [inspect](development/inspect-command.md) | ✅ | Syntax check activated ABAP objects (AFTER pull) |
| [tree](explore/tree-command.md) | ✅ | Display package hierarchy tree |
| [list](explore/list-command.md) | ✅ | List ABAP objects in package |
| [unit](development/unit-command.md) | ✅ | Run AUnit tests |
| [view](explore/view-command.md) | ✅ | View ABAP object source code from system |
| [where](explore/where-command.md) | ✅ | Where-used list - find object references |
| [preview](explore/preview-command.md) | ✅ | Preview table/CDS view data |
| [dump](explore/dump-command.md) | ✅ | Query short dumps (ST22) from ABAP system |
| [debug](development/debug-command.md) | ✅ | Interactive ABAP debugger via ADT REST API |
| [run](development/run-command.md) | ✅ | Execute ABAP program (`--program`) or class implementing `IF_OO_ADT_CLASSRUN` (`--class`) |
| [transport](development/transport-command.md) | ✅ | List, create, check, and release SAP transport requests |
| [customize](development/customize-command.md) | ✅ | Write a row to a SAP customizing table (delivery class C/E) |
| [status](utility/status-command.md) | ✅ | Status check - verify config and repo existence |
| [health](utility/health-command.md) | ✅ | Health check |
| [upgrade](utility/upgrade-command.md) | ✅ | Upgrade CLI and/or ABAP backend |

### Offline Commands (No ABAP Connection Required)

| Command | Status | Description |
|---------|--------|-------------|
| [lint](development/lint-command.md) | ✅ | Run abaplint static analysis on changed ABAP files (no ABAP connection) |
| [guide](utility/guide-command.md) | ✅ | Show full bundled ABAP development guide |
| [ref](utility/ref-command.md) | ✅ | Search ABAP reference repositories (topics, patterns, export) |

---

## Quick Start

### New Project Setup

```bash
# 1. Initialize local configuration
abapgit-agent init --package ZMY_PACKAGE

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

### Update Existing Project

```bash
# Update files (CLAUDE.md, copilot-instructions.md, guidelines) to latest version
abapgit-agent init --update
```

---

## Daily Development

```bash
# Force clean re-installation of a broken object
abapgit-agent drop --files src/zcl_my_class.clas.abap --pull

# Pull changes from git and activate in ABAP (auto-detects git remote)
abapgit-agent pull

# Pull specific files only (faster for small changes)
abapgit-agent pull --files src/zcl_my_class.clas.abap

# Force pull through a conflict (e.g. deliberate branch switch)
abapgit-agent pull --files src/zcl_my_class.clas.abap --conflict-mode ignore

# Syntax check LOCAL source BEFORE commit (fast, no pull needed)
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Run abaplint static analysis on changed files (offline, no ABAP connection)
abapgit-agent lint
abapgit-agent lint --base main                          # diff against specific branch
abapgit-agent lint --files src/zcl_my_class.clas.abap   # explicit files

# Syntax check ACTIVATED code AFTER pull (uses Code Inspector)
abapgit-agent inspect --files src/zcl_my_class.clas.abap

# Run unit tests
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap

# Display package hierarchy
abapgit-agent tree --package '$MY_PACKAGE'

# List objects in package with filtering
abapgit-agent list --package '$MY_PACKAGE'
abapgit-agent list --package '$MY_PACKAGE' --type CLAS,INTF
abapgit-agent list --package '$MY_PACKAGE' --name 'ZCL_*'

# View object definitions (classes, interfaces, tables, data elements)
abapgit-agent view --objects ZCL_MY_CLASS
abapgit-agent view --objects SFLIGHT --type TABL
abapgit-agent view --objects ZCL_MY_CLASS --full          # all sections, clean source
abapgit-agent view --objects ZCL_MY_CLASS --full --lines  # with dual line numbers for debugging

# Find where an object is used (where-used list)
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER
abapgit-agent where --objects ZIF_ABGAGT_COMMAND
abapgit-agent where --objects ZCL_AGENT --type CLAS

# Preview table/CDS view data
abapgit-agent preview --objects SFLIGHT
abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS

# Query short dumps (ST22) - investigate runtime errors
abapgit-agent dump --date TODAY
abapgit-agent dump --user DEVELOPER --date TODAY --detail 1

# Execute an ABAP program or class (IF_OO_ADT_CLASSRUN) — no SAP GUI needed
abapgit-agent run --program ZMY_REPORT
abapgit-agent run --class ZCL_MY_RUNNER

# Debug ABAP code interactively via ADT
abapgit-agent debug set --files abap/zcl_my_class.clas.abap:42
abapgit-agent debug attach                    # human REPL mode
abapgit-agent debug attach --json             # scripted AI mode (emits JSON, starts daemon)
abapgit-agent debug stack --json
abapgit-agent debug vars --json
abapgit-agent debug step --type over --json
abapgit-agent debug step --type continue --json

# Check configuration
abapgit-agent status

# Verify ABAP connection
abapgit-agent health

# Write a row to a SAP customizing table (delivery class C/E)
abapgit-agent customize ZAPP_CONFIG --set CONFIG_KEY=FEATURE_X CONFIG_VALUE=X --no-transport
abapgit-agent customize ZAPP_CONFIG --set KEY=MYAPP VALUE=active --transport DEVK900001
```

### Offline Reference

```bash
# Read the full bundled ABAP development guide
abapgit-agent guide

# Print only the path to the guide file
abapgit-agent guide --path

# Search for ABAP syntax patterns (no ABAP connection needed)
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"
abapgit-agent ref "CX_SY_"

# View specific topic documentation
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic unit-tests
abapgit-agent ref --topic internal-tables

# List all available topics
abapgit-agent ref --list-topics

# List all reference repositories
abapgit-agent ref --list-repos

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

### Repository Lifecycle

```bash
# Create online repository in ABAP
abapgit-agent create

# Delete repository from ABAP (uses current git remote)
abapgit-agent delete
```

---

## Common Options

### Global Parameters

All commands read configuration from:
- `.abapGitAgent` file in current directory
- Environment variables (`ABAP_HOST`, `ABAP_USER`, `ABAP_PROTOCOL`, etc.)

### File Patterns

| Pattern | Description |
|---------|-------------|
| `zcl_my_class.clas.abap` | ABAP class source |
| `zif_my_intf.intf.abap` | ABAP interface source |
| `zcl_my_test.clas.testclasses.abap` | ABAP test class |
| `zc_my_view.ddls.asddls` | CDS View/Entity |
| `zmy_table.tabl.abap` | Table definition |
| `zmy_prog.prog.abap` | ABAP program |
