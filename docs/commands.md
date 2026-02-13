# Commands Overview

All available CLI commands for abapGit Agent.

## Command Reference

| Command | Status | Description |
|---------|--------|-------------|
| [init](init-command.md) | ✅ | Initialize local configuration |
| [create](create-command.md) | ✅ | Create online repository in ABAP |
| [import](import-command.md) | ✅ | Import objects from ABAP package to git |
| [pull](pull-command.md) | ✅ | Pull and activate objects in ABAP |
| [inspect](inspect-command.md) | ✅ | Syntax check ABAP source files |
| [unit](unit-command.md) | ✅ | Run AUnit tests |
| [health](health-command.md) | ✅ | Health check |
| [status](status-command.md) | ✅ | Status check |

---

## Quick Start

### New Project Setup

```bash
# 1. Initialize local configuration
abapgit-agent init --folder /src --package ZMY_PACKAGE

# 2. Edit .abapGitAgent with credentials
vim .abapGitAgent

# 3. Create online repository in ABAP
abapgit-agent create

# 4. Import objects from ABAP package to git
abapgit-agent import

# 5. Push to git
git push origin main

# 6. Activate in ABAP
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

# Check configuration
abapgit-agent status

# Verify ABAP connection
abapgit-agent health
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
│  create                                         │
│  └── Creates online repo in ABAP               │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  import                                         │
│  └── Stages, commits, pushes objects to git     │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  git push                                       │
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
