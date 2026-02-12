# Commands Overview

All available CLI commands for abapGit Agent.

## Command Reference

| Command | Description | File |
|---------|-------------|------|
| [init](init-command.md) | Initialize local configuration | Initialize local configuration for git repo |
| [create](create-command.md) | Create online repository | Create abapGit repo in ABAP system |
| [pull](pull-command.md) | Pull and activate | Pull objects from git and activate in ABAP |
| [inspect](inspect-command.md) | Syntax check | Check ABAP source files for errors |
| [unit](unit-command.md) | Run unit tests | Execute AUnit tests |
| [health](health-command.md) | Health check | Verify ABAP REST API is accessible |
| [status](status-command.md) | Status check | Check if configured for current repo |

---

## Quick Start

### New Project Setup

```bash
# 1. Initialize local configuration
abapgit-agent init --folder /src --package ZMY_PACKAGE

# 2. Edit .abapGitAgent with credentials
vim .abapGitAgent

# 3. Create online repo and import existing objects
abapgit-agent create --import

# 4. Push to git
git push

# 5. Activate in ABAP
abapgit-agent pull
```

### Daily Development

```bash
# Pull changes (auto-detects git remote)
abapgit-agent pull

# Syntax check before commit
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Run tests
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Check configuration
abapgit-agent status

# Verify ABAP connection
abapgit-agent health
```

---

## Command Flow

```
┌─────────────────────────────────────────────────┐
│  init                                           │
│  └── Creates .abapGitAgent, CLAUDE.md, /src/   │
└─────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────┐
│  create --import                                │
│  └── Creates repo in ABAP, imports objects     │
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
