# Installation & Setup

This guide covers how to set up abapgit-agent to enable AI assistants (like Claude Code) to work with ABAP repositories.

## Quick Start

```bash
# 1. Deploy agent to your SAP system (one-time setup)
#    See "Step 1: Deploy Agent to ABAP System" below

# 2. Install CLI
npm install -g abapgit-agent

# 3. Initialize your ABAP project (run from git repo root)
abapgit-agent init --folder /src --package ZMY_PACKAGE
# This copies CLAUDE.md, guidelines, and creates .abapGitAgent

# 4. Edit .abapGitAgent with your SAP system details

# 5. Verify
abapgit-agent health

# 6. Pull from git to SAP
abapgit-agent pull
```

## Prerequisites

### ABAP System Requirements

1. **abapGit installed** - Developer version required
   - Follow the [abapGit installation guide](https://docs.abapgit.org/user-guide/getting-started/install.html#install-developer-version)

2. **REST handler deployed** - See Step 1 below

### Local Requirements

- Node.js 16+
- Git

---

## Step 1: Deploy Agent to ABAP System

This is a **one-time setup** for your SAP system.

### 1.1 Deploy ABAP Objects

Use abapGit to deploy all objects from the `/abap` folder.

### 1.2 Create SICF Service

1. Run transaction `SICF`
2. Navigate to: `sap/bc/z_abapgit_agent`
3. If it doesn't exist:
   - Right-click on `sap/bc` → **Create Element**
   - **Service Name**: `Z_ABAPGIT_AGENT`
   - **Handler Class**: `ZCL_ABGAGT_REST_HANDLER`
4. Activate the service

---

## Step 2: Install CLI

```bash
npm install -g abapgit-agent

# Verify installation
abapgit-agent health
```

Or for local development of abapgit-agent:

```bash
cd abapgit-agent
npm link
```

---

## Step 3: Initialize Your ABAP Project

Run this from your ABAP git repository root:

```bash
abapgit-agent init --folder /src --package ZMY_PACKAGE
```

This command:
- Creates `.abapGitAgent` config file
- Copies `CLAUDE.md` for Claude Code integration
- Copies `guidelines/` folder for ABAP reference

After initialization, edit `.abapGitAgent` with your SAP system details. See [init command docs](docs/init-command.md) for configuration options.

Verify: `abapgit-agent health` should return `{"status": "OK", "version": "1.x.x"}`.

---

## Step 4: Import Existing Package (Optional)

If your ABAP package already has objects in SAP that you want to add to git:

1. First, create the abapGit repository in ABAP:
   - Option A: `abapgit-agent create`
   - Option B: Create in abapGit UI

2. Then import objects to git:
   ```
   abapgit-agent import --message "Initial import from ZMY_PACKAGE"
   ```

This stages, commits, and pushes all objects from the package to your git repository.

See [create](docs/create-command.md) and [import](docs/import-command.md) command docs for more details.

---

## Architecture

```
Claude (VS Code) → CLI Tool → ABAP System (REST/HTTP)
                                  ↓
                    Result + Error Feedback
```

- **CLI Tool** - Node.js REST client, reads config from `.abapGitAgent`
- **ABAP Backend** - REST handler with endpoints for pull, inspect, unit, tree, view, preview
