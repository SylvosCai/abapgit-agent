---
layout: default
title: Installation & Setup
nav_order: 1
parent: Getting Started
---

# Installation & Setup

This guide covers how to set up abapgit-agent to enable AI assistants (like Claude Code) to work with ABAP repositories.

## Quick Start

```bash
# 1. Deploy agent to your SAP system (one-time setup)
#    See "Step 1: Deploy Agent to ABAP System" below

# 2. Install CLI
npm install -g abapgit-agent

# 3. Initialize your ABAP project (run from git repo root)
abapgit-agent init --package ZMY_PACKAGE
# This copies CLAUDE.md, guidelines, and creates .abapGitAgent
# Defaults: --folder /src/ --folder-logic PREFIX

# 4. Choose either legacy flat credentials or central references
#    in .abapGitAgent (see "Choose a connection format" below)

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

- Node.js 18+
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
abapgit-agent init --package ZMY_PACKAGE
```

**Optional parameters:**
- `--folder /src/` (default: `/src/`)
- `--folder-logic PREFIX` (default: `PREFIX`, or use `FULL`)

This command:
- Creates `.abapGitAgent` config file
- Copies `CLAUDE.md` for Claude Code integration
- Copies `guidelines/` folder for ABAP reference

`init` creates the backward-compatible **legacy flat** template. After initialization, choose one connection format below. See [init command docs](setup/init-command.md) for repository settings.

### Choose a connection format

Both formats use the ignored repository-local `.abapGitAgent`, but they store connection details differently. Do not combine `system` with flat SAP endpoint/account fields, or `gitProfile` with flat Git account fields.

**Legacy flat format** — edit the file created by `init`; credentials stay in this repository's ignored file:

```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "protocol": "https",
  "gitUsername": "github-username",
  "gitPassword": "github-token",
  "package": "ZMY_PACKAGE",
  "folder": "/src/",
  "folderLogic": "PREFIX"
}
```

See the repository file `.abapGitAgent.example`.

**Central reference format** — reusable endpoints and provider credentials live in the private personal catalog; the repository keeps only aliases plus its project settings:

```json
{
  "system": "DEV",
  "client": "100",
  "gitProfile": "work-git",
  "package": "ZMY_PACKAGE",
  "folder": "/src/",
  "folderLogic": "PREFIX"
}
```

Set up and bind central mode deliberately:

```bash
abapgit-agent system create DEV --host your-sap-system.com --port 443
abapgit-agent logon create 100 --system DEV --provider file --user TECH_USER
abapgit-agent credential create work-git --origin https://github.com \
  --provider file --user github-username
```

These metadata commands do **not** collect passwords or tokens. For `file` providers, add the secret only to the private catalog entry they created, then replace the repository's flat connection fields with `system`, `client`, and `gitProfile`. Other providers store Bitwarden item IDs or registered CLI-adapter references instead. Compare `examples/configuration/central/repository.abapGitAgent.json` and `examples/configuration/central/systems.json`, then read the [central configuration architecture](architecture/central-systems-client-logons.md) for provider and private-file rules.

Repository-only settings such as `package`, `folder`, `folderLogic`, `workflow`, `referenceFolder`, `testRepos`, and per-client `clientSettings` remain in `.abapGitAgent` in either format.

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

See [create](setup/create-command.md) and [import](setup/import-command.md) command docs for more details.

---

## Architecture

```
Claude (VS Code) → CLI Tool → ABAP System (REST/HTTP)
                                  ↓
                    Result + Error Feedback
```

- **CLI Tool** - Node.js REST client, reads config from `.abapGitAgent`
- **ABAP Backend** - REST handler with endpoints for pull, inspect, unit, tree, view, preview
