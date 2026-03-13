---
layout: default
title: init - Initialize Configuration
nav_order: 1
parent: Setup Commands
---

# init Command

Initialize local configuration for an existing git repository.

## Command

```bash
abapgit-agent init --package ZMY_PACKAGE [options]
abapgit-agent init --update
```

## Prerequisite

- Current folder is the root folder of a git repository
- Git remote is configured (`git remote -v` returns a URL)

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `--package` | Yes (unless `--update`) | - | Target ABAP package |
| `--folder` | No | `/src/` | Subfolder path for ABAP source files |
| `--folder-logic` | No | `PREFIX` | Folder logic: `PREFIX` or `FULL` (see below) |
| `--update` | No | - | Update `CLAUDE.md` and `guidelines/` to the latest version without touching credentials or config |

### Folder Logic Options

| Value | Description | Example |
|-------|-------------|---------|
| `PREFIX` | Subpackages derive folder names from parent package prefix | Top: `Z_PROJ`, Sub: `Z_PROJ_CORE` → `/core/` |
| `FULL` | Use full package name as folder name | Package: `Z_SOME_PKG` → `/z_some_pkg/` |

**When to use FULL:**
- Packages don't follow a common prefix naming convention
- You get "PREFIX: Unexpected package naming" errors from abapGit

**When to use PREFIX (recommended):**
- Packages follow a naming convention (e.g., all start with `Z_MYPROJECT_`)
- Cleaner folder structure with shorter names

## Tasks

### 1. Detect Git Remote URL

```bash
git remote get-url origin
```

- Fail if no remote configured

### 2. Create or Update Configuration

**If `.abapGitAgent` does NOT exist:**
- Copy from `.abapGitAgent.example` template
- Set `package`, `folder`, and `folderLogic` from CLI parameters (or defaults)
- Initialize with placeholder credentials

**If `.abapGitAgent` already exists (re-init):**
- Read existing configuration
- Update ONLY `package`, `folder`, and `folderLogic` fields
- Keep ALL other settings (host, credentials, workflow, etc.)
- Show what changed

**Example: Copying config from another project**
```bash
# Copy config from project A to project B
cp ../project-a/.abapGitAgent .

# Update package, folder, and folderLogic for project B
abapgit-agent init --package $PROJECT_B_PACKAGE --folder /src/ --folder-logic FULL

# Result: Credentials preserved, only package/folder/folderLogic updated
```

Configuration after update:
```json
{
  "host": "ldcigze.devsys.net.sap",      // ← Preserved
  "user": "MYUSER",                       // ← Preserved
  "password": "mypassword",               // ← Preserved
  "package": "$PROJECT_B_PACKAGE",        // ← Updated
  "folder": "/src/",                      // ← Updated
  "folderLogic": "FULL",                  // ← Updated
  "workflow": { "mode": "branch" }        // ← Preserved
}
```

### 3. Create Folder

```bash
mkdir -p /src/
```

Add `.gitkeep` (optional):

```bash
touch /src/.gitkeep
```

### 5. Update .gitignore

Add sensitive files to `.gitignore`:

```
.abapGitAgent
```

**Note**: `.abapGitAgent` contains credentials and should never be committed.

### 6. Copy Documentation

```bash
cp /path/to/abapgit-agent/abap/CLAUDE.md .
```

## Update Mode (`--update`)

Run `init --update` after upgrading the `abapgit-agent` npm package to get the latest `CLAUDE.md` and guidelines.

**Difference from re-running `init --package X`:**

| | `init --package X` (re-init) | `init --update` |
|---|---|---|
| Purpose | Change package / folder / folderLogic | Refresh documentation to latest version |
| `.abapGitAgent` | Updated (package, folder, folderLogic only) | Never touched |
| `CLAUDE.md` | Overwritten | Overwritten |
| `guidelines/` | Skipped if folder already exists | Always overwritten (except `*.local.md`) |
| Requires `--package` | Yes | No |

```bash
abapgit-agent init --update
```

What it updates:

| File | Behaviour |
|---|---|
| `CLAUDE.md` | Always overwritten with latest version |
| `.github/copilot-instructions.md` | Always overwritten with latest version |
| `guidelines/*.md` | Overwritten (except `*.local.md`) |
| `guidelines/*.local.md` | **Never touched** — project customisations are preserved |
| `.abapGitAgent` | Never touched |

## Output Files

| File | Description |
|------|-------------|
| `.abapGitAgent` | Configuration (user must edit host, user, password, gitUsername, gitPassword) |
| `.gitignore` | Updated with sensitive files |
| `CLAUDE.md` | ABAP coding guidelines |
| `guidelines/` | ABAP coding guidelines (searchable via `ref` command) |
| `guidelines/objects.local.md` | Project naming conventions — **never overwritten** by `--update`, safe to customise |
| `/src/` | Folder for ABAP source files |

## Customising Guidelines

Guidelines in `guidelines/` are overwritten by `init --update` to stay in sync with new abapgit-agent versions.

To add project-specific content that **survives updates**, use `*.local.md` files:

| File | Purpose |
|---|---|
| `guidelines/objects.local.md` | Project naming conventions (created automatically by `init`) |
| `guidelines/my-topic.local.md` | Any other project-specific guideline |

`*.local.md` files are:
- Never overwritten by `init --update`
- Automatically searched by the `ref` command
- Safe to commit to your project repository

**Example** — override naming conventions for a project using `Y` prefix:

```markdown
# Project Naming Conventions (Override)

| Object Type | Prefix | Example |
|---|---|---|
| Class | YCL_ | YCL_MY_CLASS |
| Interface | YIF_ | YIF_MY_INTERFACE |
| CDS View | YC_ | YC_MY_VIEW |
```

## .abapGitAgent Contents

After `init` command, `.abapGitAgent` contains:

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
  "referenceFolder": "~/abap-reference",
  "package": "ZMY_PACKAGE",
  "folder": "/src/",
  "folderLogic": "PREFIX"
}
```

## GitHub Credentials

For the `import` command to work, you need to configure GitHub credentials:

1. Create a GitHub Personal Access Token (PAT):
   - GitHub.com: https://github.com/settings/tokens

2. Add to `.abapGitAgent`:

```json
{
  "gitUsername": "<your-github-username>",
  "gitPassword": "ghp_your_token_here"
}
```

Or set environment variables:

```bash
export GIT_USERNAME="<your-github-username>"
export GIT_PASSWORD="ghp_your_token_here"
```

## Post-Init Steps

1. Edit `.abapGitAgent` with correct values:
   - `host`: SAP system hostname
   - `user`: SAP username
   - `password`: SAP password
   - `gitUsername`: GitHub username (for import command)
   - `gitPassword`: GitHub PAT (for import command)

2. Run `abapgit-agent create` to create abapGit repo in ABAP

3. Run `abapgit-agent import` to import objects to git

## Full Workflow

```
1. abapgit-agent init --folder /src/ --package ZMY_PACKAGE
   └─> Creates .abapGitAgent, CLAUDE.md, /src/, updates .gitignore

2. Edit .abapGitAgent (host, user, password, gitUsername, gitPassword)

3. abapgit-agent create
   └─> Creates online repository in ABAP

4. abapgit-agent import
   └─> Stages, commits, and pushes all objects from ZMY_PACKAGE

5. git pull
   └─> Optionally pull to local folder

6. abapgit-agent pull
   └─> Activate objects in ABAP
```

## Example

```bash
# Initialize with defaults (folder: /src/, folderLogic: PREFIX)
abapgit-agent init --package ZMYPROJECT

# Initialize with custom folder
abapgit-agent init --package ZMYPROJECT --folder /abap/

# Initialize with FULL folder logic
abapgit-agent init --package ZMYPROJECT --folder-logic FULL

# Initialize with all custom options
abapgit-agent init --package ZMYPROJECT --folder /src/ --folder-logic PREFIX

# Update CLAUDE.md and guidelines/ to the latest version (preserves credentials and config)
abapgit-agent init --update

# Edit config
vim .abapGitAgent

# Create repo (uses folderLogic from config)
abapgit-agent create

# Import objects to git
abapgit-agent import

# Pull to local folder
git pull origin main

# Activate in ABAP
abapgit-agent pull
```

## Related Commands

- [`create`](create-command.md) - Create online repository in ABAP
- [`import`](import-command.md) - Import objects from package to git
- [`pull`](pull-command.md) - Pull and activate objects in ABAP
