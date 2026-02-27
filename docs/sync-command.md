---
layout: default
title: sync - Sync ABAP Backend
nav_order: 3
parent: Utility Commands
---

# sync Command

Synchronize ABAP backend code with the installed CLI version.

## Overview

After upgrading the `abapgit-agent` npm package, the ABAP backend code needs to be updated to match. The `sync` command automates this process by:
1. Checking version compatibility between CLI and ABAP backend
2. Pulling the matching version from GitHub
3. Activating all ABAP components

## Command

```bash
# Check versions and sync if needed
abapgit-agent sync

# Force sync even if versions match
abapgit-agent sync --force

# Check version status only (no sync)
abapgit-agent sync --check

# Skip confirmation prompt
abapgit-agent sync --yes

# Dry-run mode (show what would be done)
abapgit-agent sync --dry-run

# Specify transport request
abapgit-agent sync --transport DEVK900001

# Use local repository instead of GitHub
abapgit-agent sync --source /path/to/abapgit-agent
```

## Prerequisite

- `.abapGitAgent` file exists with valid credentials
- Git is installed and available in PATH
- Internet connection (for GitHub mode)
- ABAP system is accessible

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--check` | No | Only check version status, don't sync |
| `--force` | No | Sync even if versions already match |
| `--yes, -y` | No | Skip confirmation prompt |
| `--dry-run` | No | Show what would be done without making changes |
| `--transport` | No | Transport request for activation |
| `--source` | No | Path to local abapgit-agent repository (default: clone from GitHub) |

## Workflow

### 1. Version Check

```bash
CLI version:  1.8.3 (from package.json)
ABAP version: 1.8.0 (from /health endpoint)
```

### 2. If Versions Match

```
✅ Versions match
   CLI:  v1.8.3
   ABAP: v1.8.3

No sync needed.
```

Exit unless `--force` is used.

### 3. If Versions Mismatch

```
⚠️  Version mismatch detected:
    CLI version:  v1.8.3
    ABAP version: v1.8.0

📦 Need to sync ABAP backend code to v1.8.3

This will:
- Clone abapgit-agent repository (tag: v1.8.3)
- Pull 25 ABAP files to your system
- Activate all backend components

Do you want to continue? [Y/n]
```

### 4. Sync Process

```
Cloning repository...
✅ Cloned to /tmp/abapgit-agent-sync-abc123

Checking out v1.8.3...
✅ Checked out tag v1.8.3

Finding ABAP files...
✅ Found 25 ABAP files in abap/ folder

Pulling ABAP files...
📋 Pull Log (25 messages):
────────────────────────────────────────────────────────────────────────────────
Icon │Object                           │Message
─────┼─────────────────────────────────┼────────────────────────────────────────
✅   │CLAS ZCL_ABGAGT_AGENT           │Object imported
✅   │INTF ZIF_ABGAGT_COMMAND         │Object imported
...

✅ Successfully pulled 25 files
✅ All objects activated (25 objects)

Verifying versions...
✅ Sync complete!
   CLI:  v1.8.3
   ABAP: v1.8.3

Cleaning up...
✅ Removed temporary directory
```

## Output

### Success (Versions Match)

```
✅ Versions match
   CLI:  v1.8.3
   ABAP: v1.8.3

No sync needed.
```

### Success (Sync Completed)

```
✅ Sync complete!
   CLI:  v1.8.3
   ABAP: v1.8.3

📦 Synchronized 25 ABAP objects
```

### Check Mode

```bash
$ abapgit-agent sync --check

⚠️  Version mismatch:
    CLI:  v1.8.3
    ABAP: v1.8.0

Run 'abapgit-agent sync' to update ABAP backend.
```

### Dry-Run Mode

```
🔹 DRY RUN MODE - No changes will be made

⚠️  Version mismatch detected:
    CLI version:  v1.8.3
    ABAP version: v1.8.0

Would perform:
1. Clone https://github.com/SylvosCai/abapgit-agent.git
2. Checkout tag v1.8.3
3. Pull 25 ABAP files from abap/ folder
4. Activate all objects
5. Verify version match
6. Clean up temp directory
```

### Error Cases

#### Git Not Available

```
❌ Git not found in PATH

Please install git and try again, or use --source to specify a local repository path.
```

#### Tag Not Found

```
❌ Tag v1.8.3 not found in repository

Available tags:
  v1.8.2
  v1.8.1
  v1.8.0
  ...

Please check if the CLI version matches an available release.
```

#### Pull Failed

```
❌ Failed to pull ABAP files

Error: CLAS ZCL_ABGAGT_AGENT: Syntax error on line 42

Failed Objects (2):
- CLAS ZCL_ABGAGT_AGENT
- CLAS ZCL_ABGAGT_COMMAND_PULL

Please check the error messages above and retry.
```

#### Version Still Mismatched After Sync

```
⚠️  Version verification failed
    Expected: v1.8.3
    Actual:   v1.8.0

The ABAP backend version did not update. Possible causes:
- Objects failed to activate
- ABAP health resource not updated

Please check activation log and retry if needed.
```

## Implementation Details

### Tasks

#### 1. Check CLI Version

```javascript
const cliVersion = require('../../package.json').version;
```

#### 2. Check ABAP Version

```bash
GET /health
```

Response:
```json
{
  "status": "OK",
  "version": "1.8.0"
}
```

#### 3. Clone Repository

```bash
git clone --depth 1 --branch v1.8.3 https://github.com/SylvosCai/abapgit-agent.git /tmp/sync-dir
```

Or use local source:
```bash
# Copy from local path
cp -r /path/to/abapgit-agent /tmp/sync-dir
cd /tmp/sync-dir
git checkout v1.8.3
```

#### 4. Get ABAP Files

```bash
find /tmp/sync-dir/abap -name "*.abap" -o -name "*.asddls"
```

Expected files:
- `abap/zcl_abgagt_*.clas.abap`
- `abap/zif_abgagt_*.intf.abap`
- `abap/zcl_abgagt_resource_*.clas.abap`
- etc.

#### 5. Pull Files

Reuse existing pull command logic:
```javascript
await pull({
  files: abapFiles,
  transport: options.transport,
  url: 'https://github.com/SylvosCai/abapgit-agent.git',
  branch: `v${cliVersion}`
});
```

#### 6. Verify Version

Call `/health` again and compare with CLI version.

#### 7. Cleanup

```bash
rm -rf /tmp/sync-dir
```

## Repository URL Detection

The command needs to know where to clone from:

| Priority | Source | Example |
|----------|--------|---------|
| 1 | `--source` flag | `--source /local/path` or `--source https://...` |
| 2 | Git remote URL | From current directory's `.git/config` (if in abapgit-agent repo) |
| 3 | Default | `https://github.com/SylvosCai/abapgit-agent.git` |

## Configuration

The sync command respects transport configuration:

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` in `.abapGitAgent` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

## Example Usage

### After npm upgrade

```bash
# Upgrade CLI
npm install -g abapgit-agent@latest

# Sync ABAP backend
abapgit-agent sync
```

### Check version status

```bash
abapgit-agent sync --check
```

### Force sync (e.g., to repair broken installation)

```bash
abapgit-agent sync --force
```

### Development workflow (use local changes)

```bash
# Make changes to ABAP files
vim abap/zcl_abgagt_agent.clas.abap

# Sync from local repository
abapgit-agent sync --source . --force
```

## Notes

- The sync command does NOT modify any ABAP objects that are not part of the abapgit-agent backend
- Only files in the `abap/` folder of the repository are synchronized
- The command requires the same permissions as the `pull` command
- Temporary directory is always cleaned up, even on errors
- The `--force` flag is useful for repairing corrupted installations or reverting manual changes

## Related Commands

- [`health`](health-command.md) - Check ABAP system health and version
- [`pull`](pull-command.md) - Pull and activate ABAP objects from git
