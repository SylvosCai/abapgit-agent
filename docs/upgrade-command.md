---
layout: default
title: upgrade - Upgrade CLI & ABAP
nav_order: 1
parent: Utility Commands
---

# upgrade Command

Upgrade abapgit-agent CLI and/or ABAP backend to a specific version or latest version.

## Overview

After upgrading the `abapgit-agent` npm package, the ABAP backend code needs to be updated to match. The `upgrade` command automates this process by:
1. Checking version compatibility between CLI and ABAP backend
2. Upgrading npm package (if needed)
3. Pulling ABAP code from the matching git tag
4. Verifying versions match after upgrade

## Command

```bash
# Check current versions (both CLI and ABAP)
abapgit-agent upgrade --check

# Upgrade both CLI and ABAP to latest
abapgit-agent upgrade

# Upgrade both to specific version
abapgit-agent upgrade --version 1.9.0

# Upgrade CLI only
abapgit-agent upgrade --cli-only

# Upgrade ABAP only
abapgit-agent upgrade --abap-only

# Make ABAP match current CLI version
abapgit-agent upgrade --match

# Upgrade with options
abapgit-agent upgrade --yes --transport DEVK900001
abapgit-agent upgrade --dry-run
abapgit-agent upgrade --abap-only --version 1.8.0
```

## Prerequisites

1. ✅ ABAP REST handler is configured and working
2. ✅ Current folder has `.abapGitAgent` config file
3. ✅ abapgit-agent repository already exists in abapGit system
4. ✅ User has npm permissions (for CLI upgrade)
5. ✅ Internet connection available

## Parameters

| Flag | Description |
|------|-------------|
| `--check` | Check current versions only, don't upgrade |
| `--cli-only` | Upgrade CLI package only (npm) |
| `--abap-only` | Upgrade ABAP backend only |
| `--match` | Make ABAP version match current CLI version |
| `--version X.X.X` | Upgrade to specific version (default: latest) |
| `--latest` | Upgrade to latest version (explicit, same as default) |
| `--yes, -y` | Skip all confirmation prompts |
| `--dry-run` | Show what would be done without making changes |
| `--transport TR` | Transport request for ABAP activation |

## Flag Combinations

| Command | CLI | ABAP | Version | Notes |
|---------|-----|------|---------|-------|
| `upgrade` | ✅ | ✅ | Latest | Default behavior |
| `upgrade --cli-only` | ✅ | ❌ | Latest | CLI only |
| `upgrade --abap-only` | ❌ | ✅ | Latest | ABAP only |
| `upgrade --match` | ❌ | ✅ | CLI ver | Make ABAP = CLI |
| `upgrade --version X` | ✅ | ✅ | X | Both to version X |
| `upgrade --version X --cli-only` | ✅ | ❌ | X | CLI to X |
| `upgrade --version X --abap-only` | ❌ | ✅ | X | ABAP to X |
| `upgrade --match --version X` | ❌ | ❌ | Error | Invalid combination |
| `upgrade --match --cli-only` | ❌ | ❌ | Error | Invalid combination |

## Workflow

### 1. Check Versions

```
Checking versions...

CLI:  v1.8.0 (current)
ABAP: v1.7.5 (system)

Latest available: v1.9.0
```

### 2. Show Upgrade Plan

```
📦 Upgrade Plan:

Current versions:
  CLI:  v1.8.0
  ABAP: v1.7.5

Target versions:
  CLI:  v1.9.0 (latest)
  ABAP: v1.9.0

This will:
  1. Upgrade npm package: abapgit-agent@1.9.0
  2. Pull ABAP code from git tag v1.9.0 (via abapGit)
  3. Activate all backend components

Do you want to continue? [Y/n]
```

### 3. Execute CLI Upgrade

```
Upgrading CLI...

$ npm install -g abapgit-agent@1.9.0

added 1 package in 2s
✅ CLI upgraded to v1.9.0
```

### 4. Execute ABAP Upgrade

```
Upgrading ABAP backend...

Pulling from tag v1.9.0...
[Using existing abapGit repository in system]

📋 Pull Log (25 messages):
────────────────────────────────────────────────────────────────────────────────
Icon │Object                           │Message
─────┼─────────────────────────────────┼────────────────────────────────────────
✅   │CLAS ZCL_ABGAGT_AGENT           │Object imported
✅   │CLAS ZCL_ABGAGT_RESOURCE_HEALTH │Object imported (version: 1.9.0)
✅   │INTF ZIF_ABGAGT_COMMAND         │Object imported
...

📦 Activated Objects (25):
────────────────────────────────────────────────────────────────────────────────
✅ CLAS ZCL_ABGAGT_AGENT
✅ CLAS ZCL_ABGAGT_RESOURCE_HEALTH
...

✅ ABAP upgraded to v1.9.0
```

### 5. Verify Versions

```
Verifying upgrade...

✅ Upgrade complete!
   CLI:  v1.9.0 ✓
   ABAP: v1.9.0 ✓

Both components are now on version 1.9.0
```

## Output

### Success (Versions Already Match)

```bash
$ abapgit-agent upgrade --check

Current versions:
  CLI:  v1.9.0
  ABAP: v1.9.0

✅ Versions match - no upgrade needed
```

### Success (Upgrade Completed)

```
✅ Upgrade complete!
   CLI:  v1.9.0 ✓
   ABAP: v1.9.0 ✓

Both components upgraded successfully.
```

### Check Mode

```bash
$ abapgit-agent upgrade --check

Current versions:
  CLI:  v1.8.0
  ABAP: v1.7.5

Latest available: v1.9.0

⚠️  Version mismatch detected

To upgrade:
  Both:      abapgit-agent upgrade
  CLI only:  abapgit-agent upgrade --cli-only
  ABAP only: abapgit-agent upgrade --abap-only
  Match:     abapgit-agent upgrade --match
```

### Dry-Run Mode

```bash
$ abapgit-agent upgrade --dry-run

🔹 DRY RUN - No changes will be made

Current versions:
  CLI:  v1.8.0
  ABAP: v1.7.5

Target versions:
  CLI:  v1.9.0 (latest)
  ABAP: v1.9.0

Would execute:
  1. npm install -g abapgit-agent@1.9.0
  2. abapgit-agent pull --branch v1.9.0
     (via existing abapGit repository)
  3. Verify versions match

No changes made.
```

## Scenarios

### Scenario 1: Check Versions Only

```bash
$ abapgit-agent upgrade --check

Current versions:
  CLI:  v1.8.0
  ABAP: v1.7.5

Latest available: v1.9.0

⚠️  Version mismatch detected
```

### Scenario 2: Upgrade Both to Latest

```bash
$ abapgit-agent upgrade

Checking versions...
  CLI:  v1.8.0 → v1.9.0 (latest)
  ABAP: v1.7.5 → v1.9.0

Do you want to continue? [Y/n] y

Upgrading CLI...
✅ CLI upgraded to v1.9.0

Upgrading ABAP...
[... pull and activate ...]
✅ ABAP upgraded to v1.9.0

✅ Upgrade complete!
```

### Scenario 3: CLI Only

```bash
$ abapgit-agent upgrade --cli-only

Upgrading CLI only...

Current: v1.8.0
Target:  v1.9.0 (latest)

$ npm install -g abapgit-agent@1.9.0
...

✅ CLI upgraded to v1.9.0

Note: ABAP backend is still on v1.7.5
Run 'abapgit-agent upgrade --abap-only' to upgrade ABAP
```

### Scenario 4: ABAP Only

```bash
$ abapgit-agent upgrade --abap-only

Upgrading ABAP only...

Current: v1.7.5
Target:  v1.9.0 (latest)

Pulling from tag v1.9.0...
[... pull and activate ...]

✅ ABAP upgraded to v1.9.0

Note: CLI is still on v1.8.0
Run 'abapgit-agent upgrade --cli-only' to upgrade CLI
```

### Scenario 5: Match ABAP to CLI

```bash
$ abapgit-agent upgrade --match

Making ABAP version match CLI...

  CLI:  v1.8.3 (installed)
  ABAP: v1.7.5 (current)

Target: v1.8.3 (match CLI)

Pulling from tag v1.8.3...
[... pull and activate ...]

✅ ABAP upgraded to v1.8.3
✅ Versions now match
```

### Scenario 6: Specific Version

```bash
$ abapgit-agent upgrade --version 1.8.5

Upgrading to v1.8.5...

Current versions:
  CLI:  v1.8.0
  ABAP: v1.7.5

Target: v1.8.5

[... upgrade both to 1.8.5 ...]

✅ Upgrade complete!
   CLI:  v1.8.5
   ABAP: v1.8.5
```

### Scenario 7: Skip Confirmation

```bash
$ abapgit-agent upgrade --yes

Checking versions...
  CLI:  v1.8.0 → v1.9.0
  ABAP: v1.7.5 → v1.9.0

Upgrading CLI...
✅ CLI upgraded to v1.9.0

Upgrading ABAP...
[... pull and activate ...]
✅ ABAP upgraded to v1.9.0

✅ Upgrade complete!
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

#### 3. Fetch Latest Version from npm

```bash
npm view abapgit-agent version
```

Or use npm registry API:
```bash
curl https://registry.npmjs.org/abapgit-agent/latest
```

#### 4. Determine Target Version

| Scenario | Target Version |
|----------|---------------|
| `upgrade` (default) | Latest from npm registry |
| `upgrade --version X.X.X` | X.X.X (specified) |
| `upgrade --match` | Current CLI version |
| `upgrade --latest` | Latest from npm registry (explicit) |

#### 5. Upgrade CLI

```bash
npm install -g abapgit-agent@<version>
```

Use `execSync` to run and capture output.

#### 6. Upgrade ABAP

```bash
# Reuse existing pull command
abapgit-agent pull --branch v<version>
```

This internally calls:
```javascript
await pull({
  branch: `v${targetVersion}`,  // Pull from tag
  transport: options.transport
  // url and credentials come from existing abapGit repo
});
```

#### 7. Verify Versions

- Read CLI version from `package.json`
- Call `/health` endpoint for ABAP version
- Compare both with expected target version

## Transport Request Configuration

Transport request precedence (same as pull command):

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` in `.abapGitAgent` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

## Error Handling

| Error | Response |
|-------|----------|
| npm not available | Show error: "npm not found. Please install Node.js and npm." |
| npm install fails | Show npm error output, suggest manual install |
| Invalid version specified | Show error: "Version X.X.X not found in npm registry" |
| Git tag not found | abapGit returns error: "Tag vX.X.X not found in repository" |
| ABAP pull fails | Show pull errors with details |
| Version mismatch after upgrade | Warn: "Expected vX.X.X but got vY.Y.Y" |
| .abapGitAgent not found | Error: "Config not found. Run 'abapgit-agent init'" |
| ABAP system unreachable | Error: "Cannot connect to ABAP system" |
| abapGit repo not found | Error: "Repository not found. Run 'abapgit-agent create'" |
| Conflicting flags | Error: "--match cannot be used with --version or --cli-only" |

## Example Usage

### After npm upgrade (manual)

```bash
# Manually upgraded CLI
npm install -g abapgit-agent@latest

# Now sync ABAP backend
abapgit-agent upgrade --match
```

### Automated upgrade workflow

```bash
# Upgrade everything to latest
abapgit-agent upgrade --yes
```

### Check before upgrade

```bash
# Check what versions are available
abapgit-agent upgrade --check

# Preview upgrade
abapgit-agent upgrade --dry-run

# Proceed with upgrade
abapgit-agent upgrade
```

### Upgrade to specific version

```bash
# Upgrade both to v1.8.5
abapgit-agent upgrade --version 1.8.5
```

### Development workflow

```bash
# Working on CLI changes locally
npm link

# Update ABAP to match local CLI version
abapgit-agent upgrade --match
```

## Notes

- The upgrade command uses git tags (e.g., `v1.9.0`) to fetch specific versions
- ABAP upgrade reuses the existing abapGit repository in the system
- CLI upgrade requires npm/sudo permissions depending on installation type
- The command does NOT modify any ABAP objects outside abapgit-agent backend
- Only files tracked in the abapgit-agent repository are upgraded
- Transport request handling is the same as the pull command

## Related Commands

- [`health`](health-command.md) - Check ABAP system health and version
- [`pull`](pull-command.md) - Pull and activate ABAP objects from git
- [`status`](status-command.md) - Check configuration and repository status

## Technical Notes

### Why tags work as branches

Git allows using tags anywhere a branch name is expected:
```bash
git checkout v1.8.0          # Works - checkout tag
git clone --branch v1.8.0    # Works - clone specific tag
```

abapGit uses standard git commands, so it can pull from tags just like branches.

### CLI Version Detection

The CLI version is read from `package.json` at runtime:
```javascript
const pkg = require('../../package.json');
console.log(pkg.version); // e.g., "1.9.0"
```

### ABAP Version Detection

The ABAP version is stored in `ZCL_ABGAGT_RESOURCE_HEALTH` and returned by `/health` endpoint:
```abap
lv_json = '{"status":"OK","version":"1.9.0"}'.
```

### Latest Version Detection

Query npm registry to find latest published version:
```bash
curl -s https://registry.npmjs.org/abapgit-agent/latest | jq -r '.version'
```

Or use npm CLI:
```bash
npm view abapgit-agent version
```
