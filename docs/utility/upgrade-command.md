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

**For all operations:**
- ✅ Internet connection available (for npm registry checks)
- ✅ User has npm permissions (for CLI upgrade)

**For ABAP upgrades** (`upgrade`, `--abap-only`, `--match`):
- ✅ ABAP REST handler is configured and working
- ✅ Current folder has `.abapGitAgent` config file
- ✅ abapgit-agent repository already exists in abapGit system

**For CLI-only operations** (`--cli-only`, `--check`):
- ✅ No ABAP configuration needed
- ✅ Works from any directory

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
  CLI:  v1.9.0
  ABAP: v1.9.0

This will:
  1. Upgrade npm package: abapgit-agent@1.9.0
  2. Pull ABAP code from git tag v1.9.0
  3. Activate all backend components

Do you want to continue? [Y/n]
```

### 3. Execute CLI Upgrade

```
📦 Upgrading CLI to v1.9.0...
   Running: npm install -g abapgit-agent@1.9.0

added 1 package in 2s
✅ CLI upgraded to v1.9.0
```

### 4. Execute ABAP Upgrade

```
📦 Upgrading ABAP backend to v1.9.0...
   Using git tag: v1.9.0

🚀 Starting pull for: https://github.com/SylvosCai/abapgit-agent.git
   Branch: v1.9.0

📋 Pull Log (25 messages):
────────────────────────────────────────────────────────────────────────────────
Icon │Object                           │Message
─────┼─────────────────────────────────┼────────────────────────────────────────
✅   │CLAS ZCL_ABGAGT_AGENT           │Object imported
✅   │CLAS ZCL_ABGAGT_RESOURCE_HEALTH │Object imported
✅   │INTF ZIF_ABGAGT_COMMAND         │Object imported
...

📦 Activated Objects (25):
────────────────────────────────────────────────────────────────────────────────
✅ CLAS ZCL_ABGAGT_AGENT
✅ CLAS ZCL_ABGAGT_RESOURCE_HEALTH
...

✅ Pull completed successfully!

✅ ABAP backend upgraded to v1.9.0
```

### 5. Verify Versions

```
🔍 Verifying upgrade...

✅ CLI version verified: v1.9.0
✅ ABAP version verified: v1.9.0

✅ Upgrade complete!
```

## Output

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

### Versions Already Up To Date

```bash
$ abapgit-agent upgrade --check

Current versions:
  CLI:  v1.9.0
  ABAP: v1.9.0

Latest available: v1.9.0

✅ All components are up to date
```

### Dry-Run Mode

```bash
$ abapgit-agent upgrade --dry-run

🔹 DRY RUN - No changes will be made

Current versions:
  CLI:  v1.8.0
  ABAP: v1.7.5

Target versions:
  CLI:  v1.9.0
  ABAP: v1.9.0

Would execute:
  1. npm install -g abapgit-agent@1.9.0
  2. abapgit-agent pull --url <agentRepoUrl> --branch v1.9.0
     (agentRepoUrl from .abapGitAgent config or canonical default)
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

📦 Upgrade Plan:
...
Do you want to continue? [Y/n] y

🚀 Starting upgrade...

📦 Upgrading CLI to v1.9.0...
✅ CLI upgraded to v1.9.0

📦 Upgrading ABAP backend to v1.9.0...
[... pull and activate ...]
✅ ABAP backend upgraded to v1.9.0

🔍 Verifying upgrade...
✅ CLI version verified: v1.9.0
✅ ABAP version verified: v1.9.0

✅ Upgrade complete!
```

### Scenario 3: CLI Only

```bash
$ abapgit-agent upgrade --cli-only

📦 Upgrade Plan:
...
Do you want to continue? [Y/n] y

🚀 Starting upgrade...

📦 Upgrading CLI to v1.9.0...
   Running: npm install -g abapgit-agent@1.9.0
✅ CLI upgraded to v1.9.0

🔍 Verifying upgrade...
✅ CLI version verified: v1.9.0

✅ Upgrade complete!
```

### Scenario 4: ABAP Only

```bash
$ abapgit-agent upgrade --abap-only

📦 Upgrade Plan:
...
Do you want to continue? [Y/n] y

🚀 Starting upgrade...

📦 Upgrading ABAP backend to v1.9.0...
[... pull and activate ...]
✅ ABAP backend upgraded to v1.9.0

🔍 Verifying upgrade...
✅ ABAP version verified: v1.9.0

✅ Upgrade complete!
```

### Scenario 5: Match ABAP to CLI

```bash
$ abapgit-agent upgrade --match

📦 Upgrade Plan:

Current versions:
  CLI:  v1.8.8
  ABAP: v1.7.5

Target versions:
  ABAP: v1.8.8

This will:
  1. Pull ABAP code from git tag v1.8.8
  2. Activate all backend components

Do you want to continue? [Y/n] y

🚀 Starting upgrade...

📦 Upgrading ABAP backend to v1.8.8...
[... pull and activate ...]
✅ ABAP backend upgraded to v1.8.8

🔍 Verifying upgrade...
✅ ABAP version verified: v1.8.8

✅ Upgrade complete!
```

### Scenario 6: Specific Version

```bash
$ abapgit-agent upgrade --version 1.8.5

[... upgrades both CLI and ABAP to v1.8.5 ...]

✅ Upgrade complete!
```

### Scenario 7: Skip Confirmation

```bash
$ abapgit-agent upgrade --yes

🚀 Starting upgrade...

📦 Upgrading CLI to v1.9.0...
✅ CLI upgraded to v1.9.0

📦 Upgrading ABAP backend to v1.9.0...
[... pull and activate ...]
✅ ABAP backend upgraded to v1.9.0

✅ Upgrade complete!
```

### Scenario 8: ABAP System Unreachable

When running `upgrade` (both CLI + ABAP) but the ABAP system cannot be reached:

**Interactive mode:**
```bash
$ abapgit-agent upgrade

⚠️  Could not reach ABAP system: ECONNREFUSED
⚠️  ABAP system is unreachable. Cannot upgrade ABAP backend.
   Continue with CLI upgrade only? [Y/n] y
   Run "abapgit-agent upgrade --abap-only" once the system is back.

🚀 Starting upgrade...
📦 Upgrading CLI to v1.9.0...
✅ CLI upgraded to v1.9.0

✅ Upgrade complete!
```

**With `--yes` flag:**
```bash
$ abapgit-agent upgrade --yes

⚠️  Could not reach ABAP system: ECONNREFUSED
⚠️  ABAP system is unreachable. Cannot upgrade ABAP backend.
   --yes flag set: upgrading CLI only.
   Run "abapgit-agent upgrade --abap-only" once the system is back.

📦 Upgrading CLI to v1.9.0...
✅ CLI upgraded to v1.9.0

✅ Upgrade complete!
```

> **Note:** When using `--abap-only` or `--match` with an unreachable ABAP system, the upgrade fails immediately with an error — there is no CLI-only fallback for those flags.

## agentRepoUrl Configuration

The ABAP upgrade pulls from the canonical abapgit-agent repository. You can override this for forks or mirrors via the `agentRepoUrl` field in `.abapGitAgent`:

```json
{
  "agentRepoUrl": "https://github.com/your-org/abapgit-agent.git"
}
```

If `agentRepoUrl` is not set, the canonical repository is used:
```
https://github.com/SylvosCai/abapgit-agent.git
```

No git credentials from `.abapGitAgent` (`gitUsername`/`gitPassword`) are sent when pulling from the canonical public repo. If you configure a private fork via `agentRepoUrl`, you will also need to ensure the ABAP system can authenticate — abapGit uses its own credential configuration for this.

## Implementation Details

### ABAP Upgrade Pull

The ABAP upgrade uses the `pull` command internally, calling it directly (not via CLI args):

```javascript
await pullCommand.pull(
  agentRepoUrl,    // canonical URL or agentRepoUrl from config
  `v${version}`,  // git tag, e.g. "v1.9.0"
  null,            // no file filter - pull all
  transport,       // transport request (or null)
  loadConfig,
  AbapHttp,
  false,           // non-JSON output
  null             // null = no credentials (public repo)
);
```

Passing `null` as `gitCredentials` ensures the project's SAP internal git credentials are not sent to github.com.

If the pull reports errors (activation failures), the upgrade fails and exits with an error message and a recovery command.

### Automatic Version Check Reminder

After installing this version, users will automatically see reminders when new versions are available.

#### How It Works

- **Version check frequency:** Once per 24 hours (cached)
- **Reminder display:** On stderr, so it never pollutes `--json` output
- **Cache location (OS-specific):**
  - Linux/macOS: `~/.cache/abapgit-agent/version-check.json`
  - Windows: `%LOCALAPPDATA%\abapgit-agent\cache\version-check.json`
- **Cache format:** `{ "lastCheck": timestamp, "latestVersion": "1.9.0" }`
- **Network failures:** Silent, won't interrupt workflow

#### Example Output

When running any command, if a new version is available (written to stderr):

```bash
$ abapgit-agent health
✅ ABAP system is healthy
   Version: 1.8.6

💡 New version available: 1.9.0 (current: 1.8.6)
   Run: abapgit-agent upgrade
```

### NPM Registry Mirror Support

The upgrade command automatically respects your configured npm registry, supporting corporate and regional mirrors.

```bash
# Check your current registry
npm config get registry

# Common configurations
npm config set registry https://registry.npmjs.org/              # Official npm
npm config set registry https://registry.npmmirror.com/          # Alibaba (China)
npm config set registry https://your-company.com/npm/            # Corporate proxy
```

### Version Validation

Before attempting any upgrade, the command validates the target version exists in npm registry.

```bash
npm view abapgit-agent@X.X.X version
```

**On invalid version:**
```bash
$ abapgit-agent upgrade --version 99.99.99

❌ Error: Version 99.99.99 not found in npm registry
   Please check available versions at: https://www.npmjs.com/package/abapgit-agent?activeTab=versions
```

## Transport Request Configuration

Transport request precedence (same as pull command):

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` in `.abapGitAgent` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

## Error Handling

| Error | Message | Solution |
|-------|---------|----------|
| npm not available | `❌ Error: npm is not installed or not in PATH` | Install Node.js |
| Invalid version | `❌ Error: Version X.X.X not found in npm registry` | Check available versions |
| Network failure | `❌ Error: Could not fetch latest version from npm registry` | Check network or use `--version` |
| No ABAP config | `❌ Error: .abapGitAgent config file not found` | Run `init` or use `--cli-only` |
| `--match` + `--version` | `❌ Error: Cannot use --match and --version together` | Remove one flag |
| `--match` + `--cli-only` | `❌ Error: Cannot use --match with --cli-only` | Remove `--match` |
| `--cli-only` + `--abap-only` | `❌ Error: Cannot use --cli-only and --abap-only together` | Choose one |
| npm install fails | npm error output + suggestions | Check permissions, try with sudo |
| ABAP unreachable (combined upgrade) | `⚠️  ABAP system is unreachable. Cannot upgrade ABAP backend.` | Proceed CLI-only or cancel; retry ABAP later with `--abap-only` |
| ABAP pull fails | Pull error output + `abapgit-agent upgrade --abap-only --version X.X.X` | Fix activation errors; retry with shown recovery command |
| CLI version mismatch after upgrade | `⚠️  CLI version mismatch: expected vX, got vY` | Restart terminal or reinstall globally |
| ABAP version mismatch after upgrade | `⚠️  ABAP version mismatch: expected vX, got vY` | Some components may have failed to activate; run `inspect` |

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
- No project git credentials are sent when pulling from the canonical public repo
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
npm view abapgit-agent version
```

## Cache Management

### Version Check Cache

**Location (OS-specific):**
- Linux/macOS: `~/.cache/abapgit-agent/version-check.json`
- Windows: `%LOCALAPPDATA%\abapgit-agent\cache\version-check.json`

**Format:**
```json
{
  "lastCheck": 1709654321000,
  "latestVersion": "1.9.0"
}
```

**TTL:** 24 hours

### Clearing Cache

**Linux/macOS:**
```bash
rm ~/.cache/abapgit-agent/version-check.json
```

**Windows:**
```cmd
del %LOCALAPPDATA%\abapgit-agent\cache\version-check.json
```

## FAQ

### Why do I see the reminder on every command?

The reminder is **persistent by design** — it disappears once you upgrade. The npm registry is only checked **once per day**, so there's no network overhead.

### Can I disable the reminder?

Currently, the reminder cannot be disabled. If there's demand, we can add:
```bash
export ABAPGIT_AGENT_NO_UPDATE_CHECK=1
```

### Why does --cli-only not require ABAP config?

CLI upgrades only interact with npm, not the ABAP system.

### What if npm registry is down?

- Version checks fail silently (won't interrupt workflow)
- Uses cached version if available
- Shows error only if you explicitly run `upgrade` command
- You can bypass by using `--version X.X.X` with a known version

### Why use git tags instead of branches?

Git tags are **immutable snapshots** of specific versions — `v1.9.0` always points to the same code. abapGit treats tags like branches for pulling.

### Can I rollback to an older version?

Yes, use `--version` with any previously published version:

```bash
abapgit-agent upgrade --version 1.8.0
```

This works as long as the version exists in npm registry and the git tag `v1.8.0` exists in the repository.

### Does upgrading affect my custom ABAP objects?

No. The upgrade only affects `abapgit-agent` backend objects (`ZCL_ABGAGT_*` and `ZIF_ABGAGT_*`). Your custom objects remain untouched.

### What if CLI and ABAP versions don't match?

You'll see a warning on stderr for commands that require ABAP:

```
⚠️  Version mismatch: CLI 1.9.0, ABAP API 1.8.5
   Some commands may not work correctly.
   Update ABAP code: abapgit-agent upgrade --match
```

Use `upgrade --match` to sync them:
```bash
abapgit-agent upgrade --match
```
