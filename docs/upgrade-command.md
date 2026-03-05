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

### Automatic Version Check Reminder

After installing this version, users will automatically see reminders when new versions are available.

#### How It Works

- **Version check frequency:** Once per 24 hours (cached)
- **Reminder display frequency:** Every command (if new version available)
- **Cache location (OS-specific):**
  - Linux/macOS: `~/.cache/abapgit-agent/version-check.json`
  - Windows: `%LOCALAPPDATA%\abapgit-agent\cache\version-check.json`
- **Cache format:** `{ "lastCheck": timestamp, "latestVersion": "1.9.0" }`
- **Network failures:** Silent, won't interrupt workflow

#### Example Output

When running any command, if a new version is available:

```bash
$ abapgit-agent health

✅ ABAP system is healthy
   Version: 1.8.6

💡 New version available: 1.9.0 (current: 1.8.6)
   Run: abapgit-agent upgrade
```

#### Reminder Behavior Timeline

| Time | Command | NPM Check | Reminder |
|------|---------|-----------|----------|
| Day 1, 9:00 AM | `health` | ✅ Checks npm (cache miss) | ✅ Shows |
| Day 1, 10:00 AM | `status` | ✅ Uses cache (no network) | ✅ Shows |
| Day 1, 5:00 PM | `pull` | ✅ Uses cache (no network) | ✅ Shows |
| Day 2, 9:00 AM | `health` | ✅ Cache expired, checks npm | ✅ Shows |
| Day 2, 10:00 AM | `upgrade` | User upgrades | ❌ No more reminders |

**Why persistent reminders?**
- Users won't forget about updates
- No performance cost (uses cached check)
- Self-resolving (disappears after upgrade)
- Non-intrusive (appears at end of output)

### NPM Registry Mirror Support

The upgrade command automatically respects your configured npm registry, supporting corporate and regional mirrors.

#### Supported Registries

```bash
# Check your current registry
npm config get registry

# Common configurations
npm config set registry https://registry.npmjs.org/              # Official npm
npm config set registry https://registry.npmmirror.com/          # Alibaba (China)
npm config set registry https://mirrors.cloud.tencent.com/npm/   # Tencent Cloud
npm config set registry https://your-company.com/npm/            # Corporate proxy
```

#### How It Works

1. **Version checks:** Uses configured registry via `npm config get registry`
2. **CLI upgrades:** Uses configured registry via `npm install -g`
3. **Fallback:** Uses official registry if detection fails
4. **No configuration needed:** Automatic detection

**Example with mirror:**
```bash
$ npm config get registry
https://registry.npmmirror.com/

$ abapgit-agent upgrade --check
# Uses npmmirror for version check automatically
```

### Version Validation

Before attempting any upgrade, the command validates the target version exists in npm registry.

**Validation method:**
```bash
npm view abapgit-agent@X.X.X version
```

**On invalid version:**
```bash
$ abapgit-agent upgrade --version 99.99.99

❌ Error: Version 99.99.99 not found in npm registry
   Please check available versions at: https://www.npmjs.com/package/abapgit-agent?activeTab=versions
```

This prevents failed upgrade attempts and provides helpful guidance.

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

### Actual Error Messages

| Error | Actual Message | Solution |
|-------|----------------|----------|
| npm not available | `❌ Error: npm is not installed or not in PATH`<br>`   Please install Node.js and npm: https://nodejs.org/` | Install Node.js |
| Invalid version | `❌ Error: Version 99.99.99 not found in npm registry`<br>`   Please check available versions at: https://www.npmjs.com/package/abapgit-agent?activeTab=versions` | Check available versions |
| Network failure | `❌ Error: Could not fetch latest version from npm registry`<br>`   Please check your internet connection or specify --version X.X.X` | Check network or use `--version` |
| No ABAP config | `❌ Error: .abapGitAgent config file not found`<br>`   ABAP upgrade requires configuration.`<br>`   Run: abapgit-agent init`<br>`   Or use --cli-only to upgrade CLI package only.` | Run `init` or use `--cli-only` |
| Conflicting flags (--match + --version) | `❌ Error: Cannot use --match and --version together` | Remove one flag |
| Conflicting flags (--match + --cli-only) | `❌ Error: Cannot use --match with --cli-only. --match upgrades ABAP to match CLI version` | Remove `--match` |
| Conflicting flags (--cli-only + --abap-only) | `❌ Error: Cannot use --cli-only and --abap-only together` | Choose one |
| npm install fails | Shows npm error output with suggestions | Check permissions, try with sudo |
| Version mismatch after upgrade | `⚠️  CLI version mismatch: expected v1.9.0, got v1.8.6`<br>`   You may need to restart your terminal or reinstall globally` | Restart terminal or reinstall |
| ABAP pull fails | Shows pull errors from pull command | Run `inspect` for details |
| Git tag not found | abapGit error from pull command | Check git tags exist in repository |

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

**Behavior:**
- Cache reduces npm registry API calls (respects rate limits)
- Cache is automatically created and managed
- Cache expires after 24 hours
- Invalid cache is ignored and regenerated

### Clearing Cache

If you encounter version check issues:

**Linux/macOS:**
```bash
rm ~/.cache/abapgit-agent/version-check.json
```

**Windows:**
```cmd
del %LOCALAPPDATA%\abapgit-agent\cache\version-check.json
```

**Next command will fetch fresh data:**
```bash
abapgit-agent upgrade --check
```

**When to clear cache:**
- Version check returns wrong version
- Network issues during initial check
- Testing version check behavior

## FAQ

### Why do I see the reminder on every command?

The reminder is **persistent by design**:
- ✅ Ensures you don't forget about updates
- ✅ No performance cost (uses cached data)
- ✅ Self-resolving (disappears after upgrade)
- ✅ Non-intrusive (at end of output)

The npm registry is only checked **once per day**, so there's no network overhead.

### Can I disable the reminder?

Currently, the reminder cannot be disabled. It's designed to be non-intrusive and will disappear once you upgrade.

If there's demand for disabling reminders, we can add an environment variable like:
```bash
export ABAPGIT_AGENT_NO_UPDATE_CHECK=1
```

### Why does --cli-only not require ABAP config?

CLI upgrades only interact with npm, not the ABAP system. This allows:
- ✅ Upgrading CLI in any directory
- ✅ Testing new CLI versions without ABAP
- ✅ Development workflow flexibility

### What happens if npm registry is down?

- Version checks fail silently (won't interrupt workflow)
- Uses cached version if available
- Shows error only if you explicitly run `upgrade` command
- You can bypass by using `--version X.X.X` with a known version

### Why use git tags instead of branches?

Git tags are **immutable snapshots** of specific versions:
- ✅ `v1.9.0` always points to same code
- ✅ Tags are standard for releases
- ✅ abapGit treats tags like branches for pulling
- ✅ Ensures version consistency

### Can I rollback to an older version?

Yes! Use `--version` with any previously published version:

```bash
abapgit-agent upgrade --version 1.8.0
```

This works as long as:
- Version exists in npm registry
- Git tag `v1.8.0` exists in repository

### Does upgrading affect my custom ABAP objects?

No! The upgrade only affects `abapgit-agent` backend objects:
- ✅ Only files in abapgit-agent repository are updated
- ✅ Your custom objects remain untouched
- ✅ Only affects ZCL_ABGAGT_* and ZIF_ABGAGT_* objects

### What if CLI and ABAP versions don't match?

You'll see a warning on commands that require ABAP:

```
⚠️  Version mismatch: CLI 1.9.0, ABAP API 1.8.5
   Some commands may not work correctly.
   Update ABAP code: abapgit-agent upgrade --abap-only
```

Use `upgrade --match` to sync them:
```bash
abapgit-agent upgrade --match
```

