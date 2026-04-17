---
layout: default
title: import - Import Objects
nav_order: 4
parent: Setup Commands
---

# import Command

Import existing ABAP objects from a package into an existing abapGit online repository.

## Command

```bash
abapgit-agent import
abapgit-agent import --message "Your commit message"
abapgit-agent import --branch main
abapgit-agent import --branch feature/my-branch --message "Your commit message"
```

## Prerequisites

- Repository already exists in ABAP (created via abapGit UI or `create` command)
- Package has objects to import
- GitHub credentials configured in `.abapGitAgent`

## Behavior

The import command runs **asynchronously** as a background job:

- ✅ **No HTTP timeout** - Works for large packages with thousands of objects
- ✅ **Real-time progress** - Shows progress bar with current stage
- ✅ **Non-blocking** - Returns job number immediately
- ✅ **Auto-polling** - Polls every 2 seconds until completion

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `--branch` | No | Auto-detected from current git branch | Branch to push to |
| `--message` | No | `feat: initial import from ABAP package <package>` | Commit message |
| `--help` | No | - | Show help |

## Configuration

### .abapGitAgent

Add GitHub credentials to `.abapGitAgent`:

```json
{
  "gitUsername": "<your-github-username>",
  "gitPassword": "ghp_your_github_token"
}
```

### Environment Variables

```bash
export GIT_USERNAME="<your-github-username>"
export GIT_PASSWORD="ghp_your_github_token"
```

### GitHub Personal Access Token

1. Create a PAT at:
   - GitHub.com: https://github.com/settings/tokens

2. Select scopes:
   - `repo` - Full control of private repositories
   - Or `public_repo` if your repo is public

## How It Works

The import command runs **asynchronously** using a background job with real-time progress updates:

1. **Start Background Job** - Creates ABAP background job
2. **Poll for Status** - Polls every 2 seconds for progress
3. **Display Progress** - Shows progress bar with current stage
4. **Complete** - Shows final results (files staged, time spent)

### Import Stages

The background job progresses through 5 stages:

1. **Find Repository** (10%) - Locate abapGit repository by git URL
2. **Refresh Repository** (30%) - Get latest state from ABAP
3. **Stage Files** (50-70%) - Create stage with all local files from package
4. **Prepare Commit** (70%) - Build commit message and prepare metadata
5. **Push** (90%) - Commit and push to remote repository

## Workflow

### Full Development Cycle

```
1. Develop in ABAP (SAP GUI, ADT)
   └─> Create/modify objects in package

2. abapgit-agent import
   └─> Stages, commits, and pushes to git

3. git pull (optional, in local repo)
   └─> Get files to local folder

4. Repeat
```

## Output

### Success

```
📦 Starting import job
   URL: https://github.com/user/repo.git
   Branch: main

✅ Job started: 14334000

[==============================] Import completed successfully

✅ Import completed successfully!
   Files staged: 3701
   Commit: feat: initial import from ABAP package FRA_HOME_MAIN

⏱️  Time spent: 20 seconds
📈 Stats:
   Job number: 14334000
   Started: 2026-03-04 13:33:40
   Completed: 2026-03-04 13:34:00
```

### Progress Display

During import, a progress bar shows the current stage:

```
[===============               ] Staging local files...
[===========================   ] Pushing to repository...
[==============================] Import completed successfully
```

### With Custom Message

```
📦 Starting import job
   URL: https://github.com/user/repo.git
   Branch: main
   Message: My custom import message

✅ Job started: 14335000

[==============================] Import completed successfully

✅ Import completed successfully!
   Files staged: 15
   Commit: My custom import message

⏱️  Time spent: 8 seconds
📈 Stats:
   Job number: 14335000
   Started: 2026-03-04 14:20:10
   Completed: 2026-03-04 14:20:18
```

### Error - Repository Not Found

```
📦 Starting import job
   URL: https://github.com/user/repo.git
   Branch: main

✅ Job started: 14336000

[===============               ] Finding repository...

❌ Import failed!
   Error: Repository not found
```

### Error - No Objects Found

```
📦 Starting import job
   URL: https://github.com/user/repo.git
   Branch: main

✅ Job started: 14337000

[===========================   ] Staging local files...

❌ Import failed!
   Error: No objects found in package
```

### Error - GitHub Credentials

```
📦 Starting import job
   URL: https://github.com/user/repo.git
   Branch: main

✅ Job started: 14338000

[===========================   ] Pushing to repository...

❌ Import failed!
   Error: Error during import (HTTP 403 Forbidden)
```

## Troubleshooting

### HTTP 401/403 Unauthorized

GitHub credentials not configured or invalid:

1. Create a GitHub Personal Access Token
2. Add to `.abapGitAgent`:

```json
{
  "gitUsername": "<your-github-username>",
  "gitPassword": "ghp_your_token"
}
```

**Note**: The password must be passed exactly as-is (case-sensitive). The import command preserves case sensitivity.

### Repository Not Found

The repository must exist in abapGit:

1. Run `abapgit-agent create` to create it, OR
2. Create manually in abapGit: run transaction `ZABAPGIT` (or `SE38` → `ZABAPGIT`) → New → Online → enter git URL

### No Objects Found

The package may be empty or objects not yet created:

1. Verify package in SAP GUI: `SE21` → Display Package
2. Ensure objects exist in the package

### Job Stuck or Hangs

If the import appears to hang:

1. Check ABAP background job: `SM37` → Job name: `ABGAGT_IMPORT_*`
2. View job log for errors
3. If job failed, error will be shown in CLI after timeout

### Large Package Import

For packages with thousands of objects:

- Import runs asynchronously (no HTTP timeout)
- Progress updates every 2 seconds
- Typical speed: ~100-200 objects per second
- 3700+ objects: ~20-30 seconds

## Full Workflow

### Option 1: abapgit-agent (Recommended)

```bash
# Initialize
abapgit-agent init --folder /abap --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create repository
abapgit-agent create

# Develop objects in ABAP...

# Import objects to git
abapgit-agent import

# Pull to local folder
git pull origin main
```

### Option 2: abapGit UI for Creation

```bash
# Initialize
abapgit-agent init --folder /abap --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create repository in abapGit: run transaction ZABAPGIT → New → Online → Enter git URL

# Develop objects in ABAP...

# Import objects to git
abapgit-agent import
```

## Related Commands

- [`init`](init-command.md) - Initialize local configuration
- [`create`](create-command.md) - Create online repository in ABAP
- [`pull`](pull-command.md) - Pull and activate objects in ABAP

## Project-Level Safeguards

To disable the import command for all developers on a project, add to `.abapgit-agent.json` (checked into the repository):

```json
{
  "safeguards": {
    "disableImport": true,
    "reason": "One-time operation — managed by release manager."
  }
}
```

**When `disableImport: true`:**
- All `import` commands are blocked immediately with an error
- `reason` is displayed if provided
- Error message directs users to contact the project maintainer

### Allow Specific Users to Bypass `disableImport`

Use `importAllowedUsers` to allow one or more SAP users to run import even when `disableImport: true`:

```json
{
  "safeguards": {
    "disableImport": true,
    "importAllowedUsers": ["ALICE", "JOHN"],
    "reason": "Import is restricted to the release manager."
  }
}
```

- Accepts a string (single user) or an array of strings
- User IDs are case-insensitive (matched against the `user` field in `.abapGitAgent`)
- If the current user is in the list, the `disableImport` block is bypassed

### Require a Commit Message

Use `requireImportMessage` to force developers to always supply `--message`:

```json
{
  "safeguards": {
    "requireImportMessage": true,
    "reason": "All imports must be traceable via commit message."
  }
}
```

**When `requireImportMessage: true`:**
- `abapgit-agent import` → ❌ blocked, shows error and usage hint
- `abapgit-agent import --message "Initial import"` → ✅ allowed

### Safeguard Options Reference

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `disableImport` | boolean | `false` | Completely disables the import command |
| `importAllowedUsers` | string \| string[] | `null` | Users who can bypass `disableImport` |
| `requireImportMessage` | boolean | `false` | Forces `--message` to be provided |
| `reason` | string | `null` | Optional explanation shown in error messages |
