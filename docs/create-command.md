# create Command

Create a new abapGit online repository in the ABAP system.

## Command

```bash
abapgit-agent create
```

## Prerequisite

- `init` command has been run successfully
- `.abapGitAgent` file exists with credentials (host, user, password, gitUsername, gitPassword)
- Current folder is git repo root

## What It Does

1. **Detect git remote** - Gets URL from `git remote get-url origin`
2. **Get package** - Reads from `.abapGitAgent`
3. **Get folder** - Reads from `.abapGitAgent` (default: `/src/`)
4. **Create repository** - Calls ABAP REST API to create online repository
5. **Set starting folder** - Configures the folder path in repository settings

## Parameters

None. The command auto-detects all settings from:
- Git remote URL
- `.abapGitAgent` configuration

## Output

### Success

```
🚀 Creating online repository
   URL: https://github.tools.sap/I045696/abgagt-import
   Package: AUD_TAG
   Folder: /abap/
   Name: abgagt-import
   Branch: main


✅ Repository created successfully!
   URL: https://github.tools.sap/I045696/abgagt-import
   Package: AUD_TAG
   Name: abgagt-import

Next steps:
   abapgit-agent import
```

### Error - Repository Already Exists

```
❌ Failed to create repository
   Error: Repository already exists
```

### Error - Missing Configuration

```
❌ Failed to create repository
   Error: Package not configured
```

## Post-Create Steps

After creating the repository:

```bash
# Import objects from ABAP package to git
abapgit-agent import

# Push to git
git push origin main

# Activate in ABAP
abapgit-agent pull
```

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
# Initialize
abapgit-agent init --folder /src/ --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create repo in ABAP
abapgit-agent create

# Import objects to git
abapgit-agent import

# Pull to local folder
git pull origin main

# Activate in ABAP
abapgit-agent pull
```

## Related Commands

- [`init`](init-command.md) - Initialize local configuration
- [`import`](import-command.md) - Import objects from package to git
- [`pull`](pull-command.md) - Pull and activate objects in ABAP
