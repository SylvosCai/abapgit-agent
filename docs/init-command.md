# init Command

Initialize local configuration for an existing git repository.

## Command

```bash
abapgit-agent init --folder /src --package ZMY_PACKAGE
```

## Prerequisite

- Current folder is the root folder of a git repository
- Git remote is configured (`git remote -v` returns a URL)

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `--folder` | No | `/src` | Subfolder path for ABAP source files |
| `--package` | Yes | - | Target ABAP package |

## Tasks

### 1. Detect Git Remote URL

```bash
git remote get-url origin
```

- Fail if no remote configured

### 2. Copy Configuration Template

```bash
cp .abapGitAgent.example .abapGitAgent
```

### 3. Update Configuration

Add to `.abapGitAgent`:

```json
{
  "package": "ZMY_PACKAGE",
  "folder": "/src"
}
```

### 4. Create Documentation

```bash
cp /path/to/abapgit-agent/abap/CLAUDE.md .
```

### 5. Create Folder

```bash
mkdir -p /src
```

Add `.gitkeep` (optional):

```bash
touch /src/.gitkeep
```

## Output Files

| File | Description |
|------|-------------|
| `.abapGitAgent` | Configuration (user must edit host, user, password, gitUsername, gitPassword) |
| `CLAUDE.md` | ABAP coding guidelines |
| `/src/` | Folder for ABAP source files |

## .abapGitAgent Contents

After `init` command:

```json
{
  "host": "<user must edit>",
  "sapport": 443,
  "client": "100",
  "user": "<user must edit>",
  "password": "<user must edit>",
  "language": "EN",
  "gitUsername": "<user must edit for GitHub>",
  "gitPassword": "<user must edit for GitHub>",
  "package": "ZMY_PACKAGE",
  "folder": "/src"
}
```

## GitHub Credentials

For the `import` command to work, you need to configure GitHub credentials:

1. Create a GitHub Personal Access Token (PAT):
   - GitHub.com: https://github.com/settings/tokens
   - GitHub Enterprise: https://github.tools.sap/settings/tokens

2. Add to `.abapGitAgent`:

```json
{
  "gitUsername": "I045696",
  "gitPassword": "ghp_your_token_here"
}
```

Or set environment variables:

```bash
export GIT_USERNAME="I045696"
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
1. abapgit-agent init --folder /src --package ZMY_PACKAGE
   └─> Creates .abapGitAgent, CLAUDE.md, /src/

2. Edit .abapGitAgent (host, user, password, gitUsername, gitPassword)

3. abapgit-agent create
   └─> Creates online repository in ABAP

4. abapgit-agent import
   └─> Stages, commits, and pushes all objects from ZMY_PACKAGE

5. git pull
   └─> Optionally pull to local folder
```

## Example

```bash
# Initialize
abapgit-agent init --folder /abap --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create repo
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
