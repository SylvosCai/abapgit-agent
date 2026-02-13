# import Command

Import existing ABAP objects from a package into an existing abapGit online repository.

## Command

```bash
abapgit-agent import
abapgit-agent import --message "Your commit message"
```

## Prerequisites

- Repository already exists in ABAP (created via abapGit UI or `create` command)
- Package has objects to import
- GitHub credentials configured in `.abapGitAgent`

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
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

1. **Validate** - Check repository URL exists
2. **Find Repo** - Locate abapGit repository by git URL
3. **Configure Credentials** - Set git credentials from config
4. **Refresh** - Get latest local files from package
5. **Stage** - Create stage with all files
6. **Commit** - Commit with message
7. **Push** - Push to remote

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
✅ Objects imported successfully!
   Files staged: 27
   Commit: feat: initial import from ABAP package AUD_TAG
```

### With Custom Message

```
✅ Objects imported successfully!
   Files staged: 15
   Commit: My custom import message
```

### Error - Repository Not Found

```
❌ Import failed
   Error: Repository not found. Run "abapgit-agent create" or create in abapGit UI first.
```

### Error - No Objects Found

```
❌ Import failed
   Error: No objects found in package ZMY_PACKAGE
```

### Error - GitHub Credentials

```
❌ Import failed
   Error: Unauthorized access to resource (HTTP 401)
```

## Troubleshooting

### HTTP 401 Unauthorized

GitHub credentials not configured or invalid:

1. Create a GitHub Personal Access Token
2. Add to `.abapGitAgent`:

```json
{
  "gitUsername": "<your-github-username>",
  "gitPassword": "ghp_your_token"
}
```

### Repository Not Found

The repository must exist in abapGit:

1. Run `abapgit-agent create` to create it, OR
2. Create manually in abapGit UI: https://<sap>/sap/bc/zabapgit

### No Objects Found

The package may be empty or objects not yet created:

1. Verify package in SAP GUI: `SE21` → Display Package
2. Ensure objects exist in the package

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

# Create repository in abapGit UI
# https://<sap>/sap/bc/zabapgit → New → Online → Enter git URL

# Develop objects in ABAP...

# Import objects to git
abapgit-agent import
```

## Related Commands

- [`init`](init-command.md) - Initialize local configuration
- [`create`](create-command.md) - Create online repository in ABAP
- [`pull`](pull-command.md) - Pull and activate objects in ABAP
