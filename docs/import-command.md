# import Command Requirements

## Overview

Import existing ABAP objects from a package into an existing abapGit online repository.

## Command

```bash
abapgit-agent import --message "Initial import from ABAP package"
```

## Prerequisites

- `init` command has been run successfully
- `.abapGitAgent` file exists
- Repository already exists in ABAP (created via abapGit UI OR `create` command)
- Package has objects to import

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--message` | No | Commit message (default: `feat: initial import from ABAP package <package>`) |

---

## Workflow

### 1. Validate Configuration

```bash
abapgit-agent status
```

- Verify `.abapGitAgent` exists
- Verify git repo is configured

### 2. Read Configuration

Get git remote URL:
```bash
git remote get-url origin
```

### 3. Get Repository and Package

1. **Find repository** by git URL using `zcl_abapgit_repo_srv=>get_instance( )->get( iv_url )`
2. **Get package** from repository using `ii_repo->get_package( )`
3. **Validate package** has objects to import

### 4. Import Process (All in ABAP)

The entire import happens in the ABAP system:

1. **Get repository** by key
2. **Refresh repository** to get latest local files
3. **Get local files** using `ii_repo->get_files_local()`
4. **Create stage** using `zcl_abapgit_stage`
5. **Add all files** to stage (method: add)
6. **Set committer info** from user record
7. **Commit and push** using `zcl_abapgit_services_git=>commit()`

### 5. Git Operations (Done by abapGit in ABAP)

- `git add .` - Stage all files
- `git commit -m "<message>"` - Commit with message
- `git push origin <branch>` - Push to remote

---

## API Reference

### Import Endpoint

**Endpoint:** `POST /import`

**Request:**
```json
{
  "url": "https://github.tools.sap/I045696/abgagt-import",
  "message": "feat: initial import from ABAP package ZMY_PACKAGE"
}
```

> **Note:** The package is read from the repository configuration in abapGit (`ii_repo->get_package()`).

**Response:**
```json
{
  "success": "X",
  "files_staged": 15,
  "commit_sha": "abc123def456"
}
```

### Error Response
```json
{
  "success": "",
  "error": "Repository not found",
  "message": "No repository found. Run 'abapgit-agent create' or create in abapGit UI."
}
```

---

## Implementation

### ABAP Classes

| Class | Purpose |
|-------|---------|
| `ZCL_ABGAGT_COMMAND_IMPORT` | Import command logic |
| `ZCL_ABGAGT_RESOURCE_IMPORT` | REST endpoint handler |

### ABAP Interface/Factory Changes

| File | Change |
|------|--------|
| `ZIF_ABGAGT_COMMAND.intf.abap` | Add `gc_import` constant |
| `ZCL_ABGAGT_CMD_FACTORY.clas.abap` | Register import command |
| `ZCL_ABGAGT_REST_HANDLER.clas.abap` | Register `/import` route |

### CLI Files

| File | Change |
|------|--------|
| `src/abap-client.js` | Add `import()` method |
| `src/agent.js` | Add `import()` method |
| `bin/abapgit-agent` | Add import command handler |

---

## Output

### Success

```
✅ Objects imported successfully!
   Repository: abgagt-import
   Files staged: 15
   Commit: feat: initial import from ABAP package ZMY_PACKAGE
```

### Success (with custom message)

```
✅ Objects imported successfully!
   Repository: abgagt-import
   Files staged: 15
   Commit: My custom import message
```

### Error - Repository not found

```
❌ Repository not found
   Run "abapgit-agent create" or create in abapGit UI first
```

### Error - No objects to import

```
❌ No objects found in package ZMY_PACKAGE
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Repository not found | `Repository not found. Run "abapgit-agent create" or create in abapGit UI first.` |
| Package empty | `No objects found in package <package>` |
| API error | `Failed to import objects: <error message>` |
| Git push failed | `Failed to push to remote: <error message>` |

---

## Full Workflow

### Option 1: Using abapgit-agent (Recommended)

```
1. abapgit-agent init --folder /src --package ZMY_PACKAGE
   └─> Creates .abapGitAgent, CLAUDE.md, /src/

2. Edit .abapGitAgent (host, user, password)

3. abapgit-agent create
   └─> Creates online repository in ABAP

4. abapgit-agent import
   └─> Stages, commits, and pushes all objects from ZMY_PACKAGE

5. git pull
   └─> Optionally pull to local folder
```

### Option 2: Using abapGit UI

```
1. abapgit-agent init --folder /src --package ZMY_PACKAGE
   └─> Creates .abapGitAgent, CLAUDE.md, /src/

2. Edit .abapGitAgent (host, user, password)

3. Create repository in abapGit UI (https://<sap>/sap/bc/zabapgit)
   └─> Links to your git repo

4. abapgit-agent import
   └─> Stages, commits, and pushes all objects from ZMY_PACKAGE

5. git pull
   └─> Optionally pull to local folder
```

---

## Example

```bash
# Initialize
abapgit-agent init --folder /abap --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create repository using abapgit-agent
abapgit-agent create

# OR create repository in abapGit UI (https://<sap>/sap/bc/zabapgit)

# Import objects from package to git
abapgit-agent import

# Import with custom message
abapgit-agent import --message "Initial import from SAP"

# Pull to local folder (optional)
git pull origin main
```

---

## Related Commands

- [`init`](init-command.md) - Initialize local configuration
- [`create`](create-command.md) - Create online repository in ABAP
