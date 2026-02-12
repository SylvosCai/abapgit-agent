# create Command Requirements

## Overview

> **TODO**: This command is not yet implemented.

Create a new abapGit online repository in the ABAP system.

## Command

```bash
# Create online repo only
abapgit-agent create

# Create online repo and import existing objects
abapgit-agent create --import
```

## Prerequisite

- `init` command has been run successfully
- `.abapGitAgent` file exists
- User has edited `.abapGitAgent` with correct credentials (host, user, password)
- Current folder is git repo root

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--import` | No | Import existing objects from package to git repo |

---

## Tasks

### 1. Validate Configuration

```bash
abapgit-agent status
```

- Verify `.abapGitAgent` exists
- Verify git repo is configured

```bash
abapgit-agent health
```

- Verify ABAP connection is healthy

### 2. Read Configuration

Read `.abapGitAgent` and extract:
- `host`, `sapport`, `client`, `user`, `password`, `language`
- `package`
- `folder`

### 3. Get Git Remote URL

```bash
git remote get-url origin
```

### 4. Create abapGit Online Repository

**Endpoint:** `POST /create`

**Request Body:**
```json
{
  "url": "https://github.com/org/repo.git",
  "package": "ZMY_PACKAGE",
  "folder_logic": "FULL",
  "folder": "/src"
}
```

### 5. If `--import`: Pull Existing Objects

- Use `folder` from `.abapGitAgent`
- Pull existing objects from package (no objects if package is empty)
- Write files to folder
- Git commit

---

## Output

### Success (without --import)

```
✅ Online repository created successfully
   URL: https://github.com/org/repo.git
   Package: ZMY_PACKAGE
   Name: repo

Next steps:
   abapgit-agent pull
```

### Success (with --import)

```
✅ Online repository created successfully
   URL: https://github.com/org/repo.git
   Package: ZMY_PACKAGE
   Name: repo

✅ Objects imported from package
   Files: 15
   Commit: feat: initial import from ABAP package ZMY_PACKAGE

Next steps:
   git push
   abapgit-agent pull
```

---

## Error Handling

| Error | Message |
|-------|---------|
| No .abapGitAgent | Run `init` command first |
| Missing credentials | Edit .abapGitAgent (host, user, password) |
| Health check failed | Check ABAP system connection |
| Repo already exists | Repository already exists in ABAP |
| Package not found | Package ZMY_PACKAGE does not exist |
| API error | Failed to create repository |

---

## Full Workflow

```
1. abapgit-agent init --folder /src --package ZMY_PACKAGE
   └─> Creates .abapGitAgent, CLAUDE.md, /src/

2. Edit .abapGitAgent (host, user, password)

3. abapgit-agent online --import
   └─> Creates repo in ABAP, imports objects, commits

4. git push origin main

5. abapgit-agent pull
   └─> Activates objects in ABAP
```

## Example

```bash
# Initialize
abapgit-agent init --folder /abap --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create online repo with import
abapgit-agent online --import

# Push to git
git push origin main

# Activate in ABAP
abapgit-agent pull
```

## API Reference

### Create Online Repository

**Endpoint:** `POST /create`

**Request:**
```
POST /online
Content-Type: application/json

{
  "url": "https://github.com/org/repo.git",
  "package": "ZMY_PACKAGE",
  "folder_logic": "FULL",
  "folder": "/src"
}
```

**Response:**
```
HTTP/2 201 Created
Content-Type: application/json

{
  "key": "REPO_KEY_123",
  "url": "https://github.com/org/repo.git",
  "package": "ZMY_PACKAGE",
  "name": "repo"
}
```

### Error Response

```
HTTP/4xx
Content-Type: application/json

{
  "error": "Repository already exists",
  "message": "Repository with URL https://github.com/org/repo.git already exists"
}
```
