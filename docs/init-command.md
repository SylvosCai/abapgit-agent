# init Command Requirements

## Overview

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
cp .abapGitAgent.sample .abapGitAgent
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
| `.abapGitAgent` | Configuration (user must edit host, user, password) |
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
  "package": "ZMY_PACKAGE",
  "folder": "/src"
}
```

## Post-Init Steps

1. Edit `.abapGitAgent` with correct values:
   - `host`: SAP system hostname
   - `user`: SAP username
   - `password`: SAP password

2. Run `abapgit-agent create` to create abapGit repo in ABAP

## Full Workflow

```
1. abapgit-agent init --folder /src --package ZMY_PACKAGE
   └─> Creates .abapGitAgent, CLAUDE.md, /src/

2. Edit .abapGitAgent (host, user, password)

3. abapgit-agent create --import
   └─> Creates repo in ABAP, imports objects

4. git push (if --import was used)

5. abapgit-agent pull
   └─> Activates objects in ABAP
```

## Example

```bash
# Initialize
abapgit-agent init --folder /abap --package ZMYPROJECT

# Edit config
vim .abapGitAgent

# Create repo with import
abapgit-agent create --import

# Push to git
git push origin main

# Activate in ABAP
abapgit-agent pull
```
