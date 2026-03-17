---
layout: default
title: status - Check Status
nav_order: 4
parent: Utility Commands
---

# status Command Requirements

## Overview

> **Requires ABAP Connection**: This command queries the ABAP system to verify repository existence.

Check if ABAP integration is configured for the current repository and whether the repo exists in ABAP system.

## Command

```bash
abapgit-agent status
```

## Tasks

### 1. Check Configuration File

Check if `.abapGitAgent` exists in current directory:

```bash
test -f .abapGitAgent
```

### 2. Check Repo Existence

Query the ABAP system to check if the current git remote URL has an abapGit repository.

### 3. Display Status

| Status | Output |
|--------|--------|
| Configured + Repo Exists | `✅ ABAP Git Agent is ENABLED` with config location and repo details |
| Configured + No Repo | `✅ ABAP Git Agent is ENABLED` with "Not created in ABAP system" |
| Not configured | `❌ ABAP Git Agent is NOT configured` |

---

## Output Examples

### Configured - Repo Exists

```
✅ ABAP Git Agent is ENABLED
   Config location: /path/to/repo/.abapGitAgent
   Repository: ✅ Created
      Package: $MY_PACKAGE
      URL: https://github.com/user/repo.git
      Key: abc123
```

### Configured - No Repo

```
✅ ABAP Git Agent is ENABLED
   Config location: /path/to/repo/.abapGitAgent
   Repository: ❌ Not created in ABAP system
      URL: https://github.com/user/repo.git
   Run "abapgit-agent create" to create repository
```

### Not Configured

```
❌ ABAP Git Agent is NOT configured
```

---

## Used By

- `create` command - validates configuration exists before creating repo
- `delete` command - validates repository exists before deleting

## Related Files

| File | Description |
|------|-------------|
| `.abapGitAgent` | Configuration file with ABAP connection details |
| `.abapGitAgent.sample` | Template file |

---

## Example

```bash
$ abapgit-agent status
✅ ABAP Git Agent is ENABLED
   Config location: /Users/user/project/.abapGitAgent
   Repository: ✅ Created
      Package: $MY_PACKAGE
      URL: https://github.com/user/repo.git
      Key: abc123
```
