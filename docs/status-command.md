# status Command Requirements

## Overview

Check if ABAP integration is configured for the current repository.

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

### 2. Display Status

| Status | Output |
|--------|--------|
| Configured | `✅ ABAP Git Agent is ENABLED` with config location |
| Not configured | `❌ ABAP Git Agent is NOT configured` |

---

## Output Examples

### Configured

```
✅ ABAP Git Agent is ENABLED
   Config location: /path/to/repo/.abapGitAgent
```

### Not Configured

```
❌ ABAP Git Agent is NOT configured
```

---

## Used By

- `create` command - validates configuration exists before creating repo

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
```
