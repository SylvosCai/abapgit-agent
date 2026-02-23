# delete Command

Delete an abapGit online repository from the ABAP system.

## Command

```bash
abapgit-agent delete
```

## Prerequisite

- `create` command has been run successfully (repository exists in ABAP)
- `.abapGitAgent` file exists with credentials (host, user, password)
- Current folder is git repo root with configured remote

## What It Does

1. **Detect git remote** - Gets URL from `git remote get-url origin`
2. **Find repository** - Looks up the repository in ABAP by URL
3. **Delete repository** - Removes the online repository from ABAP (leaves local files intact)

## Parameters

None. The command auto-detects the repository URL from git remote.

## Output

### Success

```
🗑️  Deleting online repository
   URL: https://github.com/org/repo.git


✅ Repository deleted successfully!
   Key: 0000000001
```

### Error - Repository Not Found

```
❌ Failed to delete repository
   Error: Repository not found
```

### Error - No Remote Configured

```
Error: No git remote configured. Please configure a remote origin.
```

## Important Notes

- **Local files are NOT deleted** - This command only removes the repository link from the ABAP system
- **Git remote is NOT affected** - Your git repository and all commits remain intact
- **Objects in ABAP are NOT deleted** - The ABAP objects (classes, tables, etc.) stay in the system

## When to Use

- Remove an abapGit online repository that is no longer needed
- Clean up test repositories
- Reconfigure a project with different folder structure

## Example

```bash
# Check current status
abapgit-agent status

# Delete the repository from ABAP
abapgit-agent delete

# Verify deletion
abapgit-agent status
```

## Related Commands

- [`create`](create-command.md) - Create a new online repository
- [`status`](status-command.md) - Check repository status
