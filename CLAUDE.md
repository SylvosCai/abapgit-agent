# ABAP AI Bridge - CLI Tool Development

This is the **abapgit-agent** CLI tool project - a Node.js application for pulling and activating ABAP code from git repositories.

## Project Structure

```
abap-ai-bridge/
├── bin/
│   └── abapgit-agent        # CLI entry point
├── src/
│   ├── agent.js             # Main agent class
│   ├── abap-client.js       # REST client for ABAP communication
│   ├── config.js            # Configuration management
│   ├── server.js            # HTTP server
│   └── logger.js             # Logging utilities
├── abap/                    # ABAP backend components
│   ├── zcl_abapgit_agent*.clas.abap
│   ├── zif_abapgit_agent.intf.abap
│   └── CLAUDE.md            # ABAP project guidelines (copy to your ABAP repos)
└── tests/
```

## CLI Commands

```bash
# Pull and activate from current git repo
abapgit-agent pull

# Pull specific files only (fast - recommended for iterative development)
abapgit-agent pull --files <file1>,<file2>,...

# Pull from specific branch
abapgit-agent pull --branch <branch>

# Pull from specific URL
abapgit-agent pull --url <git-url>

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```

## Pull Command

### Description
Pull and activate ABAP objects from git repository.

### Usage
```bash
# Auto-detect git remote and branch from current directory
abapgit-agent pull

# Pull specific files only
abapgit-agent pull --files zcl_my_class.clas.abap,zif_my_intf.intf.abap

# Pull from specific branch
abapgit-agent pull --branch develop

# Pull from specific URL (useful for CI/CD)
abapgit-agent pull --url https://github.tools.sap/I045696/my-repo.git

# Combined options
abapgit-agent pull --branch develop --files src/zcl_my_class.clas.abap
```

### File Format
Files are parsed to extract `(obj_type, obj_name)`:
- `zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `zif_my_intf.intf.abap` → INTF, ZIF_MY_INTF
- `src/zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS (subdirectory support)

### Output
```
✅ Pull completed successfully!
   Job ID: CAIS20260208115649
   Message: Pull completed successfully

📋 Pull Log (N messages):
───────────────────────────────────────────────────────────────────────────────
Icon │ Object                      │ Message
...

📦 Activated Objects (N unique):
───────────────────────────────────────────────────────────────────────────────
✅ CLAS ZCL_MY_CLASS
...

❌ Failed Objects Log (M entries):
───────────────────────────────────────────────────────────────────────────────
❌ CLAS ZCL_MY_CLASS: Error message text
Exception: Exception details
```

### Key Behaviors
1. **Activated Objects** - Only includes objects that completed successfully (no errors in log)
2. **Failed Objects Log** - Shows all error messages (duplicates allowed for multiple errors per object)
3. **Error Details** - When errors occur, displays error detail section at the top

## Health Check

### Description
Check if the ABAP REST API is healthy.

### Usage
```bash
abapgit-agent health
```

### Output
```json
{
  "status": "healthy",
  "abap": "connected",
  "version": "1.0.0"
}
```

## Status Check

### Description
Check if ABAP integration is configured for the current repository.

### Usage
```bash
abapgit-agent status
```

### Output
```
✅ ABAP Git Agent is ENABLED
   Config location: /path/to/repo/.abapGitAgent
```

Or if not configured:
```
❌ ABAP Git Agent is NOT configured
```

## Configuration

### File-based (.abapGitAgent)
Create `.abapGitAgent` in repository root:
```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "git-username",
  "gitPassword": "git-token"
}
```

### Environment Variables
```bash
export ABAP_HOST="your-sap-system.com"
export ABAP_PORT=443
export ABAP_CLIENT="100"
export ABAP_USER="TECH_USER"
export ABAP_PASSWORD="your-password"
export ABAP_LANGUAGE="EN"
export GIT_USERNAME="git-username"
export GIT_PASSWORD="git-token"
```

## Development Workflow

### CLI Tool Development

1. Make changes to CLI code (JavaScript)
2. Test locally: `node bin/abapgit-agent pull`
3. Test against real ABAP system
4. Commit and push

### ABAP Backend Development

1. Make changes to ABAP backend (abap/ folder)
2. Pull only changed files (faster):
   ```bash
   abapgit-agent pull --files abap/zcl_my_class.clas.abap
   ```
   Or pull all files:
   ```bash
   abapgit-agent pull
   ```
3. Commit and push
4. Deploy changes via abapGit to your SAP system

### Fast Iteration Workflow

For quick ABAP code changes:
1. Make small change to ABAP file
2. `git add <file> && git commit -m "fix"`
3. `abapgit-agent pull --files <file>` (seconds, not minutes)
4. Verify activation results
5. Repeat until done

## For ABAP Code Generation

**NOTE**: This file is for developing the CLI tool itself. For guidelines on **generating ABAP code** for abapGit repositories, see `/abap/CLAUDE.md`. Copy that file to your ABAP repository root when setting up new projects.
