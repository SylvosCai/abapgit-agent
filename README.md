# ABAP AI Bridge

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using REST API.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

## System-Level Integration

This package supports **system-level integration**, meaning any ABAP git repository can use it without cloning this repository:

1. **Install globally**: `npm install -g abap-ai-bridge`
2. **Configure**: Add `.abapGitAgent` to your ABAP repo
3. **Use**: Run `abapgit-agent` from your repo directory

## Architecture

```
Claude (VS Code) → CLI Tool → ABAP System (REST/HTTP)
                                  ↓
                    Result + Error Feedback
```

## Components

### ABAP Side - REST Implementation

| Object | Type | Description |
|--------|------|-------------|
| `ZIF_ABAPGIT_AGENT` | Interface | Type definitions and method signatures |
| `ZCL_ABAPGIT_AGENT` | Class | OO implementation with `pull` method |
| `ZCL_ABAPGIT_AGENT_HANDLER` | Class | REST router for endpoints |
| `ZCL_ABAPGIT_AGENT_PULL` | Class | POST /pull handler |
| `ZCL_ABAPGIT_AGENT_HEALTH` | Class | GET /health handler |

**Note:** Error details are returned directly in the REST response via `error_detail` field.

### CLI Tool (Node.js)
- REST client for ABAP communication
- Configuration management (reads from cwd/.abapGitAgent)
- Auto-detects git remote URL and branch from current directory
- Cookie-based session management

## Why REST API?

The REST API approach provides several advantages:
- **No OData/Gateway needed** - Simpler setup
- **Standard HTTP** - Easy to debug and test
- **No SEGW required** - Direct ICF handler
- **No native dependencies** - Works on any OS

## Installation

### 1. ABAP System Setup

Deploy ABAP objects using abapGit:

1. Use abapGit to deploy the ABAP objects in `/abap` folder
2. Create SICF handler:
   - Run transaction `SICF`
   - Navigate to: `sap/bc/z_abapgit_agent`
   - Create if doesn't exist:
     - Right-click on `sap/bc` → **Create Element**
     - **Service Name**: `Z_ABAPGIT_AGENT`
     - **Handler Class**: `ZCL_ABAPGIT_AGENT_HANDLER`
   - Activate the service

### 2. Install CLI Tool

```bash
# Option A: Install globally for system-level integration
npm install -g abap-ai-bridge

# Option B: Use from cloned repo
cd abap-ai-bridge
npm install
```

### 3. Configure Repository

Create `.abapGitAgent` in your ABAP repository root:

```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN"
}
```

Or set environment variables:
- `ABAP_HOST`, `ABAP_PORT`, `ABAP_CLIENT`, `ABAP_USER`, `ABAP_PASSWORD`

## Usage

### From Any ABAP Git Repository

1. **Install globally** (optional, for system-level integration):
   ```bash
   npm install -g abap-ai-bridge
   ```

2. **Configure your repository** - Create `.abapGitAgent` in repo root:
   ```json
   {
     "host": "your-sap-system.com",
     "sapport": 443,
     "client": "100",
     "user": "TECH_USER",
     "password": "your-password",
     "language": "EN"
   }
   ```

3. **Run from your ABAP repo directory**:
   ```bash
   cd /path/to/your-abap-repo
   abapgit-agent pull
   ```
   The CLI auto-detects the git remote URL and current branch.

### CLI Commands

```bash
# Pull and activate (auto-detects git remote and branch)
abapgit-agent pull

# Pull with specific branch
abapgit-agent pull --branch develop

# Override git URL if needed
abapgit-agent pull --url https://github.tools.sap/user/repo --branch main

# Health check
abapgit-agent health

# Check integration status
abapgit-agent status
```

### Local Development

```bash
# Install dependencies
cd abap-ai-bridge
npm install

# Run from package directory (auto-detects from git)
node bin/abapgit-agent pull

# Or use npm script
npm run pull -- --url <git-url> --branch main
```

## REST API

The ABAP system exposes these endpoints:

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check |
| POST | `/pull` | Pull and activate |

### POST /pull

```bash
curl -X POST https://your-system:44300/sap/bc/z_abapgit_agent/pull \
  -H "Content-Type: application/json" \
  -u USER:PASSWORD \
  -d '{"url": "https://github.com/...", "branch": "main"}'
```

Response (success):
```json
{"success":"X","job_id":"USER123_20260206_120000","message":"Pull completed successfully"}
```

Response (with activation errors):
```json
{"success":"","job_id":"USER123_20260206_120000","message":"Pull completed with activation errors","error_detail":"3 inactive objects (activation errors):\n  - CLAS ZCL_TEST_CLASS\n  - PROG ZTEST_REPORT\n  - FUGR ZTEST_FUGR"}
```

### GET /health

```bash
curl "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD
```

Response:
```json
{"status":"OK","version":"1.0.0"}
```

## API Response Structure

### ty_result Structure

```abap
TYPES: BEGIN OF ty_result,
  success TYPE abap_bool,
  job_id TYPE string,
  message TYPE string,
  error_detail TYPE string,  " Contains inactive objects if activation failed
  activated_count TYPE i,
  failed_count TYPE i,
  started_at TYPE timestampl,
  finished_at TYPE timestampl,
END OF ty_result.
```

## Workflow with Claude

1. **Generate code**: Ask Claude to write ABAP code
2. **Push to git**: Claude pushes code to repository
3. **Pull & activate**: Run agent to pull and activate
4. **Check results**: Get activation results
5. **Fix errors**: If errors, ask Claude to fix

Example:
```
You: "Create a class ZCL_AI_HELPER with a method HELLO"

Claude: [Generates code and commits to git]

You: "abapgit-agent pull"

Agent: 📌 Auto-detected git remote: https://github.tools.sap/...
🚀 Starting pull for: https://github.tools.sap/...
✅ Pull completed successfully!

OR (if activation errors)

Agent: ❌ Pull completed with errors!
   Job ID: ...
   Message: Pull completed with activation errors

📋 Error Details:
   - CLAS ZCL_TEST_CLASS (syntax error)

You: "There's a syntax error. Please fix it in the class."

Claude: [Fixes the error and pushes to git]

You: "abapgit-agent pull"

Agent: ✅ Pull completed successfully!
```

## Error Handling

| Error Type | Response | Handling |
|------------|----------|----------|
| Syntax Error | `success=""`, `error_detail` contains object names | Claude fixes and repulls |
| Activation Error | `success=""`, `error_detail` contains inactive objects | Check TADIR for details |
| Network Timeout | HTTP timeout | Retry request |
| Repository Not Found | `success=""`, message="Repository not found" | Verify URL |

## Troubleshooting

### REST API not accessible

1. Check SICF activation: `sap/bc/z_abapgit_agent`
2. Verify handler class: `ZCL_ABAPGIT_AGENT_HANDLER`
3. Check authorization

### Agent cannot connect to ABAP

1. Verify REST API URL in .abapGitAgent
2. Check credentials in .abapGitAgent
3. Test REST API directly with curl

### Activation errors not shown

1. Check TADIR for inactive objects in the package
2. Verify abapGit settings for activation
3. Check SE80 for object activation status

## License

MIT License
