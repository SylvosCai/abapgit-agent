# ABAP AI Bridge

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using REST API.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

## Quick Start

1. **Install CLI**: From this repo, run `sudo npm link`
2. **Configure repo**: Add `.abapGitAgent` with SAP credentials
3. **Validate code**: Run `abapgit-agent pull` after pushing

See [Creating New ABAP Projects](#creating-new-abap-projects) to set up a new ABAP repository with Claude Code integration.

## System-Level Integration

This package supports **system-level integration**, meaning any ABAP git repository can use it without cloning this repository:

1. **Install globally**: `sudo npm link`
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

| File | Type | Description |
|------|------|-------------|
| `zif_abapgit_agent.intf.abap` | Interface | Type definitions and method signatures |
| `zcl_abapgit_agent.clas.abap` | Class | OO implementation with `pull` method |
| `zcl_abapgit_agent_handler.clas.abap` | Class | REST router for endpoints |
| `zcl_abapgit_agent_pull.clas.abap` | Class | POST /pull handler |
| `zcl_abapgit_agent_health.clas.abap` | Class | GET /health handler |

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
sudo npm link

# Option B: Use from cloned repo
cd abap-ai-bridge
npm install
```

### 3. Configure Repository

Copy `.abapGitAgent.example` to `.abapGitAgent` in your repository root and edit:

```bash
cp .abapGitAgent.example .abapGitAgent
```

Example configuration:
```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "github-username",
  "gitPassword": "github-token"
}
```

Or set environment variables:
- `ABAP_HOST`, `ABAP_PORT`, `ABAP_CLIENT`, `ABAP_USER`, `ABAP_PASSWORD`

## Creating New ABAP Projects

### 1. Create Project Structure

```bash
# Create new ABAP repository
mkdir my-abap-repo
cd my-abap-repo

# Copy CLAUDE.md (tells Claude how to work with ABAP)
cp /path/to/abap-ai-bridge/abap/CLAUDE.md .

# Initialize git
git init
```

### 2. Add ABAP Objects

Copy your ABAP objects to the repo:

```bash
# Copy individual objects
cp /path/to/abap-ai-bridge/abap/zcl_*.abap ./

# Or create new objects following abapGit format
# See: https://github.com/abapGit/abapGit
```

### 3. Configure and Push

```bash
# Create .abapGitAgent config
cp .abapGitAgent.example .abapGitAgent
# Edit .abapGitAgent with your SAP system details

# Initial commit
git add .
git commit -m "Initial ABAP project"

# Add remote and push
git remote add origin https://github.tools.sap/user/my-abap-repo.git
git push -u origin main
```

### 4. Deploy ABAP Objects to SAP

Use abapGit in your SAP system to pull the repository.

## Claude Code Integration

Claude Code automatically reads `CLAUDE.md` in your project root for context:

1. **No local syntax validation** - Claude knows ABAP needs SAP system
2. **Use `abapgit-agent pull`** - For validation after code generation
3. **Reference abapGit docs** - For ABAP syntax and object conventions

See `abap/CLAUDE.md` for detailed ABAP project guidelines.

## Usage

### From Any ABAP Git Repository

1. **Install globally** (optional, for system-level integration):
   ```bash
   sudo npm link
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

# Syntax check for specific object
abapgit-agent syntax-check <object_type> <object_name>

# Health check
abapgit-agent health

# Check integration status
abapgit-agent status

# Run unit tests for a package (planned)
abapgit-agent unit --package <package>

# Run unit tests for specific objects (planned)
abapgit-agent unit --object <type> <name>
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
| GET | `/health` | Health check (also fetches CSRF token) |
| POST | `/pull` | Pull and activate repository |
| POST | `/syntax-check` | Check syntax of specific ABAP object |
| POST | `/unit` | Execute unit tests (planned) |

### POST /pull

Requires CSRF token. First fetch it from `/health`:

```bash
# 1. Get CSRF token and cookies
curl -c cookies.txt -D headers.txt -X GET "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: fetch"

# 2. Extract CSRF token from headers
CSRF=$(grep -i "x-csrf-token" headers.txt | awk '{print $2}' | tr -d '\r')

# 3. Pull repository
curl -X POST "https://your-system:44300/sap/bc/z_abapgit_agent/pull" \
  -H "Content-Type: application/json" \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: $CSRF" \
  -b cookies.txt \
  -u USER:PASSWORD \
  -d '{"url": "https://github.tools.sap/user/repo.git", "branch": "main"}'
```

Request body:
```json
{
  "url": "https://github.tools.sap/user/repo.git",
  "branch": "main",
  "username": "git-username",
  "password": "git-token"
}
```

Response (success):
```json
{"success":"X","job_id":"USER123_20260206_120000","message":"Pull completed successfully"}
```

Response (with activation errors):
```json
{"success":"","job_id":"USER123_20260206_120000","message":"Pull completed with activation errors","error_detail":"Errors/Warnings:\n  - CLAS ZCL_TEST_CLASS: syntax error"}
```

### GET /health

```bash
curl "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD \
  -H "sap-client: 100"
```

Response:
```json
{"status":"OK","version":"1.0.0"}
```

### POST /syntax-check

Check syntax of a specific ABAP object:

```bash
curl -X POST "https://your-system:44300/sap/bc/z_abapgit_agent/syntax-check" \
  -H "Content-Type: application/json" \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: $CSRF" \
  -b cookies.txt \
  -u USER:PASSWORD \
  -d '{"object_type": "CLAS", "object_name": "ZCL_MY_CLASS"}'
```

Request body:
```json
{
  "object_type": "CLAS",
  "object_name": "ZCL_MY_CLASS"
}
```

Response (success):
```json
{"success":"X","object_type":"CLAS","object_name":"ZCL_MY_CLASS","error_count":0,"errors":[]}
```

Response (with errors):
```json
{"success":"","object_type":"CLAS","object_name":"ZCL_MY_CLASS","error_count":2,"errors":[{"line":"15","column":"12","text":"\"MESSAGE\" is not a declaration"},{"line":"20","column":"5","text\":\"Variable \"LV_TEST\" not found\"}]}
```

## API Response Structure

### Pull Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for success, '' for errors |
| `job_id` | String | Job identifier |
| `message` | String | Status message |
| `error_detail` | String | Error details (if any) |

### Syntax Check Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for no errors, '' for errors |
| `object_type` | String | ABAP object type (e.g., 'CLAS', 'PROG') |
| `object_name` | String | ABAP object name |
| `error_count` | Integer | Number of syntax errors found |
| `errors` | Array | List of errors with line, column, text |

## Workflow with Claude Code

1. **Generate code**: Ask Claude Code to write ABAP code
2. **Push to git**: Claude Code commits and pushes to repository
3. **Pull & activate**: Run `abapgit-agent pull` to pull and activate in ABAP
4. **Check results**: Review activation results
5. **Fix errors**: If errors, ask Claude Code to fix them

Example in Claude Code:
```
User: "Create a class ZCL_AI_HELPER with a method HELLO"

Claude Code: [Generates ABAP code and commits to git]

User: Run: abapgit-agent pull

Claude Code terminal: $ abapgit-agent pull
📌 Auto-detected git remote: https://github.tools.sap/I045696/abap-ai-test.git
🚀 Starting pull for: https://github.tools.sap/I045696/abap-ai-test.git
   Branch: main

✅ Pull completed successfully!
   Job ID: CAIS20260207044507

OR (if activation errors)

Claude Code terminal: $ abapgit-agent pull
❌ Pull completed with errors!
   Job ID: CAIS20260207044507
   Message: Pull completed with activation errors

📋 Error Details:
   - CLAS ZCL_AI_SAMPLE: Error updating where-used list...

User: "There's a syntax error. Please fix it in the class."

Claude Code: [Fixes the syntax error and pushes to git]

User: Run: abapgit-agent pull

Claude Code terminal: $ abapgit-agent pull
✅ Pull completed successfully!
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
