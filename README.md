# ABAP AI Bridge

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using REST API.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

## Architecture

```
Claude (VS Code) → Local Agent (Node.js) → ABAP System (REST/HTTP)
                                    ↓
                  Result + Error Feedback
```

## Components

### ABAP Side - REST Implementation

| Object | Type | Description |
|--------|------|-------------|
| `ZIF_ABAPGIT_AGENT` | Interface | Type definitions and method signatures |
| `ZCL_ABAPGIT_AGENT` | Class | OO implementation with `pull` method |
| `ZCL_ABAPGIT_AGENT_HANDLER` | Class | REST router for all endpoints |
| `ZCL_ABAPGIT_AGENT_PULL` | Class | POST /pull handler |
| `ZCL_ABAPGIT_AGENT_STATUS` | Class | GET /status handler |
| `ZCL_ABAPGIT_AGENT_HEALTH` | Class | GET /health handler |

**Note:** Error details are returned directly in the REST response via `error_detail` field.

### Local Agent (Node.js)
- HTTP server exposing REST API
- REST client for ABAP communication
- Configuration management
- Logging

## Why REST API?

The REST API approach provides several advantages:
- **No OData/Gateway needed** - Simpler setup
- **Standard HTTP** - Easy to debug and test
- **No SEGW required** - Direct ICF handler
- **No native dependencies** - Works on any OS

## Installation

### 1. Install Node.js Dependencies

```bash
cd abap-ai-bridge
npm install
```

### 2. Configure ABAP Connection

Copy and edit `config.example.json`:

```json
{
  "host": "your-sap-system.com",
  "sapport": 44300,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "agent": {
    "port": 3000,
    "pollInterval": 5000
  }
}
```

### 3. Deploy ABAP Objects

Use abapGit to deploy the ABAP objects in `/abap` folder.

### 4. Create REST API Handler (SICF)

1. Run transaction `SICF`
2. Navigate to: `sap/bc/z_abapgit_agent`
3. Create if doesn't exist:
   - Right-click on `sap/bc` → **Create Element**
   - **Service Name**: `Z_ABAPGIT_AGENT`
   - **Handler Class**: `ZCL_ABAPGIT_AGENT_HANDLER`
4. Activate the service

## Usage

### Start the Agent

```bash
npm start
```

The agent will start on `http://localhost:3000`.

### Claude Integration

```bash
# Pull and activate repository
node scripts/claude-integration.js pull --url <git-url> --branch main

# Check job status
node scripts/claude-integration.js status <job-id>

# Health check
node scripts/claude-integration.js health
```

## REST API

The ABAP system exposes these endpoints:

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check |
| POST | `/pull` | Pull and activate |
| GET | `/status?job_id=<id>` | Get job status |

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

### GET /status

```bash
curl "https://your-system:44300/sap/bc/z_abapgit_agent/status?job_id=..." \
  -u USER:PASSWORD
```

Response:
```json
{"job_id":"USER123_20260206_120000","status":"COMPLETED","success":"X","message":"..."}
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

You: "node scripts/claude-integration.js pull --url <repo-url>"

Agent: {"success":"X","job_id":"...","message":"Pull completed successfully"}

OR (if activation errors)

Agent: {"success":"","job_id":"...","message":"Pull completed with activation errors","error_detail":"..."}

You: "There's a syntax error. Please fix it in the class."

Claude: [Fixes the error and pushes to git]
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
