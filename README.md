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
                  Result Polling + Error Feedback
```

## Components

### ABAP Side
- `ZIF_ABAPGIT_AGENT` - Interface definitions
- `ZABAPGAGENT_PULL` - Function module to trigger pull
- `ZABAPGAGENT_GET_STATUS` - Function module to get job status
- `ZABAPGAGENT_PULL_JOB` - Report for pull and activation
- `ZCL_ABAPGIT_AGENT_REST` - REST API handler (ICF)

### Database Tables
- `ZABAPGLOG` - Job execution log
- `ZABAPGRES` - Job results
- `ZABAPGERR` - Error messages

### Local Agent (Node.js)
- HTTP server exposing REST API
- REST client for ABAP communication
- Claude integration scripts

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
   - **Handler Class**: `ZCL_ABAPGIT_AGENT_REST`
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

# Wait for completion
node scripts/claude-integration.js wait <job-id>

# Health check
node scripts/claude-integration.js health
```

## REST API

The ABAP system exposes these endpoints:

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check |
| POST | `/pull` | Start pull and activation |
| GET | `/status?job_id=<id>` | Get job status |

### POST /pull

```bash
curl -X POST https://your-system:44300/sap/bc/z_abapgit_agent/pull \
  -H "Content-Type: application/json" \
  -u USER:PASSWORD \
  -d '{"url": "https://github.com/...", "branch": "main"}'
```

Response:
```json
{
  "success": "X",
  "job_id": "USER123_20260205_120000",
  "message": "Job submitted: ..."
}
```

### GET /status

```bash
curl "https://your-system:44300/sap/bc/z_abapgit_agent/status?job_id=..." \
  -u USER:PASSWORD
```

Response:
```json
{
  "job_id": "USER123_20260205_120000",
  "status": "COMPLETED",
  "success": "X",
  "message": "Job ..."
}
```

### GET /health

```bash
curl "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD
```

Response:
```json
{"status":"OK","version":"1.0"}
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

Agent: {"job_id": "...", "message": "Job submitted"}

[After completion]
Agent: {"status": "COMPLETED", "success": "X", "activated": 5}

You: "There's a syntax error in line 15 of the class. Please fix it."

Claude: [Fixes the error and pushes to git]
```

## Troubleshooting

### REST API not accessible

1. Check SICF activation: `sap/bc/z_abapgit_agent`
2. Verify handler class: `ZCL_ABAPGIT_AGENT_REST`
3. Check authorization

### Agent cannot connect to ABAP

1. Verify REST API URL in config.json
2. Check credentials in config.json
3. Test REST API directly with curl

## License

MIT License
