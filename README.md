# ABAP AI Bridge

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using OData.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

## Architecture

```
Claude (VS Code) → Local Agent (Node.js) → ABAP System (OData/HTTP)
                                    ↓
                  Result Polling + Error Feedback
```

## Components

### ABAP Side
- `ZIF_ABAPGIT_AGENT` - Interface definitions
- `Z_ABAPGIT_AGENT_PULL` - RFC function to trigger background job
- `Z_ABAPGIT_AGENT_PULL_JOB` - Background job for pull and activation
- `ZCL_ABAPGIT_AGENT_ODATA` - OData Model Provider Class (MPC)
- `ZCL_ABAPGIT_AGENT_DP` - OData Data Provider Class (DPC)

### Database Tables
- `Z_ABAPGIT_AGENT_LOG` - Job execution log
- `Z_ABAPGIT_AGENT_RES` - Job results
- `Z_ABAPGIT_AGENT_ERR` - Error messages

### Local Agent (Node.js)
- HTTP server exposing REST API
- OData client for ABAP communication (no RFC SDK needed)
- Claude integration scripts

## Why OData?

The OData approach provides several advantages:
- **No native dependencies** - Works on any OS without SAP RFC SDK
- **Standard HTTP** - Easy to debug and test
- **Gateway integration** - Uses standard SAP Gateway infrastructure
- **Firewall friendly** - Uses standard HTTPS port

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
  "sapport": 443,
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

Use abapGit to deploy the ABAP objects in `/abap` folder:

1. **Create database tables** (SE11):
   - `Z_ABAPGIT_AGENT_LOG`
   - `Z_ABAPGIT_AGENT_RES`
   - `Z_ABAPGIT_AGENT_ERR`

2. **Create interface**: `ZIF_ABAPGIT_AGENT`

3. **Create function group**: `Z_ABAPGIT_AGENT`

4. **Create function modules**:
   - `Z_ABAPGIT_AGENT_PULL`
   - `Z_ABAPGIT_AGENT_GET_STATUS`

5. **Create report**: `Z_ABAPGIT_AGENT_PULL_JOB`

6. **Create OData service** (SEGW):
   - Create project `Z_ABAPGIT_AGENT`
   - Import MPC/DPC classes
   - Entity Types:
     - `PullCommand` (JobId, Status, Message, StartedAt)
     - `JobStatus` (JobId, Status, Success, Message, ActivatedCount, FailedCount, StartedAt, FinishedAt)
     - `LogEntry` (JobId, Timestamp, Type, Message, ObjectType, ObjectName)
     - `HealthCheck` (Status, Version, Timestamp)
   - Generate runtime artifacts
   - Activate and test

7. **Activate OData service** (SICF):
   - Path: `/sap/opu/odata/sap/Z_ABAPGIT_AGENT_SRV`
   - Test in browser: `https://your-system/sap/opu/odata/sap/Z_ABAPGIT_AGENT_SRV/$metadata`

### 4. Configure ICF Service

Ensure the OData service is accessible:
- Transaction: `SICF`
- Service Path: `/sap/opu/odata/sap/Z_ABAPGIT_AGENT_SRV`
- Requires authentication

## Usage

### Start the Agent

```bash
npm start
```

The agent will start on `http://localhost:3000`.

### Claude Integration

In Claude Code, you can call the agent:

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

### Direct API Calls

```bash
# Start pull
curl -X POST http://localhost:3000/api/pull \
  -H "Content-Type: application/json" \
  -d '{"url": "https://github.com/...", "branch": "main"}'

# Check status
curl http://localhost:3000/api/jobs/<job-id>

# Wait for completion
curl http://localhost:3000/api/jobs/<job-id>?wait=true

# Health check
curl http://localhost:3000/api/health
```

## API Reference

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/health` | Health check |
| POST | `/api/pull` | Start pull and activation |
| GET | `/api/jobs/:jobId` | Get job status |
| GET | `/api/jobs` | List all jobs |

### POST /api/pull

Request:
```json
{
  "url": "https://github.com/user/repo",
  "branch": "main"
}
```

Response:
```json
{
  "success": true,
  "jobId": "USER123_20260205_120000",
  "status": "RUNNING",
  "message": "Pull job started",
  "pollUrl": "/api/jobs/USER123_20260205_120000"
}
```

### GET /api/jobs/:jobId

Response:
```json
{
  "jobId": "USER123_20260205_120000",
  "status": "COMPLETED",
  "success": true,
  "activatedCount": 15,
  "failedCount": 2,
  "message": "Activation completed",
  "errorLog": [
    "Syntax error in ZCL_TEST_PROG: Line 42",
    "Object Z_TEST not found"
  ]
}
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

Agent: {"status": "RUNNING", "jobId": "..."}

[After completion]
Agent: {"success": false, "errorLog": ["Syntax error in line 15"]}

You: "There's a syntax error in line 15 of the class. Please fix it."

Claude: [Fixes the error and pushes to git]
```

## Error Handling

| Error Type | Description | Action |
|------------|-------------|--------|
| SYNTAX_ERROR | ABAP syntax error | Claude fixes the code |
| ACTIVATE_FAILED | Activation failed | Check ABAP system |
| TIMEOUT | Job timeout | Retry or increase timeout |
| NETWORK | Connection failed | Check OData service URL |

## Security Considerations

- Use a dedicated technical user (not personal credentials)
- Store passwords in environment variables or `.env` file
- Restrict network access to ABAP system
- Consider using OAuth 2.0 for production

## Troubleshooting

### Agent cannot connect to ABAP

1. Verify OData service URL in config.json
2. Check credentials in config.json
3. Test OData service directly in browser
4. Run `npm start` and check logs

### OData service not accessible

1. Check SICF activation: `/sap/opu/odata/sap/Z_ABAPGIT_AGENT_SRV`
2. Verify Gateway is running
3. Check authorization (role: /UI2/AG)

### Job stays in RUNNING status

1. Check background jobs in SM37
2. Check job log in STMS or ST11
3. Verify abapGit is installed and working
4. Check log entries in Z_ABAPGIT_AGENT_LOG

### Activation fails with many errors

1. Check if objects are in transport request
2. Verify package assignment
3. Run syntax check in SE80 first

## Environment Variables

Instead of config.json, you can use environment variables:

```bash
export ABAP_HOST="your-sap-system.com"
export ABAP_PORT=443
export ABAP_CLIENT="100"
export ABAP_USER="TECH_USER"
export ABAP_PASSWORD="secret"
export ABAP_LANGUAGE="EN"
export AGENT_PORT=3000
export POLL_INTERVAL=5000
```

## License

MIT License
