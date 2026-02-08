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

# Syntax check for specific object
abapgit-agent syntax-check <object_type> <object_name>

# Run unit tests
abapgit-agent unit --package <package>
abapgit-agent unit --object <type> <name>

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```

## Pull Output Format

The `pull` command returns detailed information about the pull operation:

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
```

### JSON Response Structure

The REST API returns the following structure:

```json
{
  "success": "X",
  "job_id": "CAIS20260208115649",
  "message": "Pull completed successfully",
  "error_detail": "",
  "activated_count": 14,
  "failed_count": 0,
  "started_at": "2026-02-08T11:56:49.1234567+00:00",
  "finished_at": "2026-02-08T11:56:51.6543210+00:00",
  "log_messages": [
    {
      "type": "S",
      "id": "",
      "number": "000",
      "text": "Object ZCL_MY_CLASS imported",
      "obj_type": "CLAS",
      "obj_name": "ZCL_MY_CLASS",
      "exception": ""
    }
  ],
  "activated_objects": [
    {
      "type": "S",
      "id": "",
      "number": "000",
      "text": "Object ZCL_MY_CLASS imported",
      "obj_type": "CLAS",
      "obj_name": "ZCL_MY_CLASS",
      "exception": ""
    }
  ],
  "failed_objects": []
}
```

### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | string | 'X' if successful, '' if errors |
| `job_id` | string | Unique job ID for the pull operation |
| `message` | string | Human-readable status message |
| `error_detail` | string | Detailed error messages (if any) |
| `activated_count` | number | Number of unique activated objects |
| `failed_count` | number | Number of failed objects |
| `started_at` | timestamp | Start time of the operation |
| `finished_at` | timestamp | End time of the operation |
| `log_messages` | array | All log messages (success, error, info, warning) |
| `activated_objects` | array | Unique successfully activated objects |
| `failed_objects` | array | Failed objects with error details |

### Message Types (TYPE field)

| Type | Icon | Description |
|------|------|-------------|
| `S` | ✅ | Success |
| `E` | ❌ | Error |
| `W` | ⚠️ | Warning |
| `A` | 🛑 | Abort |
| `I` | ℹ️ | Info |

## For ABAP Code Generation

**NOTE**: This file is for developing the CLI tool itself. For guidelines on **generating ABAP code** for abapGit repositories, see `/abap/CLAUDE.md`. Copy that file to your ABAP repository root when setting up new projects.

## Development Workflow

1. Make changes to CLI code (JavaScript) or ABAP backend (abap/ folder)
2. Test locally: `node bin/abapgit-agent pull`
3. Test against real ABAP system
4. Commit and push
5. Deploy ABAP changes via abapGit to your SAP system
