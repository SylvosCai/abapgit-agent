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

# Pull specific files only
abapgit-agent pull --files <file1>,<file2>,...

# Pull from specific branch
abapgit-agent pull --branch <branch>

# Pull from specific URL
abapgit-agent pull --url <git-url>

# Inspect source file(s) for issues (currently runs syntax check)
abapgit-agent inspect --files <file1>,<file2>,...

# Run unit tests
abapgit-agent unit --files <file1>,<file2>,...

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

❌ Failed Objects Log (M entries):
───────────────────────────────────────────────────────────────────────────────
❌ CLAS ZCL_MY_CLASS: Error message text
Exception: Exception details
```

### Key Behaviors

1. **Activated Objects** - Only includes objects that completed successfully (no errors in log)
2. **Failed Objects Log** - Shows all error messages (duplicates allowed for multiple errors on same object)
3. **Error Details** - When errors occur, displays error detail section at the top

### Example with Errors

```
❌ Pull completed with errors!
   Job ID: CAIS20260209041349
   Message: Pull completed with errors

📋 Error Details:
───────────────────────────────────────────────────────────────────────────────
CLAS ZCL_AI_MATH: The statement METHOD ... . is unexpected
Exception: The statement METHOD ... . is unexpected

📋 Pull Log (17 messages):
───────────────────────────────────────────────────────────────────────────────
Icon │ Object                      │ Message
...
❌ │ CLAS ZCL_AI_MATH             │ The statement METHOD ... . is unexpected

📦 Activated Objects (3 unique):
───────────────────────────────────────────────────────────────────────────────
✅ CLAS ZCL_AI_MATH_HANDLER
✅ CLAS ZCL_AI_SAMPLE

❌ Failed Objects Log (3 entries):
───────────────────────────────────────────────────────────────────────────────
❌ CLAS ZCL_AI_MATH: The statement METHOD ... . is unexpected
Exception: The statement METHOD ... . is unexpected
❌ CLAS ZCL_AI_MATH: Import of object ZCL_AI_MATH failed
❌ CLAS ZCL_AI_MATH_HANDLER: Error updating where-used list for CLAS ZCL_AI_MATH_HANDLER
```

### File Filtering

Use `--files` to pull specific files:

```bash
abapgit-agent pull --files zcl_ai_math.clas.abap,zcl_ai_sample.clas.abap
```

Files in subdirectories are supported (e.g., `src/zcl_my_class.clas.abap`).

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
| `activated_count` | number | Number of unique activated objects (excludes objects with errors) |
| `failed_count` | number | Number of failed object entries |
| `started_at` | timestamp | Start time of the operation |
| `finished_at` | timestamp | End time of the operation |
| `log_messages` | array | All log messages (success, error, info, warning) |
| `activated_objects` | array | Unique successfully activated objects |
| `failed_objects` | array | All error log entries (duplicates allowed for multiple errors per object) |

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
