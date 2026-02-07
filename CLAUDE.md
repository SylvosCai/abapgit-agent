# ABAP AI Bridge

This project provides the `abapgit-agent` CLI tool for pulling and activating ABAP code from git repositories.

## Commands

```bash
# Pull and activate from current git repo
abapgit-agent pull

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```

## Configuration

Create `.abapGitAgent` in your ABAP repository root:

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

## For ABAP Code Generation

When generating ABAP code for abapGit:

1. Reference https://github.com/abapGit/abapGit for object serialization format
2. Generate code in proper ABAP syntax
3. Push to git, then use `abapgit-agent pull` to validate

## JSON Handling in ABAP

Always use `/ui2/cl_json` for JSON serialization and deserialization:

```abap
" Deserialize JSON to ABAP structure
DATA ls_data TYPE ty_request.
ls_data = /ui2/cl_json=>deserialize( json = lv_json ).

" Serialize ABAP structure to JSON
lv_json = /ui2/cl_json=>serialize( data = ls_response ).
```

Do NOT use manual string operations (FIND, SHIFT, etc.) for JSON parsing.
