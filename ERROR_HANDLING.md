# Error Handling

## Error Types

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
