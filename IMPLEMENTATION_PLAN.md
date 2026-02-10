# ABAP Git Bridge - Implementation Plan

## Overview
Create an automated workflow where Claude generates ABAP code, pushes to git, and a local agent pulls/activates the code in the ABAP system with proper error handling.

## Architecture

```
┌─────────────┐     ┌─────────────────┐     ┌────────────────┐
│   Claude     │────▶│  Local Agent    │────▶│   ABAP System  │
│  (VS Code)   │     │  (Node.js)      │     │  (abapGit)     │
└─────────────┘     └─────────────────┘     └────────────────┘
                           │                        │
                           │                        ▼
                    ┌─────────────┐         ┌────────────────┐
                    │ Result Cache│◀───────│ Activation Log │
                    └─────────────┘         └────────────────┘
```

## Communication Flow

1. Claude pushes code to git
2. Claude calls agent: `POST /pull { url: "...", branch: "main", username, password }`
3. Agent calls ABAP: `POST /sap/bc/z_abapgit_agent/pull`
4. ABAP executes pull synchronously
5. ABAP returns: `{ success, job_id, message, error_detail }`
6. Agent returns response to Claude
7. If errors, Claude fixes and repeats

## Implemented Features

- Pull and activate repository
- Syntax check via Code Inspector (`/inspect`)
- AUnit test execution (`/unit`)
- Command factory pattern for extensibility

## Future Enhancements

1. **Extended Inspect Command**
   - ATC checks (Assessment Task Controller)
   - Code Inspector checks (SCI)
   - Custom rule sets
   - Quality gates

2. **Package-Level Unit Tests**
   - Run all AUnit tests in a package
   - `--package $MY_PACKAGE` support
   - Aggregate results across all test classes

3. **Source Code Viewer**
   - View ABAP source from CLI
   - Navigate to type definitions
   - Cross-reference lookups
