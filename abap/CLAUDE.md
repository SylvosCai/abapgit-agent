# ABAP Project Guidelines - Template

This file provides guidelines for **generating ABAP code** in abapGit repositories.

**Use this file as a template**: Copy it to your ABAP repository root when setting up new projects with Claude Code.

## ABAP Syntax Validation

This is an ABAP project. **Do not attempt local syntax validation** - ABAP code can only be validated in an SAP system.

**To validate ABAP code:**

1. After generating code, push changes to git
2. Pull only changed files (fast):
   ```bash
   abapgit-agent pull --files abap/zcl_my_class.clas.abap
   ```
   Or pull all files:
   ```bash
   abapgit-agent pull
   ```
3. Review activation results carefully
4. **"Error updating where-used list" = SYNTAX ERROR** - This is NOT a warning!
5. If Failed Objects > 0, there are syntax errors - fix them before proceeding

## Fast Iteration Workflow

For quick ABAP code changes:

```bash
# 1. Make small change to ABAP file
# 2. Commit and push
git add abap/zcl_my_class.clas.abap
git commit -m "fix: ..."
git push

# 3. Pull only changed file (seconds, not minutes)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# 4. Repeat until done
```

**Important:** Only run `abapgit-agent pull` when ABAP code has actually changed. If you're only modifying JavaScript, JSON, or markdown files, skip the pull step.

## JSON Handling - ALWAYS Use /ui2/cl_json

**CRITICAL**: Always use `/ui2/cl_json` for JSON serialization and deserialization.

**Correct:**
```abap
" Deserialize JSON to ABAP structure
DATA ls_data TYPE ty_request.
ls_data = /ui2/cl_json=>deserialize( json = lv_json ).

" Serialize ABAP structure to JSON
lv_json = /ui2/cl_json=>serialize( data = ls_response ).
```

**Never use**:
- Manual string operations (CONCATENATE, SPLIT, etc.)
- String templates for complex structures
- Direct assignment without /ui2/cl_json

This is enforced by ABAP - manual string operations for JSON parsing will cause type conflicts.

```bash
# After making changes to ABAP files
git add .
git commit -m "Describe changes"
git push

# Validate in ABAP system (single file - fast)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# Or validate all files
abapgit-agent pull
```

## Handling Persistent Syntax Errors

When fixing ABAP syntax errors using the commit-pull-commit loop:

1. **First 2-3 attempts**: Analyze the error message and try to fix based on the error details
2. **If errors persist after 2-3 iterations**:
   - Stop repeating the same fix attempts
   - Search the web for the specific ABAP error message or syntax issue
   - Use keywords like "ABAP", the error code, and relevant context
   - Examples: "ABAP error 'XXXX' CLAS", "ABAP syntax MESSAGE is not a declaration"
3. **After finding information**:
   - Apply the correct fix based on documentation
   - Test again with `abapgit-agent pull`

**Never guess** - ABAP syntax is strict. If you're unsure, search first.

## Understanding abapgit-agent Output

**Critical: Never ignore these messages!**

| Message | Meaning | Action Required |
|---------|---------|-----------------|
| `Error updating where-used list` | **SYNTAX ERROR** - object has errors | Fix the syntax error |
| `Failed Objects (N) > 0` | Objects failed to activate | Fix syntax errors |
| `Success: X` with all objects checked | All good | Proceed |

**Example error flow:**
```
❌ Pull completed with errors!
⚠️  Failed Objects (1):
   ✗ CLAS ZCL_MY_CLASS: Error updating where-used list

Action: The class has syntax errors. Use inspect to get details.
```

## Check Local Implementation First

When implementing new features or fixing issues:

1. **Always check local implementations first** - This project already contains working examples of:
   - REST handlers (e.g., `zcl_abgagt_resource_pull`, `zcl_abgagt_resource_inspect`)
   - JSON serialization using `/ui2/cl_json`
   - ABAP object activation patterns
   - Error handling approaches

2. **Before inventing something new:**
   - Search the `abap/` folder for similar implementations
   - Look at existing patterns in the codebase
   - Reuse existing helper classes and methods
   - Follow the established code style

3. **Examples:**
   - Need to create a new REST endpoint? → Study `zcl_abgagt_resource_pull.clas.abap`
   - Need to serialize JSON? → Use `/ui2/cl_json` as shown in existing handlers
   - Need to query TADIR? → Check how other classes do it

**Don't guess patterns** - The codebase has proven implementations. Reuse them.

**Common mistakes to avoid:**
- Using wrong method parameter names (e.g., `set_string_data( iv_data = x )` should be `set_string_data( x )`)
- Forgetting to use `/ui2/cl_json` for JSON operations
- Using inline DATA declarations incorrectly

## Local Code Reference (Offline Support)

When network issues prevent accessing online resources, you can maintain a local folder with ABAP code examples for reference.

**Setup:**

1. Configure the reference folder path in `.abapGitAgent`:
   ```json
   {
     "referenceFolder": "<path-to-reference-folder>"
   }
   ```

2. Clone ABAP repositories for reference into this folder:
   ```bash
   # abapGit itself - best reference for ABAP patterns
   git clone https://github.com/abapGit/abapGit.git

   # ABAP coding style guides (Clean ABAP, code review)
   git clone https://github.com/SAP/styleguides.git

   # ABAP cheat sheets with code snippets for various topics
   git clone https://github.com/SAP/abap-cheat-sheets.git
   ```

**Usage:**

- When Claude needs to reference ABAP patterns, read files from the folder configured in `.abapGitAgent` (`referenceFolder`)
- Useful for offline development or when network is unreliable
- Keep commonly used ABAP utilities, class patterns, and examples

## ABAP Object Types

Common object types in this project:
- `CLAS` - Classes
- `PROG` - Programs
- `FUGR` - Function Groups
- `INTF` - Interfaces
- `TABL` - Tables
- `DDLS` - Data Definitions

## Naming Conventions

- Use `Z_` or `Y_` prefix for custom objects
- Class names: `ZCL_<NAME>`
- Interface names: `ZIF_<NAME>`
- Programs: `Z<NAME>`
- Package: `$<PROJECT_NAME>`
