# ABAP Project Guidelines - Template

This file provides guidelines for **generating ABAP code** in abapGit repositories.

**Use this file as a template**: Copy it to your ABAP repository root when setting up new projects with Claude Code.

## ABAP Syntax Validation

This is an ABAP project. **Do not attempt local syntax validation** - ABAP code can only be validated in an SAP system.

**To validate ABAP code:**

1. After generating code, push changes to git
2. Run `abapgit-agent pull` to pull and activate in ABAP system
3. Review activation results for errors
4. If errors exist, fix them and repeat
5. Use `abapgit-agent syntax-check <type> <name>` for detailed syntax errors

```bash
# After making changes to ABAP files
git add .
git commit -m "Describe changes"
git push

# Validate in ABAP system
abapgit-agent pull

# Check specific object for syntax errors
abapgit-agent syntax-check CLAS ZCL_MY_CLASS

# Run unit tests (planned)
abapgit-agent unit --package ZMY_PACKAGE
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

## Check Local Implementation First

When implementing new features or fixing issues:

1. **Always check local implementations first** - This project already contains working examples of:
   - REST handlers (e.g., `zcl_abapgit_agent_pull`, `zcl_abapgit_agent_syntax`)
   - JSON serialization using `/ui2/cl_json`
   - ABAP object activation patterns
   - Error handling approaches

2. **Before inventing something new:**
   - Search the `abap/` folder for similar implementations
   - Look at existing patterns in the codebase
   - Reuse existing helper classes and methods
   - Follow the established code style

3. **Examples:**
   - Need to create a new REST endpoint? → Study `zcl_abapgit_agent_pull.clas.abap`
   - Need to serialize JSON? → Use `/ui2/cl_json` as shown in existing handlers
   - Need to query TADIR? → Check how other classes do it

**Don't guess patterns** - The codebase has proven implementations. Reuse them.

## ABAP Reference

For ABAP syntax, objects, and abapGit conventions, refer to:

- **abapGit**: https://github.com/abapGit/abapGit
- abapGit documentation explains object types, serialization format, and best practices

## Local Code Reference (Offline Support)

When network issues prevent accessing online resources, you can maintain a local folder with ABAP code examples for reference.

**Setup:**

1. Create a folder to store ABAP code examples (e.g., `~/abap-reference`)
2. Clone abapGit or other ABAP repositories for reference
3. Configure the path in `.abapGitAgent`:
   ```json
   {
     "referenceFolder": "~/abap-reference"
   }
   ```

**Usage:**

- When Claude needs to reference ABAP patterns, it can read files from this folder
- Useful for offline development or when network is unreliable
- Keep commonly used ABAP utilities, class patterns, and examples

**Recommended repositories to clone:**

```bash
# abapGit itself - best reference for ABAP patterns
git clone https://github.com/abapGit/abapGit.git

# ABAP utilities and patterns
git clone https://github.com/abap-tools/abap-utilities.git
```

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
