# ABAP Development with abapGit

You are working on an ABAP project using abapGit for version control.

## Critical Rule: Use `ref` Command First

**When starting to work on any unfamiliar ABAP topic, syntax, or pattern, you MUST use the `ref` command BEFORE writing any code.**

```
❌ WRONG: Start writing code immediately based on assumptions
✅ CORRECT: Run ref command first to look up the correct pattern
```

### Why This Matters

- ABAP syntax is strict and differs from other languages
- Guessing leads to activation errors that waste time
- The `ref` command provides accurate, verified patterns from SAP's official cheat sheets and custom guidelines

### When to Use `ref` Command

You MUST use `ref` command when:

| Scenario | Example |
|----------|---------|
| Implementing a new ABAP feature | "How do I use FILTER operator?" |
| Using a pattern you're unsure about | "What's the correct VALUE #() syntax?" |
| Working with unfamiliar exception types | "How to properly handle CX_SY_* exceptions?" |
| Any SQL-related operations | "How to write a proper SELECT with JOIN?" |
| Working with tables/internal tables | "What's the modern way to process internal tables?" |
| Getting syntax errors | Check reference before trying different approaches |

### How to Use

```bash
# Search for a specific pattern (searches cheat sheets + custom guidelines)
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"
abapgit-agent ref "VALUE #("

# Browse by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic constructors
abapgit-agent ref --topic internal-tables

# List all available topics
abapgit-agent ref --list-topics
```

## Commands

| Command | Description |
|---------|-------------|
| `abapgit-agent pull` | Pull and activate all ABAP objects |
| `abapgit-agent pull --files <file>` | Pull and activate specific file only |
| `abapgit-agent inspect --files <file>` | Syntax check ABAP source |
| `abapgit-agent unit --files <file>` | Run AUnit tests |
| `abapgit-agent tree --package <package>` | Display package hierarchy |
| `abapgit-agent view --objects <name>` | View object definitions |
| `abapgit-agent ref "<pattern>"` | Search ABAP reference |

## Quick Reference

```bash
# ⚠️  BEFORE writing code on unfamiliar topics - search first!
abapgit-agent ref "PATTERN_TO_SEARCH"
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql

# After editing ABAP files:
git add . && git commit -m "feat: description" && git push
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# If pull fails with syntax error:
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Explore tables/views:
abapgit-agent preview --objects ZTABLE
abapgit-agent view --objects ZTABLE --type TABL
abapgit-agent tree --package $MY_PACKAGE
```

## Common Workflow

1. **Research first**: Use `abapgit-agent ref` to look up unfamiliar patterns
2. Generate/edit ABAP code
3. Push to git: `git add . && git commit && git push`
4. Activate in ABAP: `abapgit-agent pull --files file.clas.abap`
5. Check for errors - fix if needed (use `ref` to find correct pattern)
6. Repeat

## Explore Unknown Objects

**Before working with unfamiliar objects, use `view` command:**

```bash
# Check table structure
abapgit-agent view --objects ZMY_TABLE --type TABL

# Check class definition
abapgit-agent view --objects ZCL_UNKNOWN_CLASS

# Check interface
abapgit-agent view --objects ZIF_UNKNOWN_INTERFACE

# Check data element
abapgit-agent view --objects ZMY_DTEL --type DTEL
```

## When to Use View Command

AI assistant SHOULD call `view` command when:
- User asks to "check", "look up", or "explore" an unfamiliar object
- Working with a table/structure and you don't know the fields
- Calling a class/interface method and you don't know the parameters
- You need to verify an object exists before using it

## Guidelines Index

Detailed guidelines are available in the `abap/guidelines/` folder:

| File | Topic |
|------|-------|
| `../abap/guidelines/01_sql.md` | ABAP SQL Best Practices |
| `../abap/guidelines/02_exceptions.md` | Exception Handling |
| `../abap/guidelines/03_testing.md` | Unit Testing (including CDS) |
| `../abap/guidelines/04_cds.md` | CDS Views |
| `../abap/guidelines/05_classes.md` | ABAP Classes and Objects |
| `../abap/guidelines/06_objects.md` | XML Metadata, Naming Conventions |
| `../abap/guidelines/07_json.md` | JSON Handling |

These guidelines are automatically searched by the `ref` command.

## Key ABAP Rules

1. **Global classes MUST use `PUBLIC`**:
   ```abap
   CLASS zcl_my_class DEFINITION PUBLIC.  " <- REQUIRED
   ```

2. **Use `/ui2/cl_json` for JSON**:
   ```abap
   DATA ls_data TYPE ty_request.
   ls_data = /ui2/cl_json=>deserialize( json = lv_json ).
   lv_json = /ui2/cl_json=>serialize( data = ls_response ).
   ```

3. **Test class name max 30 chars**: `ltcl_util` (not `ltcl_abgagt_util_test`)

4. **Interface method implementation**: Use prefix `zif_interface~method_name`

5. **abapGit files need XML metadata**: `.clas.xml`, `.intf.xml` alongside `.clas.abap`, `.intf.abap`

## Error Handling

- Activation fails with "Error updating where-used list" = **syntax error**
- Use `abapgit-agent inspect --files <file>` for detailed error messages

## Object Naming

| Pattern | Type |
|---------|------|
| `ZCL_*` | Class |
| `ZIF_*` | Interface |
| `Z*` | Other objects |
