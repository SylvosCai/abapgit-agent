# ABAP Project Guidelines - Template

This file provides guidelines for **generating ABAP code** in abapGit repositories.

**Use this file as a template**: Copy it to your ABAP repository root when setting up new projects with Claude Code.

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

# Export custom guidelines to reference folder
abapgit-agent ref export
```

### Decision Flow

```
Start working on unfamiliar ABAP topic/error
        │
        ▼
Run `abapgit-agent ref "<pattern>"` or `ref --topic <topic>`
        │
        ▼
Read the reference material (cheat sheets + guidelines)
        │
        ▼
Understand the correct pattern
        │
        ▼
Write code based on verified patterns
```

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

## Guidelines Index

Detailed guidelines are available in the `guidelines/` folder:

| File | Topic |
|------|-------|
| `guidelines/01_sql.md` | ABAP SQL Best Practices |
| `guidelines/02_exceptions.md` | Exception Handling |
| `guidelines/03_testing.md` | Unit Testing (including CDS) |
| `guidelines/04_cds.md` | CDS Views |
| `guidelines/05_classes.md` | ABAP Classes and Objects |
| `guidelines/06_objects.md` | XML Metadata, Naming Conventions |
| `guidelines/07_json.md` | JSON Handling |

These guidelines are automatically searched by the `ref` command.

## Custom Guidelines

You can add your own guidelines:

1. Create `.md` files in `abap/guidelines/` folder
2. Export to reference folder: `abapgit-agent ref export`
3. The `ref` command will search both cheat sheets and your custom guidelines

## For More Information

- [SAP ABAP Cheat Sheets](https://github.com/SAP-samples/abap-cheat-sheets)
- [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
