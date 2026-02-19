# ref Command Requirements

## Overview

Search ABAP cheat sheets for syntax patterns and topics. This command works offline without requiring an ABAP connection, making it useful for quick reference lookups during development.

## Command

```bash
# Search for a pattern
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "CX_SY_"
abapgit-agent ref "FILTER #"

# View specific topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic unit-tests

# List available topics
abapgit-agent ref --list-topics

# JSON output for scripting
abapgit-agent ref "VALUE #(" --json
abapgit-agent ref --topic sql --json
```

## Prerequisite

- ABAP cheat sheets must be cloned locally
- Reference folder must be configured in `.abapGitAgent` or located at a common path

### Setup

Clone the ABAP cheat sheets to a common location:

```bash
mkdir -p ~/abap-reference
cd ~/abap-reference
git clone https://github.com/SAP-samples/abap-cheat-sheets.git
```

Or configure in `.abapGitAgent`:

```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "referenceFolder": "/path/to/abap-reference"
}
```

## Auto-Detection

The command auto-detects the reference folder in this order:

1. `referenceFolder` from `.abapGitAgent` in current working directory
2. `~/abap-reference/abap-cheat-sheets`
3. `~/Documents/abap-reference/abap-cheat-sheets`
4. `~/Documents/code/abap-reference/abap-cheat-sheets`

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `pattern` | No* | Pattern to search for in cheat sheets |
| `--topic` | No* | View specific topic by name |
| `--list-topics` | No | List all available topics |
| `--json` | No | Output results as JSON |

*Either `pattern`, `--topic`, or `--list-topics` must be specified.

---

## Tasks

### 1. Detect Reference Folder

Search for cheat sheets in order:
- `.abapGitAgent` → `referenceFolder` + `/abap-cheat-sheets`
- `~/abap-reference/abap-cheat-sheets`
- `~/Documents/abap-reference/abap-cheat-sheets`
- `~/Documents/code/abap-reference/abap-cheat-sheets`

### 2. Pattern Search (Default Mode)

When a pattern argument is provided:

1. Search all `.md` files in cheat sheets directory
2. Case-insensitive match
3. Return file list and matching contexts
4. Show first 5 unique matches with line numbers

### 3. Topic View Mode (`--topic`)

When `--topic` is specified:

1. Map topic name to cheat sheet file
2. Display first 100 lines of content
3. Show truncation notice if content is longer

### 4. List Topics Mode (`--list-topics`)

Display all available topics with their file mappings.

---

## Output

### Pattern Search (Human Readable)

```
  🔍 Searching for: 'CORRESPONDING'
  📁 Reference folder: /Users/me/abap-reference

  ✅ Found in 23 file(s):
     • 01_Internal_Tables.md
     • 05_Constructor_Expressions.md
     • ...

  📄 Preview (first 5 matches):
  ────────────────────────────────────────────────────────────
  📄 05_Constructor_Expressions.md (line 1871):
    "Using the primary table key without specifying USING KEY
  → DATA(f1) = FILTER #( fi_tab1 WHERE a >= 4 ).

  📄 01_Internal_Tables.md (line 1041):
    ... using the
  → [`CORRESPONDING`](...)
    - Note that the existing content is deleted.
```

### Pattern Search (JSON)

```bash
abapgit-agent ref-search "CORRESPONDING" --json
```

```json
{
  "pattern": "CORRESPONDING",
  "referenceFolder": "/Users/me/abap-reference",
  "files": [
    "01_Internal_Tables.md",
    "05_Constructor_Expressions.md",
    ...
  ],
  "matches": [
    {
      "file": "05_Constructor_Expressions.md",
      "line": 1871,
      "context": "\"Using the primary table key without specifying USING KEY\r\nDATA(f1) = FILTER #( fi_tab1 WHERE a >= 4 ).\r\n"
    },
    ...
  ]
}
```

### Topic View

```bash
abapgit-agent ref-search --topic exceptions
```

```
  📖 27_Exceptions.md
  ────────────────────────────────────────────────────────────

  <a name="top"></a>

  # Exceptions and Runtime Errors

  - [Exceptions and Runtime Errors](#exceptions-and-runtime-errors)
    - [Exceptions](#exceptions)
    - [Exception Classes](#exception-classes)
      - [Exception Categories](#exception-categories)
      ...
```

### List Topics

```bash
abapgit-agent ref-search --list-topics
```

```
  📚 Available ABAP Reference Topics
  📁 Reference folder: /Users/me/abap-reference

  Topic                File
  ────────────────────────────────────────────────────────────
  internal-tables      01_Internal_Tables.md
  structures           02_Structures.md
  sql                  03_ABAP_SQL.md
  oop                  04_ABAP_Object_Orientation.md
  constructors         05_Constructor_Expressions.md
  exceptions           27_Exceptions.md
  unit-tests           14_ABAP_Unit_Tests.md
  cds                  15_CDS_View_Entities.md
  json-xml             21_XML_JSON.md
  ...
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Reference folder not found | `❌ Reference folder not found` with setup instructions |
| Unknown topic | `❌ Unknown topic: <name>` with available topics |
| File not found | `❌ File not found: <file>` |
| No pattern specified | Usage instructions |

---

## Available Topics

| Topic | Cheat Sheet | Description |
|-------|-------------|-------------|
| `internal-tables` | 01_Internal_Tables.md | Internal table operations |
| `structures` | 02_Structures.md | Working with structures |
| `sql` | 03_ABAP_SQL.md | ABAP SQL syntax |
| `oop` | 04_ABAP_Object_Orientation.md | Object-oriented ABAP |
| `constructors` | 05_Constructor_Expressions.md | VALUE, FILTER, CORRESPONDING, etc. |
| `dynamic` | 06_Dynamic_Programming.md | RTTI, RTTC, field symbols |
| `strings` | 07_String_Processing.md | String manipulation |
| `eml` | 08_EML_ABAP_for_RAP.md | Entity Manipulation Language |
| `hierarchies` | 10_ABAP_SQL_Hierarchies.md | SQL hierarchy queries |
| `grouping` | 11_Internal_Tables_Grouping.md | Internal table grouping |
| `amdp` | 12_AMDP.md | ABAP Managed Database Procedures |
| `flow` | 13_Program_Flow_Logic.md | Control structures |
| `unit-tests` | 14_ABAP_Unit_Tests.md | ABAP Unit testing |
| `cds` | 15_CDS_View_Entities.md | CDS View Entities |
| `datatypes` | 16_Data_Types_and_Objects.md | Data types |
| `luw` | 17_SAP_LUW.md | Logical Unit of Work |
| `cloud` | 19_ABAP_for_Cloud_Development.md | ABAP Cloud Development |
| `json-xml` | 21_XML_JSON.md | JSON/XML processing |
| `exceptions` | 27_Exceptions.md | Exception handling |
| `performance` | 32_Performance_Notes.md | Performance optimization |
| `patterns` | 34_OO_Design_Patterns.md | Design patterns |
| `rap` | 36_RAP_Behavior_Definition_Language.md | RAP Behavior Definition |

---

## Examples

### Search for Constructor Expression

```bash
abapgit-agent ref-search "FILTER #"
```

Shows examples of FILTER operator usage from 01_Internal_Tables.md and 05_Constructor_Expressions.md.

### View Exception Handling Documentation

```bash
abapgit-agent ref-search --topic exceptions
```

Displays the complete exception handling cheat sheet including TRY-CATCH and classical exceptions.

### Find JSON Handling Patterns

```bash
abapgit-agent ref-search "/ui2/cl_json"
```

Shows JSON serialization/deserialization examples from 21_XML_JSON.md.

### Use in Scripts

```bash
# Get JSON output for processing
abapgit-agent ref-search "CORRESPONDING" --json | jq '.files[]'
```

---

## Use Cases

Use `ref-search` when:
- You need to quickly look up ABAP syntax without internet
- You want to find examples of specific patterns
- You're debugging and need to check exception handling patterns
- You want to learn about unfamiliar ABAP topics
- You need structured JSON output for tooling integration

Unlike other commands, `ref-search` does **not** require:
- ABAP system connection
- `.abapGitAgent` credentials (only `referenceFolder` is used)
- Network access (works completely offline)

---

## Implementation Notes

### Bash Script Alternative

A bash script version is also available at `bin/abap-ref-search` for environments where Node.js startup time is a concern. The bash version:
- Is slightly faster for simple queries
- Does not support JSON output
- Does not support `--json` flag
- Works on Unix-like systems only (macOS, Linux)

### Performance

| Mode | Typical Time |
|------|--------------|
| Pattern search | ~50-100ms |
| Topic view | ~40-80ms |
| List topics | ~40-70ms |

Performance depends on:
- Number of cheat sheet files (37 files total)
- Size of files being searched
- Pattern complexity
