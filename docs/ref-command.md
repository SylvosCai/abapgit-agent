# ref Command Requirements

## Overview

Search ABAP reference repositories for syntax patterns and topics. This command searches across multiple ABAP repositories (cheat sheets, code examples, and any other ABAP projects) in the configured reference folder. It works offline without requiring an ABAP connection, making it useful for quick reference lookups during development.

## Command

```bash
# Search for a pattern across all reference repositories
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "CX_SY_"
abapgit-agent ref "FILTER #"

# View specific topic (from cheat sheets)
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic unit-tests

# List available topics
abapgit-agent ref --list-topics

# List all reference repositories
abapgit-agent ref --list-repos

# JSON output for scripting
abapgit-agent ref "VALUE #(" --json
abapgit-agent ref --topic sql --json
```

## Prerequisite

- Reference folder must be configured in `.abapGitAgent` or located at a common path
- ABAP repositories (cheat sheets, code repos) should be cloned into the reference folder

### Setup

Clone the ABAP cheat sheets and any other ABAP repositories to a common location:

```bash
mkdir -p ~/abap-reference
cd ~/abap-reference

# Clone cheat sheets
git clone https://github.com/SAP-samples/abap-cheat-sheets.git

# Clone other ABAP repositories for code search
git clone https://github.com/abapGit/abapGit.git
git clone <your-other-abap-repo>.git
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
2. `~/abap-reference/`
3. `~/Documents/abap-reference/`
4. `~/Documents/code/abap-reference/`

All subdirectories in the reference folder that contain ABAP code (`.abap` files, `.git` folders, or `abap/` directories) are automatically discovered and searched.

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `pattern` | No* | Pattern to search for in reference repositories |
| `--topic` | No* | View specific topic by name (from cheat sheets) |
| `--list-topics` | No | List all available topics from cheat sheets |
| `--list-repos` | No | List all discovered reference repositories |
| `--json` | No | Output results as JSON |

*Either `pattern`, `--topic`, `--list-topics`, or `--list-repos` must be specified.

---

## Tasks

### 1. Detect Reference Folder

Search for cheat sheets in order:
- `.abapGitAgent` → `referenceFolder` + `/abap-cheat-sheets`
- `~/abap-reference/abap-cheat-sheets`
- `~/Documents/abap-reference/abap-cheat-sheets`
- `~/Documents/code/abap-reference/abap-cheat-sheets`

### 2. Discover Reference Repositories

When the ref command runs:

1. Scan the reference folder for all subdirectories
2. Identify repositories that:
   - Contain a `.git` folder (Git repositories)
   - Contain ABAP files (`.abap`, `.clas.abap`, `.intf.abap`, etc.)
   - Have an `abap/` or `src/` subdirectory
3. Index all searchable files (`.md`, `.abap`, `.txt`, `.asddls`)

### 3. Pattern Search (Default Mode)

When a pattern argument is provided:

1. Search across all files in all discovered repositories
2. Search file types: `.md`, `.abap`, `.txt`, `.asddls`
3. Case-insensitive match
4. Return file list with repository grouping
5. Show first 5 unique matches with line numbers

### 4. Topic View Mode (`--topic`)

When `--topic` is specified:

1. Map topic name to cheat sheet file
2. Display first 100 lines of content
3. Show truncation notice if content is longer

### 5. List Topics Mode (`--list-topics`)

Display all available topics with their file mappings (from cheat sheets).

### 6. List Repositories Mode (`--list-repos`)

Display all discovered reference repositories with their types.

---

## Output

### Pattern Search (Human Readable)

```
  🔍 Searching for: 'CORRESPONDING'
  📁 Reference folder: /Users/me/abap-reference
  📚 Repositories (3): abap-cheat-sheets, abap-ai-view, abapGit

  ✅ Found in 23 file(s):

     📦 abap-cheat-sheets/
        • 01_Internal_Tables.md
        • 05_Constructor_Expressions.md
        • ...

     📦 abap-ai-view/
        • abap/zcl_cais_amdp_demo.clas.abap
        • ...

  📄 Preview (first 5 matches):
  ────────────────────────────────────────────────────────────
  📄 abap-cheat-sheets/05_Constructor_Expressions.md (line 1871):
    "Using the primary table key without specifying USING KEY
  → DATA(f1) = FILTER #( fi_tab1 WHERE a >= 4 ).

  📄 abap-ai-view/abap/zcl_cais_amdp_demo.clas.abap (line 45):
    ... using the
  → DATA(result) = CORRESPONDING #( lt_source MAPPING field1 = src1 ).
    - Note that the existing content is deleted.
```

### Pattern Search (JSON)

```bash
abapgit-agent ref "CORRESPONDING" --json
```

```json
{
  "pattern": "CORRESPONDING",
  "referenceFolder": "/Users/me/abap-reference",
  "repositories": [
    "abap-cheat-sheets",
    "abap-ai-view",
    "abapGit"
  ],
  "files": [
    {
      "repo": "abap-cheat-sheets",
      "file": "01_Internal_Tables.md"
    },
    {
      "repo": "abap-cheat-sheets",
      "file": "05_Constructor_Expressions.md"
    },
    {
      "repo": "abap-ai-view",
      "file": "abap/zcl_cais_amdp_demo.clas.abap"
    }
  ],
  "matches": [
    {
      "repo": "abap-cheat-sheets",
      "file": "05_Constructor_Expressions.md",
      "line": 1871,
      "context": "\"Using the primary table key without specifying USING KEY\r\nDATA(f1) = FILTER #( fi_tab1 WHERE a >= 4 ).\r\n"
    },
    {
      "repo": "abap-ai-view",
      "file": "abap/zcl_cais_amdp_demo.clas.abap",
      "line": 45,
      "context": "DATA(result) = CORRESPONDING #( lt_source MAPPING field1 = src1 )."
    }
  ]
}
```

### Topic View

```bash
abapgit-agent ref --topic exceptions
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
abapgit-agent ref --list-topics
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

### List Repositories

```bash
abapgit-agent ref --list-repos
```

```
  📚 ABAP Reference Repositories
  📁 Reference folder: /Users/me/abap-reference

  Found 3 repository(ies):

  Repository                    Type
  ──────────────────────────────────────────────────
  abap-cheat-sheets             Git Repo
  abap-ai-view                  Git Repo
  abapGit                       Git Repo
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Reference folder not found | `❌ Reference folder not found` with setup instructions |
| No repositories found | `❌ No ABAP repositories found in reference folder` with instructions to clone repos |
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
abapgit-agent ref "FILTER #"
```

Shows examples of FILTER operator usage from cheat sheets and any other ABAP repositories.

### View Exception Handling Documentation

```bash
abapgit-agent ref --topic exceptions
```

Displays the complete exception handling cheat sheet including TRY-CATCH and classical exceptions.

### Find JSON Handling Patterns

```bash
abapgit-agent ref "/ui2/cl_json"
```

Shows JSON serialization/deserialization examples from all reference repositories.

### List Available Reference Repositories

```bash
abapgit-agent ref --list-repos
```

Shows all discovered ABAP repositories in the reference folder.

### Use in Scripts

```bash
# Get JSON output for processing
abapgit-agent ref "CORRESPONDING" --json | jq '.files[]'

# Search only in specific repo (using jq filter)
abapgit-agent ref "CORRESPONDING" --json | jq '.matches[] | select(.repo == "abapGit")'
```

---

## Use Cases

Use `ref` when:
- You need to quickly look up ABAP syntax without internet
- You want to find examples of specific patterns across multiple codebases
- You're debugging and need to check exception handling patterns
- You want to learn about unfamiliar ABAP topics
- You need structured JSON output for tooling integration
- You want to search for code patterns in your own ABAP repositories

Unlike other commands, `ref` does **not** require:
- ABAP system connection
- `.abapGitAgent` credentials (only `referenceFolder` is used)
- Network access (works completely offline)

---

## Implementation Notes

### Repository Discovery

The `ref` command automatically discovers ABAP repositories in the reference folder:

1. **Git Repositories** - Directories containing a `.git` folder
2. **ABAP Folders** - Directories containing ABAP indicators:
   - `.abap` files
   - `abap/` or `src/` subdirectories
   - abapGit files (`.clas.abap`, `.intf.abap`, `.tabl.xml`)
   - Object names with `zcl_` or `zif_` prefixes

### Performance

| Mode | Typical Time |
|------|--------------|
| Pattern search | ~100-500ms (depends on number of repos) |
| Topic view | ~40-80ms |
| List topics | ~40-70ms |
| List repos | ~50-100ms |

Performance depends on:
- Number of repositories in reference folder
- Total number of files to search
- Size of files being searched
- Pattern complexity

### Searchable File Types

The following file types are searched when using pattern search:
- `.md` - Markdown documentation
- `.abap` - ABAP source code
- `.txt` - Text files
- `.asddls` - CDS view source files
