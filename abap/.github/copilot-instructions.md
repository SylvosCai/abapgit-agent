# ABAP Development with abapGit

You are working on an ABAP project using abapGit for version control.

---

## Critical Rules

### 1. Use `ref` Command for Unfamiliar Topics

**When starting to work on ANY unfamiliar ABAP topic, syntax, or pattern, you MUST use the `ref` command BEFORE writing any code.**

```
❌ WRONG: Start writing code immediately based on assumptions
✅ CORRECT: Run ref command first to look up the correct pattern
```

**Why**: ABAP syntax is strict. Guessing leads to activation errors that waste time.

| Scenario | Example |
|----------|---------|
| Implementing new ABAP feature | "How do I use FILTER operator?" |
| Unfamiliar pattern | "What's the correct VALUE #() syntax?" |
| SQL operations | "How to write a proper SELECT with JOIN?" |
| CDS views | "How to define CDS view with associations?" |
| Getting syntax errors | Check reference before trying approaches |

```bash
# For CDS topics
abapgit-agent ref --topic cds
abapgit-agent ref "CDS view"
abapgit-agent ref "association"
```

```bash
# Search for a pattern
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"

# Browse by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql

# List all topics
abapgit-agent ref --list-topics
```

### 2. Read `.abapGitAgent` for Folder Location

**Before creating ANY ABAP object file, you MUST read `.abapGitAgent` to determine the correct folder.**

```
❌ WRONG: Assume files go in "abap/" folder
✅ CORRECT: Read .abapGitAgent to get the "folder" property value
```

The folder is configured in `.abapGitAgent` (property: `folder`):
- If `folder` is `/src/` → files go in `src/` (e.g., `src/zcl_my_class.clas.abap`)
- If `folder` is `/abap/` → files go in `abap/` (e.g., `abap/zcl_my_class.clas.abap`)

### 3. Create XML Metadata for Each ABAP Object

**Each ABAP object requires an XML metadata file for abapGit to understand how to handle it.**

| Object Type | ABAP File (if folder=/src/) | XML File |
|-------------|------------------------------|----------|
| Class | `src/zcl_*.clas.abap` | `src/zcl_*.clas.xml` |
| Interface | `src/zif_*.intf.abap` | `src/zif_*.intf.xml` |
| Program | `src/z*.prog.abap` | `src/z*.prog.xml` |
| Table | `src/z*.tabl.abap` | `src/z*.tabl.xml` |
| CDS View | `src/zc_*.ddls.asddls` | `src/zc_*.ddls.xml` |

**Use `ref --topic abapgit` for complete XML templates.**

---

## Development Workflow

```
1. Read .abapGitAgent → get folder value
       │
       ▼
2. Research → use ref command for unfamiliar topics
       │
       ▼
3. Write code → place in correct folder (e.g., src/zcl_*.clas.abap)
       │
       ▼
4. Commit and push → git add . && git commit && git push
       │
       ▼
5. Activate → abapgit-agent pull --files src/file.clas.abap
       │
       ▼
6. Verify → Check pull output
   - Objects NOT in "Activated Objects" but in "Failed Objects Log" → Syntax error (check inspect)
   - Objects NOT appearing at all → XML metadata issue
       │
       ▼
7. If needed → Use inspect to check syntax (runs against ABAP system)
```

**IMPORTANT**:
- **ALWAYS push to git BEFORE running pull** - abapGit reads from git
- **Use inspect AFTER pull** to check syntax on objects already in ABAP
- **Check pull output**:
  - In "Failed Objects Log" → Syntax error (use inspect for details)
  - Not appearing at all → XML metadata is wrong

### Commands

```bash
# 1. Pull/activate after pushing to git (abapGit reads from git!)
abapgit-agent pull --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap

# 2. Inspect AFTER pull to check syntax (runs against ABAP system)
abapgit-agent inspect --files src/zcl_class1.clas.abap,src/zif_interface1.intf.abap

# Run unit tests (multiple test classes)
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap

# View object definitions (multiple objects)
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE

# Preview table data (multiple tables/views)
abapgit-agent preview --objects ZTABLE1,ZTABLE2

# Explore table structures
abapgit-agent view --objects ZTABLE --type TABL

# Display package tree
abapgit-agent tree --package \$MY_PACKAGE
```

---

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

AI assistant SHOULD call `view` command when:
- User asks to "check", "look up", or "explore" an unfamiliar object
- Working with a table/structure and you don't know the fields
- Calling a class/interface method and you don't know the parameters

---

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

---

## Error Handling

- Activation fails with "Error updating where-used list" = **syntax error**
- Use `abapgit-agent inspect --files <file>` for detailed error messages

---

## Guidelines Index

Detailed guidelines are available in the `guidelines/` folder:

| File | Topic |
|------|-------|
| `../guidelines/01_sql.md` | ABAP SQL Best Practices |
| `../guidelines/02_exceptions.md` | Exception Handling |
| `../guidelines/03_testing.md` | Unit Testing (including CDS) |
| `../guidelines/04_cds.md` | CDS Views |
| `../guidelines/05_classes.md` | ABAP Classes and Objects |
| `../guidelines/06_objects.md` | Object Naming Conventions |
| `../guidelines/07_json.md` | JSON Handling |
| `../guidelines/08_abapgit.md` | abapGit XML Metadata Templates |

These guidelines are automatically searched by the `ref` command.

---

## Object Naming

| Pattern | Type |
|---------|------|
| `ZCL_*` | Class |
| `ZIF_*` | Interface |
| `Z*` | Other objects |
