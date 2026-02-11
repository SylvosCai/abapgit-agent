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

# 4. Check for syntax errors (if pull shows "Error updating where-used list")
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# 5. Repeat until done
```

### Transport Request Support

When working with development packages that require transport requests:

```bash
# Pull with specific transport request
abapgit-agent pull --files abap/zcl_my_class.clas.abap --transport DEVK900001

# Pull all files with transport request
abapgit-agent pull --transport DEVK900001
```

If `--transport` is not specified, abapGit will use its default behavior (create a new transport or use the default transport for the user).

**Important:** Only run `abapgit-agent pull` when ABAP code has actually changed. If you're only modifying JavaScript, JSON, or markdown files, skip the pull step.

**Syntax Check Command:**
When pull shows "Error updating where-used list", use:
```bash
abapgit-agent inspect --files abap/zcl_my_class.clas.abap
```
This will show detailed syntax errors including line numbers and error messages.

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

## ABAP Class Definition - Must Use PUBLIC

**CRITICAL**: Global ABAP classes MUST use `PUBLIC` in the class definition:

```abap
" Correct - global class
CLASS zcl_my_class DEFINITION PUBLIC.
  ...
ENDCLASS.

" Wrong - treated as local class, will fail activation
CLASS zcl_my_class DEFINITION.
  ...
ENDCLASS.
```

**Error symptom**: `Error updating where-used list for CLAS ZCL_MY_CLASS`

**Fix**: Add `PUBLIC` keyword:
```abap
CLASS zcl_my_class DEFINITION PUBLIC.  " <- PUBLIC required
```

## Interface Method Implementation

When implementing interface methods in ABAP classes, use the interface prefix:

```abap
" Interface definition
INTERFACE zif_my_interface PUBLIC.
  METHODS do_something IMPORTING iv_param TYPE string.
ENDINTERFACE.

" Class implementation - use interface prefix
CLASS zcl_my_class DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_my_interface.
ENDCLASS.

CLASS zcl_my_class IMPLEMENTATION.
  METHOD zif_my_interface~do_something.  " <- Use interface prefix
    " Implementation here
  ENDMETHOD.
ENDCLASS.
```

**Wrong**: `METHOD do_something.` - parameter `iv_param` will be unknown
**Correct**: `METHOD zif_my_interface~do_something.` - parameters recognized

## Creating New ABAP Objects - XML Metadata Required

**IMPORTANT**: When creating new ABAP objects in abapGit format, you MUST create XML metadata files alongside the `.clas.abap` and `.intf.abap` files.

### XML Metadata Format for CLAS

For `zcl_abgagt_util.clas.abap`, create `zcl_abgagt_util.clas.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_ABGAGT_UTIL</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

### XML Metadata Format for INTF

For `zif_abgagt_util.intf.abap`, create `zif_abgagt_util.intf.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOINTERF>
    <CLSNAME>ZIF_ABGAGT_UTIL</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOINTERF>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Without XML files**, abapGit will NOT recognize the objects during pull, and they won't be activated in the ABAP system.

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

## Command Factory Pattern

### Command Class Naming
Command implementations should follow the naming convention:
- `ZCL_ABGAGT_COMMAND_<COMMAND>` where `<COMMAND>` is PULL, INSPECT, UNIT, etc.

Example:
```abap
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.
```

### Command Interface
Commands implement `ZIF_ABGAGT_COMMAND` with constants:

```abap
INTERFACE zif_abgagt_command PUBLIC.
  CONSTANTS:
    gc_pull TYPE string VALUE 'PULL',
    gc_inspect TYPE string VALUE 'INSPECT',
    gc_unit TYPE string VALUE 'UNIT'.

  METHODS get_name RETURNING VALUE(rv_name) TYPE string.
  METHODS execute IMPORTING it_files TYPE string_table
                 RETURNING VALUE(rv_result) TYPE string.
ENDINTERFACE.
```

### Factory with Dynamic Object Creation
Use dynamic object creation to avoid syntax errors in one command affecting others:

```abap
CLASS zcl_abgagt_cmd_factory DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_factory) TYPE REF TO zif_abgagt_cmd_factory.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_command_map,
             command TYPE string,
             class_name TYPE string,
           END OF ty_command_map.
    DATA mt_command_map TYPE TABLE OF ty_command_map.
ENDCLASS.

CLASS zcl_abgagt_cmd_factory IMPLEMENTATION.
  METHOD constructor.
    mt_command_map = VALUE #(
      ( command = zif_abgagt_command=>gc_pull     class_name = 'ZCL_ABGAGT_COMMAND_PULL' )
      ( command = zif_abgagt_command=>gc_inspect class_name = 'ZCL_ABGAGT_COMMAND_INSPECT' )
      ( command = zif_abgagt_command=>gc_unit   class_name = 'ZCL_ABGAGT_COMMAND_UNIT' )
    ).
  ENDMETHOD.

  METHOD zif_abgagt_cmd_factory~get_command.
    READ TABLE mt_command_map WITH KEY command = iv_command
      ASSIGNING FIELD-SYMBOL(<ls_map>).
    IF sy-subrc = 0.
      CREATE OBJECT ro_command TYPE (<ls_map>-class_name).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

**Benefits:**
- Factory class activates independently of command classes
- Syntax errors in one command don't affect others
- Add new commands by updating the mapping table

## Best Practices

### Always Return Interface Types, Not Class Types

When defining factory methods or creating objects, return the interface type, not the class type:

```abap
" WRONG - returns concrete class
CLASS-METHODS get_instance
  RETURNING VALUE(ro_factory) TYPE REF TO zcl_abgagt_cmd_factory.

" CORRECT - returns interface type
CLASS-METHODS get_instance
  RETURNING VALUE(ro_factory) TYPE REF TO zif_abgagt_cmd_factory.
```

**Why?** Interface types provide better abstraction, easier testing, and follow the dependency inversion principle. Callers depend on the interface (abstraction), not the concrete class.

## Unit Testing with Local Test Classes

### File Structure

For ABAP local unit tests, use a **separate file** with `.testclasses.abap` extension:

```
abap/
  zcl_my_class.clas.abap          <- Main class (no test code)
  zcl_my_class.clas.testclasses.abap  <- Local test class
  zcl_my_class.clas.xml           <- XML with WITH_UNIT_TESTS = X
```

### Required Elements

1. **Test class file** (`zcl_my_class.clas.testclasses.abap`):
   ```abap
   *"* use this source file for your test class implementation
   *"* local test class
   CLASS ltcl_zcl_my_class DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
     PRIVATE SECTION.
       DATA mo_cut TYPE REF TO zcl_my_class.
       METHODS setup.
       METHODS test_method1 FOR TESTING.
       METHODS test_method2 FOR TESTING.
   ENDCLASS.

   CLASS ltcl_zcl_my_class IMPLEMENTATION.
     METHOD setup.
       CREATE OBJECT mo_cut.
     ENDMETHOD.
     METHOD test_method1.
       " Test code using cl_abap_unit_assert
     ENDMETHOD.
   ENDCLASS.
   ```

2. **XML metadata** (`zcl_my_class.clas.xml`):
   ```xml
   <VSEOCLASS>
     ...
     <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
   </VSEOCLASS>
   ```

### Naming Conventions

- Test class name: `LTCL_ZCL_<CLASSNAME>` (e.g., `LTCL_ZCL_COMMAND_PULL`)
- Test methods: `TEST_<methodname> FOR TESTING` or simply `test_method FOR TESTING`
- Test file: `<classname>.clas.testclasses.abap`

### CRITICAL: Method Name Length Limit

**Test method names MUST NOT exceed 30 characters!**

```abap
" WRONG - 34 characters (syntax error)
METHODS test_execute_with_minimal_params FOR TESTING.

" CORRECT - 18 characters
METHODS test_exec_minimal FOR TESTING.
```

Examples of compliant names:
- `test_get_name` (13 chars)
- `test_exec_minimal` (18 chars)
- `test_exec_files` (16 chars)
- `test_interface` (15 chars)

### Common Assertions

```abap
cl_abap_unit_assert=>assert_equals( act = lv_actual exp = lv_expected msg = 'Error message' ).
cl_abap_unit_assert=>assert_not_initial( act = lv_data msg = 'Should not be initial' ).
cl_abap_unit_assert=>assert_bound( act = lo_ref msg = 'Should be bound' ).
cl_abap_unit_assert=>assert_true( act = lv_bool msg = 'Should be true' ).
```

### What NOT To Do

- ❌ Don't add test methods directly in the main `.clas.abap` file
- ❌ Don't use `CLASS ... DEFINITION ...` without the special comment header
- ❌ Don't reference `<TESTCLASS>` in XML - abapGit auto-detects `.testclasses.abap`
- ❌ Don't use nested local classes inside the main class definition

### Running Tests

In ABAP: SE24 → Test → Execute Unit Tests

Or via abapGit: Pull the files and run tests in the ABAP system.
