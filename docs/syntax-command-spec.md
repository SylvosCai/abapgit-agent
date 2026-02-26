# Syntax Command Specification

## Overview

The `syntax` command performs syntax checking on ABAP source code **without requiring pull/activation**. This enables faster feedback loops during development by checking code directly from git before committing to the ABAP system.

## Problem Statement

Current workflow requires:
1. Push code to git
2. Run `abapgit-agent pull` (which activates objects)
3. Check for syntax errors in pull output
4. If errors, fix and repeat

This is slow and pollutes the ABAP system with potentially broken inactive objects.

## Solution: Working Area Approach

The working area approach writes source code to **inactive includes** temporarily, runs syntax check, and then **cleans up without activation**.

### How ADT Does It

ADT (ABAP Development Tools) uses this approach:
1. Write source to inactive version using `INSERT REPORT ... STATE 'I'`
2. Check syntax using `SEO_CLASS_CHECK_CLASSPOOL` (for classes)
3. Return errors/warnings
4. Delete inactive version (cleanup)

### Sequence Diagram

```
┌─────────┐     ┌──────────────┐     ┌─────────────┐     ┌──────────────┐
│  CLI    │     │ REST Handler │     │ SYNTAX Cmd  │     │ ABAP System  │
└────┬────┘     └──────┬───────┘     └──────┬──────┘     └──────┬───────┘
     │                 │                    │                   │
     │ POST /syntax    │                    │                   │
     │ {source, type}  │                    │                   │
     │────────────────>│                    │                   │
     │                 │ execute(params)    │                   │
     │                 │───────────────────>│                   │
     │                 │                    │                   │
     │                 │                    │ INSERT REPORT     │
     │                 │                    │ (STATE='I')       │
     │                 │                    │──────────────────>│
     │                 │                    │                   │
     │                 │                    │ SEO_CLASS_CHECK   │
     │                 │                    │──────────────────>│
     │                 │                    │<──────────────────│
     │                 │                    │ (errors/warnings) │
     │                 │                    │                   │
     │                 │                    │ DELETE REPORT     │
     │                 │                    │ (STATE='I')       │
     │                 │                    │──────────────────>│
     │                 │                    │                   │
     │                 │<───────────────────│                   │
     │                 │  {results}         │                   │
     │<────────────────│                    │                   │
     │  {results}      │                    │                   │
```

## API Design

### CLI Command

```bash
# Check class source from file
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Check multiple files
abapgit-agent syntax --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap

# Check with source from stdin (for AI integration)
cat src/zcl_my_class.clas.abap | abapgit-agent syntax --type CLAS --name ZCL_MY_CLASS
```

### REST Endpoint

```
POST /sap/bc/z_abapgit_agent/syntax
Content-Type: application/json

{
  "objects": [
    {
      "type": "CLAS",
      "name": "ZCL_MY_CLASS",
      "source": "CLASS zcl_my_class DEFINITION PUBLIC...\nENDCLASS.\nCLASS zcl_my_class IMPLEMENTATION...\nENDCLASS."
    }
  ]
}
```

### Response

```json
{
  "success": true,
  "command": "SYNTAX",
  "results": [
    {
      "object_type": "CLAS",
      "object_name": "ZCL_MY_CLASS",
      "success": true,
      "error_count": 0,
      "errors": [],
      "warnings": []
    }
  ]
}
```

**With errors:**

```json
{
  "success": false,
  "command": "SYNTAX",
  "results": [
    {
      "object_type": "CLAS",
      "object_name": "ZCL_MY_CLASS",
      "success": false,
      "error_count": 1,
      "errors": [
        {
          "line": "15",
          "column": "10",
          "text": "Unknown type \"ZNONEXISTENT\"",
          "include": "ZCL_MY_CLASS==========CM001",
          "method_name": "CONSTRUCTOR"
        }
      ],
      "warnings": []
    }
  ]
}
```

## Implementation Details

### Supported Object Types

| Type | Implementation Approach |
|------|------------------------|
| CLAS | Write to class includes (CP, CU, CO, CI, CM*), use `SEO_CLASS_CHECK_CLASSPOOL` |
| INTF | Write to interface pool, use `SEO_INTERFACE_CHECK_INTFPOOL` |
| PROG | Use `SYNTAX-CHECK` statement directly |
| DDLS | Use `CL_DD_DDL_HANDLER_FACTORY` (existing approach) |

### Class Syntax Check Flow

For classes, the implementation must handle multiple includes:

| Include | Suffix | Content |
|---------|--------|---------|
| Class Pool | CP | Main program container |
| Public Section | CU | PUBLIC SECTION definitions |
| Protected Section | CO | PROTECTED SECTION definitions |
| Private Section | CI | PRIVATE SECTION definitions |
| Local Definitions | CCDEF | Local class definitions |
| Local Implementations | CCIMP | Local class implementations |
| Local Macros | CCMAC | Local macros |
| Test Classes | CCAU | Test class definitions |
| Method Includes | CM001-CMnnn | Individual method implementations |

### Algorithm for Class Check

```abap
METHOD check_class_source.
  DATA: lv_clskey TYPE seoclskey,
        lv_program TYPE program,
        lt_source TYPE string_table.

  lv_clskey-clsname = iv_class_name.

  " 1. Parse source into sections using CL_OO_SOURCE_SCANNER_CLASS
  DATA(lo_scanner) = cl_oo_source_scanner_class=>create_class_scanner(
    clif_name = iv_class_name
    source    = it_source ).
  lo_scanner->scan( ).

  " 2. Write each section to inactive includes
  " Public section
  lv_program = cl_oo_classname_service=>get_pubsec_name( iv_class_name ).
  lt_source = lo_scanner->get_public_section_source( ).
  INSERT REPORT lv_program FROM lt_source STATE 'I'.

  " Protected section
  lv_program = cl_oo_classname_service=>get_prosec_name( iv_class_name ).
  lt_source = lo_scanner->get_protected_section_source( ).
  INSERT REPORT lv_program FROM lt_source STATE 'I'.

  " Private section
  lv_program = cl_oo_classname_service=>get_prisec_name( iv_class_name ).
  lt_source = lo_scanner->get_private_section_source( ).
  INSERT REPORT lv_program FROM lt_source STATE 'I'.

  " Method implementations
  DATA(lt_methods) = lo_scanner->get_method_implementations( ).
  LOOP AT lt_methods INTO DATA(lv_method).
    lt_source = lo_scanner->get_method_impl_source( lv_method ).
    lv_program = cl_oo_classname_service=>get_method_include(
      mtdkey = VALUE #( clsname = iv_class_name cpdname = lv_method ) ).
    INSERT REPORT lv_program FROM lt_source STATE 'I'.
  ENDLOOP.

  " 3. Run syntax check (does NOT activate)
  CALL FUNCTION 'SEO_CLASS_CHECK_CLASSPOOL'
    EXPORTING
      clskey               = lv_clskey
      suppress_error_popup = abap_true
    IMPORTING
      syntaxerror          = rv_has_error
    EXCEPTIONS
      OTHERS               = 1.

  " 4. Collect detailed errors using SLIN or exception messages
  " ...

  " 5. Cleanup - delete inactive versions
  DELETE REPORT cl_oo_classname_service=>get_pubsec_name( iv_class_name ) STATE 'I'.
  DELETE REPORT cl_oo_classname_service=>get_prosec_name( iv_class_name ) STATE 'I'.
  DELETE REPORT cl_oo_classname_service=>get_prisec_name( iv_class_name ) STATE 'I'.
  " ... delete method includes
ENDMETHOD.
```

### Alternative: SYNTAX-CHECK Statement

For simpler cases or when class doesn't exist yet, use `SYNTAX-CHECK` statement:

```abap
METHOD check_with_syntax_statement.
  DATA: lv_msg  TYPE string,
        lv_line TYPE i,
        lv_word TYPE string,
        ls_dir  TYPE trdir.

  " Get TRDIR entry from existing class (for context) or build manually
  ls_dir-uccheck = 'X'.  " Standard ABAP
  " ls_dir-uccheck = '5'.  " ABAP for Cloud

  " Build class skeleton with source
  DATA(lt_program) = build_class_skeleton(
    iv_class_name = iv_class_name
    it_source     = it_source ).

  " Run syntax check
  SYNTAX-CHECK FOR lt_program
    MESSAGE lv_msg
    LINE lv_line
    WORD lv_word
    DIRECTORY ENTRY ls_dir.

  IF sy-subrc <> 0.
    " Error found
    rs_result-success = abap_false.
    rs_result-errors = VALUE #( (
      line = lv_line
      text = lv_msg
      word = lv_word ) ).
  ENDIF.
ENDMETHOD.
```

## ABAP Classes

### New Classes

| Class | Purpose |
|-------|---------|
| `ZCL_ABGAGT_COMMAND_SYNTAX` | Main command implementation |
| `ZCL_ABGAGT_SYNTAX_CHECKER` | Core syntax check logic (working area approach) |
| `ZCL_ABGAGT_RESOURCE_SYNTAX` | REST resource handler |

### Class Diagram

```
┌─────────────────────────────────┐
│  ZCL_ABGAGT_COMMAND_SYNTAX      │
│  (implements ZIF_ABGAGT_COMMAND)│
├─────────────────────────────────┤
│ + execute()                     │
│ - parse_source()                │
│ - build_result()                │
└───────────────┬─────────────────┘
                │ uses
                ▼
┌─────────────────────────────────┐
│  ZCL_ABGAGT_SYNTAX_CHECKER      │
├─────────────────────────────────┤
│ + check_class()                 │
│ + check_interface()             │
│ + check_program()               │
│ - write_inactive_source()       │
│ - run_syntax_check()            │
│ - collect_errors()              │
│ - cleanup_inactive()            │
└─────────────────────────────────┘
```

## Error Handling

### Scenarios

| Scenario | Behavior |
|----------|----------|
| Class doesn't exist | Create temporary skeleton, check, cleanup |
| Class exists (active) | Write to inactive version, check against active |
| Class exists (inactive) | Overwrite inactive, check, cleanup |
| Syntax error found | Return error details with line/column |
| System error | Return error message, ensure cleanup |

### Cleanup Guarantee

**Critical**: Cleanup must happen even if errors occur. Use `TRY...FINALLY`:

```abap
TRY.
    " Write inactive source
    write_inactive_source( ... ).

    " Run check
    run_syntax_check( ... ).

  CATCH cx_root INTO DATA(lx_error).
    " Handle error
    rs_result-errors = VALUE #( ( text = lx_error->get_text( ) ) ).

  CLEANUP.
    " ALWAYS cleanup
    cleanup_inactive( ... ).
ENDTRY.
```

## Limitations

1. **Class must be parseable** - Source must be valid enough to scan into sections
2. **Dependencies must exist** - Referenced types/classes must exist in system
3. **No cross-object check** - Each object checked independently
4. **Temporary DB writes** - Inactive versions are written (and deleted) during check

## Comparison with Existing Commands

| Command | Purpose | Requires Pull? | Writes to DB? |
|---------|---------|----------------|---------------|
| `pull` | Activate from git | N/A | Yes (active) |
| `inspect` | Check existing objects | Yes | No |
| `syntax` (NEW) | Check source directly | No | Yes (inactive, temp) |

## Future Enhancements

1. **Batch checking** - Check multiple objects in single request
2. **Diff-based checking** - Only check changed methods
3. **Cloud-ready check** - Use `uccheck = '5'` for ABAP Cloud syntax
4. **Warning levels** - Configure which warnings to report

## References

- [ABAP Cheat Sheets - Dynamic Programming](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/06_Dynamic_Programming.md)
- [abapGit - OO Class Handling](https://github.com/abapGit/abapGit/blob/main/src/objects/oo/zcl_abapgit_oo_class.clas.abap)
- [SAP Help - SYNTAX-CHECK](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsyntax-check_for_itab.htm)
