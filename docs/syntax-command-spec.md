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

## Solution: SYNTAX-CHECK Statement Approach

The implementation uses the `SYNTAX-CHECK` ABAP statement to check source code in-memory without writing to the database. Source is wrapped with appropriate pool statements (CLASS-POOL, INTERFACE-POOL) and checked directly.

### Sequence Diagram

```
┌─────────┐     ┌──────────────┐     ┌─────────────┐     ┌──────────────┐
│  CLI    │     │ REST Handler │     │ SYNTAX Cmd  │     │ Checker      │
└────┬────┘     └──────┬───────┘     └──────┬──────┘     └──────┬───────┘
     │                 │                    │                   │
     │ POST /syntax    │                    │                   │
     │ {source, type}  │                    │                   │
     │────────────────>│                    │                   │
     │                 │ execute(params)    │                   │
     │                 │───────────────────>│                   │
     │                 │                    │                   │
     │                 │                    │ Factory.create()  │
     │                 │                    │──────────────────>│
     │                 │                    │<──────────────────│
     │                 │                    │                   │
     │                 │                    │ checker.check()   │
     │                 │                    │──────────────────>│
     │                 │                    │  SYNTAX-CHECK     │
     │                 │                    │<──────────────────│
     │                 │                    │ (errors/warnings) │
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

# Check with ABAP Cloud syntax (stricter)
abapgit-agent syntax --files src/zcl_my_class.clas.abap --cloud
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
      "source": "CLASS zcl_my_class DEFINITION PUBLIC...\nENDCLASS.\nCLASS zcl_my_class IMPLEMENTATION...\nENDCLASS.",
      "locals_def": "CLASS lcl_helper DEFINITION...",
      "locals_imp": "CLASS lcl_helper IMPLEMENTATION..."
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
          "line": 15,
          "text": "Field \"LV_UNDEFINED\" is unknown.",
          "word": "LV_UNDEFINED"
        }
      ],
      "warnings": []
    }
  ]
}
```

## Architecture: Object-Type Based Checkers

The syntax checker uses **object-type based implementations** with dynamic instantiation via naming convention.

### Class Diagram

```
┌─────────────────────────────────────────────────────────────┐
│ zif_abgagt_syntax_checker (interface)                       │
├─────────────────────────────────────────────────────────────┤
│ + get_object_type() -> string                               │
│ + check(iv_name, it_source) -> ty_result                    │
└─────────────────────────────────────────────────────────────┘
                           ▲
           ┌───────────────┼───────────────┐
           │               │               │
┌──────────┴───────┐ ┌─────┴─────┐ ┌───────┴──────┐
│zcl_abgagt_syntax │ │zcl_abgagt │ │zcl_abgagt_   │
│_chk_clas         │ │_syntax_   │ │syntax_chk_   │
│                  │ │chk_intf   │ │prog          │
├──────────────────┤ ├───────────┤ ├──────────────┤
│+set_locals_def() │ │           │ │+set_uccheck()│
│+set_locals_imp() │ │           │ │              │
│+clear_locals()   │ │           │ │              │
└──────────────────┘ └───────────┘ └──────────────┘

┌─────────────────────────────────────────────────────────────┐
│ zcl_abgagt_syntax_chk_factory                               │
├─────────────────────────────────────────────────────────────┤
│ gc_checker_prefix = 'ZCL_ABGAGT_SYNTAX_CHK_'                │
│ + create(iv_object_type) -> zif_abgagt_syntax_checker       │
│ + is_supported(iv_object_type) -> abap_bool                 │
└─────────────────────────────────────────────────────────────┘
```

### Naming Convention

Checkers are named using TADIR object type as suffix:

| Object Type | Checker Class |
|-------------|---------------|
| CLAS | `ZCL_ABGAGT_SYNTAX_CHK_CLAS` |
| INTF | `ZCL_ABGAGT_SYNTAX_CHK_INTF` |
| PROG | `ZCL_ABGAGT_SYNTAX_CHK_PROG` |

### Dynamic Instantiation

The factory creates checkers dynamically by constructing the class name:

```abap
METHOD create.
  DATA lv_class_name TYPE string.

  " Build class name from naming convention
  lv_class_name = gc_checker_prefix && to_upper( iv_object_type ).

  " Try to create instance dynamically
  TRY.
      CREATE OBJECT ro_checker TYPE (lv_class_name).
    CATCH cx_sy_create_object_error.
      " Checker class doesn't exist for this object type
      CLEAR ro_checker.
  ENDTRY.
ENDMETHOD.
```

### Adding New Object Types

To add support for a new object type (e.g., FUGR):

1. Create class `ZCL_ABGAGT_SYNTAX_CHK_FUGR` implementing `ZIF_ABGAGT_SYNTAX_CHECKER`
2. Implement `check()` method with appropriate syntax check logic
3. No changes needed in factory or command class

## Implementation Details

### Class Checker (ZCL_ABGAGT_SYNTAX_CHK_CLAS)

For classes, the implementation:
1. Wraps source with `CLASS-POOL.` statement
2. Optionally includes local class definitions (CCDEF) and implementations (CCIMP)
3. Runs `SYNTAX-CHECK` statement with TRDIR context

```abap
METHOD check.
  DATA lt_skeleton TYPE string_table.

  " Build skeleton: CLASS-POOL + locals_def + main source + locals_imp
  APPEND 'CLASS-POOL.' TO lt_skeleton.

  IF mt_locals_def IS NOT INITIAL.
    APPEND LINES OF mt_locals_def TO lt_skeleton.
  ENDIF.

  APPEND LINES OF it_source TO lt_skeleton.

  IF mt_locals_imp IS NOT INITIAL.
    APPEND LINES OF mt_locals_imp TO lt_skeleton.
  ENDIF.

  " Run syntax check
  SYNTAX-CHECK FOR lt_skeleton
    MESSAGE lv_msg
    LINE lv_line
    WORD lv_word
    DIRECTORY ENTRY ls_dir.
ENDMETHOD.
```

### Interface Checker (ZCL_ABGAGT_SYNTAX_CHK_INTF)

For interfaces:
1. Wraps source with `INTERFACE-POOL.` statement
2. Runs `SYNTAX-CHECK` statement

### Program Checker (ZCL_ABGAGT_SYNTAX_CHK_PROG)

For programs:
1. Runs `SYNTAX-CHECK` directly on source
2. Supports `uccheck` parameter for ABAP Cloud compatibility

## ABAP Classes

| Class | Purpose |
|-------|---------|
| `ZIF_ABGAGT_SYNTAX_CHECKER` | Interface for syntax checkers |
| `ZCL_ABGAGT_SYNTAX_CHK_CLAS` | Class syntax checker (supports local classes) |
| `ZCL_ABGAGT_SYNTAX_CHK_INTF` | Interface syntax checker |
| `ZCL_ABGAGT_SYNTAX_CHK_PROG` | Program syntax checker |
| `ZCL_ABGAGT_SYNTAX_CHK_FACTORY` | Factory for creating checkers by object type |
| `ZCL_ABGAGT_COMMAND_SYNTAX` | Command implementation |
| `ZCL_ABGAGT_RESOURCE_SYNTAX` | REST resource handler |

## Supported Object Types

| Type | Checker | Features |
|------|---------|----------|
| CLAS | `ZCL_ABGAGT_SYNTAX_CHK_CLAS` | Supports local classes (CCDEF, CCIMP) |
| INTF | `ZCL_ABGAGT_SYNTAX_CHK_INTF` | Basic interface checking |
| PROG | `ZCL_ABGAGT_SYNTAX_CHK_PROG` | Supports uccheck (Standard/Cloud) |

## Error Handling

| Scenario | Behavior |
|----------|----------|
| Unsupported object type | Factory returns empty, command returns error |
| Syntax error found | Returns error details with line number and message |
| Empty source | Returns error "No source provided" |

## Comparison with Other Commands

| Command | Purpose | Requires Pull? | Writes to DB? |
|---------|---------|----------------|---------------|
| `pull` | Activate from git | N/A | Yes (active) |
| `inspect` | Check existing objects (SCI) | Yes | No |
| `syntax` | Check source directly | No | No |

## Future Enhancements

1. **DDLS checker** - Add `ZCL_ABGAGT_SYNTAX_CHK_DDLS` for CDS view checking
2. **FUGR checker** - Add `ZCL_ABGAGT_SYNTAX_CHK_FUGR` for function groups
3. **Warning levels** - Configure which warnings to report
4. **Cross-object check** - Check dependencies between objects

## References

- [SAP Help - SYNTAX-CHECK](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsyntax-check_for_itab.htm)
- [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
