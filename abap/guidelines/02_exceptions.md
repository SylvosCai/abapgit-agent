# Exception Handling - Classical vs Class-Based

**Searchable keywords**: exception, RAISING, TRY, CATCH, cx_static_check, cx_dynamic_check, EXCEPTIONS, sy-subrc, class-based exception, classical exception

## TOPICS IN THIS FILE
1. Quick Identification - line 5
2. Classical Exceptions - line 12
3. Class-Based Exceptions - line 25
4. Method Signatures - line 50
5. Best Practices - line 80

ABAP has two exception handling mechanisms. Using the wrong one causes silent failures.

## Quick Identification

| Method Signature | Exception Type | Handling Pattern |
|-----------------|----------------|------------------|
| `EXCEPTIONS exc = 1` | Classical | `sy-subrc` check |
| `RAISING cx_...` | Class-based | `TRY-CATCH` |

## Classical Exceptions (Old Style)

Methods declared with `EXCEPTIONS` in signature:

```abap
" Method declaration
class-methods describe_by_name
  importing p_name type any
  returning value(p_descr_ref) type ref to cl_abap_typedescr
  exceptions
    type_not_found.  " <- Classical!

" Calling with classical exception
cl_abap_structdescr=>describe_by_name(
  EXPORTING
    p_name = iv_tabname
  RECEIVING
    p_descr_ref = lo_descr
  EXCEPTIONS
    type_not_found = 1
    OTHERS = 2 ).
IF sy-subrc <> 0.
  " Handle error
ENDIF.
```

**Common classical exception methods:**
- `cl_abap_structdescr=>describe_by_name` - `type_not_found`
- `cl_abap_tabledescr=>describe_by_name` - `type_not_found`
- Many RTTI (Run Time Type Information) methods

## Class-Based Exceptions (Modern)

Methods declared with `RAISING`:

```abap
" Method declaration
methods get_ref
  importing p_name type string
  returning value(p_ref) type ref to cl_ci_checkvariant
  raising cx_ci_checkvariant.  " <- Class-based!

" Calling with TRY-CATCH
TRY.
    lo_variant = cl_ci_checkvariant=>get_ref( p_name = lv_variant ).
  CATCH cx_ci_checkvariant INTO DATA(lx_error).
    " Handle error
ENDTRY.
```

## The Mistake to Avoid

**WRONG**: Using TRY-CATCH for classical exceptions
```abap
" This does NOT catch type_not_found!
TRY.
    lo_descr = cl_abap_structdescr=>describe_by_name( iv_name ).
  CATCH cx_root.  " Never reached!
ENDTRY.
```

**CORRECT**: Use proper pattern for each type
```abap
" Classical - use sy-subrc
cl_abap_structdescr=>describe_by_name(
  EXPORTING p_name = iv_name
  RECEIVING p_descr_ref = lo_descr
  EXCEPTIONS type_not_found = 1 ).
IF sy-subrc <> 0.
  " Handle error
ENDIF.

" Class-based - use TRY-CATCH
TRY.
    lo_obj = cl_ci_checkvariant=>get_ref( lv_name ).
  CATCH cx_root INTO DATA(lx_error).
    " Handle error
ENDTRY.
```

## How to Check Which to Use

Use the `view` command to check method signature:

```bash
abapgit-agent view --objects CL_ABAP_STRUCTDESCR
```

Look for:
- `exceptions TYPE_NOT_FOUND` → Classical (use `sy-subrc`)
- `raising CX_SY_RTTI` → Class-based (use `TRY-CATCH`)

Or search the cheat sheets:

```bash
abapgit-agent ref --topic exceptions
```
