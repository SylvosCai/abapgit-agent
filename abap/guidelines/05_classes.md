# ABAP Classes and Objects

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
