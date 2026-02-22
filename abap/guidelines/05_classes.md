# ABAP Classes and Objects

**Searchable keywords**: CLASS, DEFINITION, PUBLIC, CREATE OBJECT, NEW, METHOD, INTERFACES, inheritance, FINAL, ABSTRACT

## TOPICS IN THIS FILE
1. Class Definition (PUBLIC) - line 3
2. Constructor - line 20
3. Interfaces - line 35
4. Inline Declaration - line 50

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

## Use Interface Type for References

When a class implements an interface, use the **interface type** instead of the class type for references:

```abap
" Interface definition
INTERFACE zif_my_interface PUBLIC.
  METHODS do_something RETURNING VALUE(rv_result) TYPE string.
ENDINTERFACE.

" Class implements interface
CLASS zcl_my_class DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_my_interface.
    CLASS-METHODS get_instance RETURNING VALUE(ro_instance) TYPE REF TO zif_my_interface.
ENDCLASS.

" Caller - use interface type, not class type
CLASS zcl_consumer DEFINITION PUBLIC.
  PRIVATE SECTION.
    DATA mo_instance TYPE REF TO zif_my_interface.  " <- Use interface type
ENDCLASS.

METHOD zcl_consumer->do_something.
  mo_instance = zcl_my_class=>get_instance( ).

  " Call without interface prefix - cleaner code
  DATA(lv_result) = mo_instance->do_something( ).
ENDMETHOD.
```

**Benefits:**
- Cleaner code: `mo_instance->method( )` instead of `mo_instance->zif_my_interface~method( )`
- Flexibility: Can swap implementation class without changing caller (dependency inversion)
- Consistency: All callers use the same interface type

**Key rule:** Always use `REF TO zif_xxx` not `REF TO zcl_xxx` for instance variables and parameters.
