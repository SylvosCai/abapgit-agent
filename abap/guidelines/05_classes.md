# ABAP Classes and Objects

**Searchable keywords**: CLASS, DEFINITION, PUBLIC, CREATE OBJECT, NEW, METHOD, INTERFACES, inheritance, FINAL, ABSTRACT

## TOPICS IN THIS FILE
1. Class Definition (PUBLIC) - line 3
2. Constructor - line 20
3. Interfaces - line 35
4. Inline Declaration - line 50
5. Abstract Methods - line 99
6. FINAL Class Limitation - line 117
7. Working with TYPE any - line 135

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

## Abstract Methods

The ABSTRACT keyword must come immediately after the method name:

```abap
" ✅ Correct - ABSTRACT right after method name
METHODS get_name ABSTRACT
  RETURNING VALUE(rv_name) TYPE string.

" ❌ Wrong - ABSTRACT after parameters (syntax error)
METHODS get_name
  RETURNING VALUE(rv_name) TYPE string
  ABSTRACT.
```

## FINAL Class Limitation

A FINAL class cannot have abstract methods. Use plain REDEFINITION instead:

```abap
" ❌ Wrong in FINAL class - syntax error
CLASS zcl_my_class DEFINITION PUBLIC FINAL.
  METHODS parse_request ABSTRACT REDEFINITION.
ENDCLASS.

" ✅ Correct in FINAL class - use REDEFINITION only
CLASS zcl_my_class DEFINITION PUBLIC FINAL.
  METHODS parse_request REDEFINITION.
ENDCLASS.
```

## Working with TYPE any

TYPE any cannot be used with CREATE DATA. When a base class defines parameters with TYPE any, use a typed local variable in the subclass:

```abap
" Base class defines:
CLASS zcl_base DEFINITION PUBLIC ABSTRACT.
  PROTECTED SECTION.
    METHODS parse_request
      IMPORTING iv_json TYPE string
      EXPORTING es_request TYPE any.
ENDCLASS.

" Subclass implementation:
CLASS zcl_subclass DEFINITION PUBLIC FINAL.
  INHERITING FROM zcl_base.
  PROTECTED SECTION.
    METHODS parse_request REDEFINITION.
ENDCLASS.

CLASS zcl_subclass IMPLEMENTATION.
  METHOD parse_request.
    " Use typed local variable
    DATA: ls_request TYPE ty_my_params.

    /ui2/cl_json=>deserialize(
      EXPORTING json = iv_json
      CHANGING data = ls_request ).

    es_request = ls_request.  " Assign typed to generic
  ENDMETHOD.
ENDCLASS.
```

**Key points:**
- Declare a local variable with the concrete type
- Deserialize JSON into the typed local variable
- Assign to the generic TYPE any parameter
