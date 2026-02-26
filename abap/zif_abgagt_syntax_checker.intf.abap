"! <p class="shorttext synchronized">Syntax Checker Interface</p>
"! Base interface for syntax checking ABAP source code without activation.
"! Implementations are object-type specific (CLAS, INTF, PROG, etc.).
INTERFACE zif_abgagt_syntax_checker PUBLIC.

  " Error structure
  TYPES: BEGIN OF ty_error,
           line        TYPE i,
           column      TYPE i,
           text        TYPE string,
           word        TYPE string,
           include     TYPE string,
           method_name TYPE string,
         END OF ty_error.

  TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

  " Warning structure
  TYPES: BEGIN OF ty_warning,
           line     TYPE i,
           column   TYPE i,
           text     TYPE string,
           severity TYPE string,
           include  TYPE string,
         END OF ty_warning.

  TYPES ty_warnings TYPE STANDARD TABLE OF ty_warning WITH NON-UNIQUE DEFAULT KEY.

  " Result structure
  TYPES: BEGIN OF ty_result,
           object_type  TYPE string,
           object_name  TYPE string,
           success      TYPE abap_bool,
           error_count  TYPE i,
           errors       TYPE ty_errors,
           warnings     TYPE ty_warnings,
           message      TYPE string,
         END OF ty_result.

  "! Get the object type this checker handles
  "! @parameter rv_type | Object type (CLAS, INTF, PROG, etc.)
  METHODS get_object_type
    RETURNING VALUE(rv_type) TYPE string.

  "! Run syntax check on provided source
  "! @parameter iv_name | Object name (e.g., ZCL_MY_CLASS)
  "! @parameter it_source | Source code as string table
  "! @parameter rs_result | Syntax check result
  METHODS check
    IMPORTING iv_name          TYPE clike
              it_source        TYPE string_table
    RETURNING VALUE(rs_result) TYPE ty_result.

ENDINTERFACE.
