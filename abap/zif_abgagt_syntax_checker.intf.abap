"! <p class="shorttext synchronized">Syntax Checker Interface</p>
"! Interface for syntax checking ABAP source code without activation.
"! Implementations: working_area (writes to inactive includes) and syntax_statement (in-memory only).
INTERFACE zif_abgagt_syntax_checker PUBLIC.

  " Check modes
  CONSTANTS:
    gc_mode_working_area     TYPE string VALUE 'WORKING_AREA',
    gc_mode_syntax_statement TYPE string VALUE 'SYNTAX_STATEMENT'.

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

  " Source object structure (input)
  TYPES: BEGIN OF ty_source_object,
           object_type TYPE string,
           object_name TYPE string,
           source      TYPE string,
         END OF ty_source_object.

  TYPES ty_source_objects TYPE STANDARD TABLE OF ty_source_object WITH NON-UNIQUE DEFAULT KEY.

  "! Check class source syntax
  "! @parameter iv_class_name | Class name (e.g., ZCL_MY_CLASS)
  "! @parameter it_source | Source code as string table
  "! @parameter rs_result | Syntax check result
  METHODS check_class
    IMPORTING iv_class_name    TYPE seoclsname
              it_source        TYPE string_table
    RETURNING VALUE(rs_result) TYPE ty_result.

  "! Check interface source syntax
  "! @parameter iv_intf_name | Interface name
  "! @parameter it_source | Source code as string table
  "! @parameter rs_result | Syntax check result
  METHODS check_interface
    IMPORTING iv_intf_name     TYPE seoclsname
              it_source        TYPE string_table
    RETURNING VALUE(rs_result) TYPE ty_result.

  "! Check program source syntax
  "! @parameter iv_program_name | Program name
  "! @parameter it_source | Source code as string table
  "! @parameter iv_uccheck | Unicode check mode (X=Standard, 5=Cloud)
  "! @parameter rs_result | Syntax check result
  METHODS check_program
    IMPORTING iv_program_name  TYPE syrepid
              it_source        TYPE string_table
              iv_uccheck       TYPE trdir-uccheck DEFAULT 'X'
    RETURNING VALUE(rs_result) TYPE ty_result.

  "! Check class source with local classes
  "! @parameter iv_class_name | Class name (e.g., ZCL_MY_CLASS)
  "! @parameter it_source | Main class source code
  "! @parameter it_locals_def | Local class definitions (CCDEF)
  "! @parameter it_locals_imp | Local class implementations (CCIMP)
  "! @parameter rs_result | Syntax check result
  METHODS check_class_with_locals
    IMPORTING iv_class_name    TYPE seoclsname
              it_source        TYPE string_table
              it_locals_def    TYPE string_table OPTIONAL
              it_locals_imp    TYPE string_table OPTIONAL
    RETURNING VALUE(rs_result) TYPE ty_result.

ENDINTERFACE.
