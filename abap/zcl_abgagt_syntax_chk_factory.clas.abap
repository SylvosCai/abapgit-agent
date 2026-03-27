"! <p class="shorttext synchronized">Syntax Checker Factory</p>
"! Factory class to create syntax checker instances based on object type.
"! Uses naming convention: ZCL_ABGAGT_SYNTAX_CHK_{TADIR_TYPE}
CLASS zcl_abgagt_syntax_chk_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    "! Class name prefix for checkers
    CONSTANTS gc_checker_prefix TYPE string VALUE 'ZCL_ABGAGT_SYNTAX_CHK_'.

    "! Create syntax checker instance by object type
    "! @parameter iv_object_type | TADIR object type (CLAS, INTF, PROG, etc.)
    "! @parameter ro_checker | Syntax checker instance (initial if not supported)
    CLASS-METHODS create
      IMPORTING iv_object_type    TYPE string
      RETURNING VALUE(ro_checker) TYPE REF TO zif_abgagt_syntax_checker.

    "! Check if object type is supported
    "! @parameter iv_object_type | TADIR object type
    "! @parameter rv_supported | True if checker exists for this type
    CLASS-METHODS is_supported
      IMPORTING iv_object_type     TYPE string
      RETURNING VALUE(rv_supported) TYPE abap_bool.

ENDCLASS.

CLASS zcl_abgagt_syntax_chk_factory IMPLEMENTATION.

  METHOD create.

    " Normalize object type to uppercase
    DATA(lv_type) = to_upper( iv_object_type ).

    " Build class name from naming convention
    DATA(lv_class_name) = gc_checker_prefix && lv_type.

    " Try to create instance dynamically
    TRY.
        CREATE OBJECT ro_checker TYPE (lv_class_name).
      CATCH cx_sy_create_object_error.
        " Checker class doesn't exist for this object type
        CLEAR ro_checker.
    ENDTRY.
  ENDMETHOD.

  METHOD is_supported.
    DATA(lo_checker) = create( iv_object_type ).
    rv_supported = xsdbool( lo_checker IS BOUND ).
  ENDMETHOD.

ENDCLASS.
