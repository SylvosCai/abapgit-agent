"! <p class="shorttext synchronized">Syntax Checker Factory</p>
"! Factory class to create syntax checker instances based on mode.
CLASS zcl_abgagt_syntax_chk_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    "! Create syntax checker instance
    "! @parameter iv_mode | Check mode (WORKING_AREA or SYNTAX_STATEMENT)
    "! @parameter ro_checker | Syntax checker instance
    CLASS-METHODS create
      IMPORTING iv_mode            TYPE string DEFAULT zif_abgagt_syntax_checker=>gc_mode_working_area
      RETURNING VALUE(ro_checker)  TYPE REF TO zif_abgagt_syntax_checker.

    "! Create working area syntax checker
    "! @parameter ro_checker | Syntax checker instance
    CLASS-METHODS create_working_area
      RETURNING VALUE(ro_checker) TYPE REF TO zif_abgagt_syntax_checker.

    "! Create syntax statement syntax checker
    "! @parameter ro_checker | Syntax checker instance
    CLASS-METHODS create_syntax_statement
      RETURNING VALUE(ro_checker) TYPE REF TO zif_abgagt_syntax_checker.

ENDCLASS.


CLASS zcl_abgagt_syntax_chk_factory IMPLEMENTATION.

  METHOD create.
    CASE iv_mode.
      WHEN zif_abgagt_syntax_checker=>gc_mode_working_area.
        ro_checker = create_working_area( ).
      WHEN zif_abgagt_syntax_checker=>gc_mode_syntax_statement.
        ro_checker = create_syntax_statement( ).
      WHEN OTHERS.
        " Default to working area
        ro_checker = create_working_area( ).
    ENDCASE.
  ENDMETHOD.


  METHOD create_working_area.
    ro_checker = NEW zcl_abgagt_syntax_chk_workarea( ).
  ENDMETHOD.


  METHOD create_syntax_statement.
    ro_checker = NEW zcl_abgagt_syntax_chk_stmt( ).
  ENDMETHOD.

ENDCLASS.
