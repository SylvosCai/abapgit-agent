CLASS zcl_abgagt_bg_decision DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_bg_decision.
ENDCLASS.

CLASS zcl_abgagt_bg_decision IMPLEMENTATION.
  METHOD zif_abgagt_bg_decision~should_run_in_background.
    DATA lo_progressable TYPE REF TO zif_abgagt_progressable.

    " ========================================
    " Priority 1: Resource forces background
    " ========================================
    IF is_config-force_background = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    " ========================================
    " Priority 2: Resource forces synchronous
    " ========================================
    IF is_config-force_sync = abap_true.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " ========================================
    " Priority 3: Command implements progressable interface?
    " ========================================
    TRY.
        lo_progressable ?= io_command.
        " Success = command implements progressable interface
        rv_result = abap_true.
        RETURN.
      CATCH cx_sy_move_cast_error.
        " Command does NOT implement progressable interface
        " Continue to next priority
    ENDTRY.

    " ========================================
    " Priority 4: Explicit request parameter
    " ========================================
    TRY.
        ASSIGN COMPONENT 'BACKGROUND' OF STRUCTURE is_request_data
          TO FIELD-SYMBOL(<lv_bg>).
        IF sy-subrc = 0 AND <lv_bg> = abap_true.
          rv_result = abap_true.
          RETURN.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    " ========================================
    " Priority 5: Payload-based heuristics
    " ========================================
    DATA(lv_command_name) = io_command->get_name( ).
    CASE lv_command_name.
      WHEN 'PULL'.
        " Large file counts
        TRY.
            ASSIGN COMPONENT 'FILES' OF STRUCTURE is_request_data
              TO FIELD-SYMBOL(<lt_files>).
            IF sy-subrc = 0 AND lines( <lt_files> ) > 10.
              rv_result = abap_true.
              RETURN.
            ENDIF.
          CATCH cx_root.
        ENDTRY.

      WHEN 'INSPECT'.
        " Package-wide inspection
        TRY.
            ASSIGN COMPONENT 'PACKAGE' OF STRUCTURE is_request_data
              TO FIELD-SYMBOL(<lv_pkg>).
            IF sy-subrc = 0 AND <lv_pkg> IS NOT INITIAL.
              rv_result = abap_true.
              RETURN.
            ENDIF.
          CATCH cx_root.
        ENDTRY.
    ENDCASE.

    " ========================================
    " Default: Synchronous execution
    " ========================================
    rv_result = abap_false.
  ENDMETHOD.
ENDCLASS.
