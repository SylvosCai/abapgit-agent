"----------------------------------------------------------------------
" Test Doubles - Implementations
"----------------------------------------------------------------------

CLASS lcl_command_stub IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = 'STUB'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    rv_result = '{"success":true}'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_progressable_command_stub IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = 'PROGRESSABLE_STUB'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    rv_result = '{"success":true}'.
  ENDMETHOD.
ENDCLASS.
