CLASS zcl_abgagt_pull DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name.
    METHODS execute.
ENDCLASS.

CLASS zcl_abgagt_pull IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'PULL'.
  ENDMETHOD.

  METHOD execute.
    " Pull command implementation
    " For now, return success result
    rv_result = '{"success":true,"command":"PULL","file_count":' && |{ lines( it_files ) }| && '}'.
  ENDMETHOD.
ENDCLASS.
