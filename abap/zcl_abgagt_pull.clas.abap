CLASS zcl_abgagt_pull DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.

CLASS zcl_abgagt_pull IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = 'PULL'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    " Pull command implementation
    rv_result = '{"success":true,"command":"PULL","file_count":' && |{ lines( it_files ) }| && '}'.
  ENDMETHOD.
ENDCLASS.
