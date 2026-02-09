CLASS zcl_abgagt_inspect DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.

CLASS zcl_abgagt_inspect IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = 'INSPECT'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    " Inspect command implementation
    rv_result = '{"success":true,"command":"INSPECT","file_count":' && |{ lines( it_files ) }| && '}'.
  ENDMETHOD.
ENDCLASS.
