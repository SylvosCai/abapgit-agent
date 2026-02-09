CLASS zcl_abgagt_unit DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = 'UNIT'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    " Unit command implementation
    rv_result = '{"success":true,"command":"UNIT","file_count":' && |{ lines( it_files ) }| && '}'.
  ENDMETHOD.
ENDCLASS.
