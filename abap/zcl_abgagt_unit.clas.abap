CLASS zcl_abgagt_unit DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name REDEFINITION.
    METHODS execute REDEFINITION.
ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'UNIT'.
  ENDMETHOD.

  METHOD execute.
    " Unit command implementation
    rv_result = '{"success":true,"command":"UNIT","file_count":' && |{ lines( it_files ) }| && '}'.
  ENDMETHOD.
ENDCLASS.
