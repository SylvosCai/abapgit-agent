"! <p class="shorttext synchronized">REST Resource for Syntax Command</p>
CLASS zcl_abgagt_resource_syntax DEFINITION PUBLIC FINAL
                            INHERITING FROM zcl_abgagt_resource_base
                            CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_syntax IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zcl_abgagt_command_syntax=>gc_syntax.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Syntax'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_syntax=>ty_syntax_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_syntax=>ty_syntax_params.
    ls_request = is_request.

    " Validate that objects array is provided and not empty
    rv_valid = xsdbool( ls_request-objects IS NOT INITIAL AND lines( ls_request-objects ) > 0 ).

    " Validate each object has required fields
    IF rv_valid = abap_true.
      LOOP AT ls_request-objects INTO DATA(ls_obj).
        " type and name are always required
        IF ls_obj-type IS INITIAL OR ls_obj-name IS INITIAL.
          rv_valid = abap_false.
          EXIT.
        ENDIF.
        " source is required UNLESS testclasses is provided (testclasses-only case)
        IF ls_obj-source IS INITIAL AND ls_obj-testclasses IS INITIAL.
          rv_valid = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_syntax=>ty_syntax_params.
    ls_request = is_request.

    IF ls_request-objects IS INITIAL OR lines( ls_request-objects ) = 0.
      rv_message = 'Objects array is required. Each object needs: type, name, source (or testclasses)'.
      RETURN.
    ENDIF.

    LOOP AT ls_request-objects INTO DATA(ls_obj).
      IF ls_obj-type IS INITIAL.
        rv_message = |Object at index { sy-tabix } missing "type" (CLAS, INTF, PROG)|.
        RETURN.
      ENDIF.
      IF ls_obj-name IS INITIAL.
        rv_message = |Object at index { sy-tabix } missing "name"|.
        RETURN.
      ENDIF.
      IF ls_obj-source IS INITIAL AND ls_obj-testclasses IS INITIAL.
        rv_message = |Object at index { sy-tabix } missing "source" or "testclasses"|.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
