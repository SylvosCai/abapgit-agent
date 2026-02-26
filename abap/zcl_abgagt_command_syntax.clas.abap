"! <p class="shorttext synchronized">Syntax Command - Check source without activation</p>
"! Checks ABAP source code syntax directly without requiring pull/activation.
"! Uses the working area approach (inactive includes) or SYNTAX-CHECK statement.
CLASS zcl_abgagt_command_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abgagt_command.

    " Request parameters
    TYPES: BEGIN OF ty_source_object,
             type   TYPE string,      " CLAS, INTF, PROG
             name   TYPE string,      " Object name
             source TYPE string,      " Source code (newline separated)
           END OF ty_source_object.

    TYPES ty_source_objects TYPE STANDARD TABLE OF ty_source_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_syntax_params,
             objects   TYPE ty_source_objects,
             mode      TYPE string,    " 'working_area' or 'syntax_statement' (default: working_area)
             uccheck   TYPE string,    " 'X' (Standard) or '5' (Cloud) - default: X
           END OF ty_syntax_params.

    " Result types (re-export from checker)
    TYPES ty_error TYPE zcl_abgagt_syntax_checker=>ty_error.
    TYPES ty_errors TYPE zcl_abgagt_syntax_checker=>ty_errors.
    TYPES ty_warning TYPE zcl_abgagt_syntax_checker=>ty_warning.
    TYPES ty_warnings TYPE zcl_abgagt_syntax_checker=>ty_warnings.
    TYPES ty_result TYPE zcl_abgagt_syntax_checker=>ty_result.

    " Response structure
    TYPES: BEGIN OF ty_response,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             results TYPE STANDARD TABLE OF ty_result WITH NON-UNIQUE DEFAULT KEY,
           END OF ty_response.

    " Command constant
    CONSTANTS gc_syntax TYPE string VALUE 'SYNTAX'.

  PRIVATE SECTION.

    " Syntax checker instance
    DATA mo_checker TYPE REF TO zcl_abgagt_syntax_checker.

    "! Parse source string to string table (split by newlines)
    METHODS parse_source
      IMPORTING iv_source       TYPE string
      RETURNING VALUE(rt_lines) TYPE string_table.

    "! Check single object
    METHODS check_object
      IMPORTING is_object       TYPE ty_source_object
                iv_mode         TYPE string
                iv_uccheck      TYPE trdir-uccheck
      RETURNING VALUE(rs_result) TYPE ty_result.

ENDCLASS.


CLASS zcl_abgagt_command_syntax IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = gc_syntax.
  ENDMETHOD.


  METHOD zif_abgagt_command~execute.
    DATA: ls_params   TYPE ty_syntax_params,
          ls_response TYPE ty_response,
          lv_uccheck  TYPE trdir-uccheck.

    " Initialize response
    ls_response-command = gc_syntax.
    ls_response-success = abap_true.

    " Parse parameters
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate input
    IF ls_params-objects IS INITIAL.
      ls_response-success = abap_false.
      ls_response-message = 'No objects provided for syntax check'.
      rv_result = /ui2/cl_json=>serialize( data = ls_response ).
      RETURN.
    ENDIF.

    " Set defaults
    IF ls_params-mode IS INITIAL.
      ls_params-mode = 'working_area'.
    ENDIF.

    IF ls_params-uccheck IS INITIAL OR ls_params-uccheck = 'X'.
      lv_uccheck = 'X'.  " Standard ABAP
    ELSEIF ls_params-uccheck = '5'.
      lv_uccheck = '5'.  " ABAP for Cloud
    ELSE.
      lv_uccheck = 'X'.
    ENDIF.

    " Create checker instance
    CREATE OBJECT mo_checker.

    " Check each object
    LOOP AT ls_params-objects INTO DATA(ls_object).
      DATA(ls_result) = check_object(
        is_object  = ls_object
        iv_mode    = ls_params-mode
        iv_uccheck = lv_uccheck ).

      APPEND ls_result TO ls_response-results.

      " Update overall success
      IF ls_result-success = abap_false.
        ls_response-success = abap_false.
      ENDIF.
    ENDLOOP.

    " Set overall message
    DATA(lv_total) = lines( ls_response-results ).
    DATA(lv_failed) = REDUCE i( INIT count = 0
                                 FOR ls_res IN ls_response-results
                                 WHERE ( success = abap_false )
                                 NEXT count = count + 1 ).

    IF lv_failed = 0.
      ls_response-message = |All { lv_total } object(s) passed syntax check|.
    ELSE.
      ls_response-message = |{ lv_failed } of { lv_total } object(s) have syntax errors|.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.


  METHOD parse_source.
    " Split source string by newlines (handles both LF and CRLF)
    DATA: lv_source TYPE string.

    lv_source = iv_source.

    " Replace CRLF with LF
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN lv_source WITH cl_abap_char_utilities=>newline.

    " Split by newline
    SPLIT lv_source AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
  ENDMETHOD.


  METHOD check_object.
    DATA: lt_source TYPE string_table,
          lv_name   TYPE seoclsname.

    " Parse source string to lines
    lt_source = parse_source( is_object-source ).

    " Convert name to uppercase
    lv_name = to_upper( is_object-name ).

    " Check based on object type
    CASE to_upper( is_object-type ).

      WHEN 'CLAS'.
        IF iv_mode = 'syntax_statement'.
          " Use SYNTAX-CHECK statement (simpler, no DB writes)
          rs_result = mo_checker->check_class_syntax_statement(
            iv_class_name = lv_name
            it_source     = lt_source
            iv_uccheck    = iv_uccheck ).
        ELSE.
          " Use working area approach (writes to inactive includes)
          rs_result = mo_checker->check_class(
            iv_class_name = lv_name
            it_source     = lt_source ).
        ENDIF.

      WHEN 'INTF'.
        rs_result = mo_checker->check_interface(
          iv_intf_name = lv_name
          it_source    = lt_source ).

      WHEN 'PROG'.
        rs_result = mo_checker->check_program(
          iv_program_name = CONV #( lv_name )
          it_source       = lt_source
          iv_uccheck      = iv_uccheck ).

      WHEN OTHERS.
        " Unsupported object type
        rs_result-object_type = is_object-type.
        rs_result-object_name = is_object-name.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = |Unsupported object type: { is_object-type }. Supported: CLAS, INTF, PROG| ) ).
        rs_result-message = |Unsupported object type: { is_object-type }|.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
