"! <p class="shorttext synchronized">Syntax Command - Check source without activation</p>
"! Checks ABAP source code syntax directly without requiring pull/activation.
"! Uses object-type specific checkers via factory.
CLASS zcl_abgagt_command_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abgagt_command.

    " Request parameters
    TYPES: BEGIN OF ty_source_object,
             type        TYPE string,      " CLAS, INTF, PROG
             name        TYPE string,      " Object name
             source      TYPE string,      " Source code (newline separated)
             locals_def  TYPE string,      " Local class definitions (optional, for CLAS)
             locals_imp  TYPE string,      " Local class implementations (optional, for CLAS)
             testclasses TYPE string,      " Test classes (optional, for CLAS)
             fixpt       TYPE string,      " FIXPT flag from XML metadata (optional, for CLAS)
           END OF ty_source_object.

    TYPES ty_source_objects TYPE STANDARD TABLE OF ty_source_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_syntax_params,
             objects   TYPE ty_source_objects,
             uccheck   TYPE string,    " 'X' (Standard) or '5' (Cloud) - default: X
           END OF ty_syntax_params.

    " Result types (re-export from interface)
    TYPES ty_error TYPE zif_abgagt_syntax_checker=>ty_error.
    TYPES ty_errors TYPE zif_abgagt_syntax_checker=>ty_errors.
    TYPES ty_warning TYPE zif_abgagt_syntax_checker=>ty_warning.
    TYPES ty_warnings TYPE zif_abgagt_syntax_checker=>ty_warnings.
    TYPES ty_result TYPE zif_abgagt_syntax_checker=>ty_result.

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

    "! Parse source string to string table (split by newlines)
    METHODS parse_source
      IMPORTING iv_source       TYPE string
      RETURNING VALUE(rt_lines) TYPE string_table.

    "! Check single object using appropriate checker
    METHODS check_object
      IMPORTING is_object        TYPE ty_source_object
                iv_uccheck       TYPE trdir-uccheck
      RETURNING VALUE(rs_result) TYPE ty_result.

ENDCLASS.


CLASS zcl_abgagt_command_syntax IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = gc_syntax.
  ENDMETHOD.


  METHOD zif_abgagt_command~execute.
    DATA: ls_params   TYPE ty_syntax_params,
          ls_response TYPE ty_response,
          ls_object   TYPE ty_source_object,
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

    " Set uccheck default
    IF ls_params-uccheck IS INITIAL OR ls_params-uccheck = 'X'.
      lv_uccheck = 'X'.  " Standard ABAP
    ELSEIF ls_params-uccheck = '5'.
      lv_uccheck = '5'.  " ABAP for Cloud
    ELSE.
      lv_uccheck = 'X'.
    ENDIF.

    " Check each object
    LOOP AT ls_params-objects INTO ls_object.
      DATA(ls_result) = check_object(
        is_object  = ls_object
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
    DATA lv_source TYPE string.

    lv_source = iv_source.

    " Replace CRLF with LF
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN lv_source WITH cl_abap_char_utilities=>newline.

    " Split by newline
    SPLIT lv_source AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
  ENDMETHOD.


  METHOD check_object.
    DATA: lt_source     TYPE string_table,
          lo_checker    TYPE REF TO zif_abgagt_syntax_checker,
          lv_type       TYPE string,
          lv_name       TYPE string.

    " Normalize type and name
    lv_type = to_upper( is_object-type ).
    lv_name = to_upper( is_object-name ).

    " Get checker for this object type
    lo_checker = zcl_abgagt_syntax_chk_factory=>create( lv_type ).

    IF lo_checker IS NOT BOUND.
      " Unsupported object type
      rs_result-object_type = is_object-type.
      rs_result-object_name = is_object-name.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = 1
        text = |Unsupported object type: { is_object-type }. Syntax command only supports CLAS, INTF, PROG. Use 'pull' command for other object types.| ) ).
      rs_result-message = |Unsupported object type: { is_object-type }. Use 'pull' command instead.|.
      RETURN.
    ENDIF.

    " Parse source
    lt_source = parse_source( is_object-source ).

    " Handle type-specific setup
    CASE lv_type.
      WHEN 'CLAS'.
        " Set local classes if provided
        DATA lo_class_checker TYPE REF TO zcl_abgagt_syntax_chk_clas.
        lo_class_checker ?= lo_checker.

        IF is_object-locals_def IS NOT INITIAL.
          lo_class_checker->set_locals_def( parse_source( is_object-locals_def ) ).
        ENDIF.
        IF is_object-locals_imp IS NOT INITIAL.
          lo_class_checker->set_locals_imp( parse_source( is_object-locals_imp ) ).
        ENDIF.
        IF is_object-testclasses IS NOT INITIAL.
          lo_class_checker->set_testclasses( parse_source( is_object-testclasses ) ).
        ENDIF.

      WHEN 'PROG'.
        " Set uccheck for programs
        DATA lo_prog_checker TYPE REF TO zcl_abgagt_syntax_chk_prog.
        lo_prog_checker ?= lo_checker.
        lo_prog_checker->set_uccheck( iv_uccheck ).
    ENDCASE.

    " Set FIXPT for all types if provided (via interface)
    IF is_object-fixpt IS NOT INITIAL.
      lo_checker->set_fixpt( is_object-fixpt ).
    ENDIF.

    " Run check with object name
    rs_result = lo_checker->check(
      iv_name   = lv_name
      it_source = lt_source ).
  ENDMETHOD.

ENDCLASS.
