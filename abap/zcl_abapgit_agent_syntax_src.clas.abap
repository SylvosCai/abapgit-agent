*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax_src DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abapgit_agent_src_agent.

    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_src IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA lv_json_resp TYPE string.
    DATA lt_source_code TYPE string_table.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Find and extract the source_code array from JSON
    FIND REGEX '"source_code"\s*:\s*\[' IN lv_json.
    IF sy-subrc <> 0.
      lv_json_resp = '{"success":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Invalid JSON format"}]}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Extract array content between [ and ]
    DATA(lv_array_start) = sy-fdpos + 14. " position after '"source_code":'
    DATA(lv_array_str) = lv_json+lv_array_start.
    DATA(lv_bracket_pos) = 0.
    FIND FIRST OCCURRENCE OF ']' IN lv_array_str MATCH OFFSET lv_bracket_pos.
    IF lv_bracket_pos = 0.
      lv_json_resp = '{"success":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Malformed JSON array"}]}'.
      lo_entity = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA(lv_array_content) = lv_array_str(lv_bracket_pos).

    " Parse array elements - each element is "line content"
    DATA(lv_pos) = 0.
    WHILE lv_pos < strlen( lv_array_content ).
      " Find start of quoted string
      FIND FIRST OCCURRENCE OF '"' IN lv_array_content+lv_pos MATCH OFFSET DATA(lv_quote_start).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      DATA(lv_content_start) = lv_pos + lv_quote_start + 1.

      " Find end of quoted string
      DATA(lv_escape) = abap_false.
      DATA(lv_search_pos) = lv_content_start.
      WHILE lv_search_pos < strlen( lv_array_content ).
        DATA(lv_char) = lv_array_content+lv_search_pos(1).
        IF lv_char = '\'.
          lv_escape = abap_true.
          lv_search_pos = lv_search_pos + 2.
          CONTINUE.
        ENDIF.
        IF lv_char = '"'.
          IF lv_escape = abap_true.
            lv_escape = abap_false.
            lv_search_pos = lv_search_pos + 1.
            CONTINUE.
          ENDIF.
          " Found closing quote
          DATA(lv_content_end) = lv_search_pos.
          DATA(lv_line) = lv_array_content+lv_content_start(lv_content_end - lv_content_start).
          " Unescape common sequences
          REPLACE ALL OCCURRENCES OF '\n' IN lv_line WITH cl_abap_char_utilities=>newline.
          REPLACE ALL OCCURRENCES OF '\t' IN lv_line WITH cl_abap_char_utilities=>horizontal_tab.
          REPLACE ALL OCCURRENCES OF '\\"' IN lv_line WITH '"'.
          REPLACE ALL OCCURRENCES OF '\\' IN lv_line WITH ''.
          APPEND lv_line TO lt_source_code.
          lv_pos = lv_search_pos + 1.
          EXIT.
        ENDIF.
        lv_search_pos = lv_search_pos + 1.
      ENDWHILE.
    ENDWHILE.

    " Check if we got source code
    IF lines( lt_source_code ) = 0.
      lv_json_resp = '{"success":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Source code is required"}]}'.
      lo_entity = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Build source code for syntax check
    " Add PROGRAM statement if not present
    DATA lt_wrapped_code TYPE string_table.
    DATA lv_first_line TYPE string.
    READ TABLE lt_source_code INDEX 1 INTO lv_first_line.
    IF lv_first_line CS 'PROGRAM' OR lv_first_line CS 'REPORT'.
      " Already has program statement
      lt_wrapped_code = lt_source_code.
    ELSE.
      " Add program statement at the beginning
      APPEND 'PROGRAM z_syntax_check.' TO lt_wrapped_code.
      LOOP AT lt_source_code INTO DATA(lv_line).
        APPEND lv_line TO lt_wrapped_code.
      ENDLOOP.
    ENDIF.

    " Call syntax check agent - returns structure
    DATA ls_result TYPE zcl_abapgit_agent_src_agent=>ty_result.
    ls_result = mo_agent->syntax_check_source( it_source_code = lt_wrapped_code ).

    " Convert success to 'X' or '' for JSON
    DATA lv_success TYPE string.
    IF ls_result-success = abap_true.
      lv_success = 'X'.
    ENDIF.

    " Build response structure (same as /syntax-check)
    DATA: BEGIN OF ls_response,
            success TYPE string,
            error_count TYPE i,
            errors TYPE ty_errors,
          END OF ls_response.

    ls_response-success = lv_success.
    ls_response-error_count = ls_result-error_count.
    IF ls_result-error_count > 0.
      APPEND VALUE #( line = ls_result-line
                      column = ls_result-column
                      text = ls_result-text ) TO ls_response-errors.
    ENDIF.

    " Serialize to JSON using /ui2/cl_json
    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
