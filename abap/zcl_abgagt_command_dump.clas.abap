*"*"use source
*"*"Local Interface:
**********************************************************************
" DUMP command implementation - query short dumps from ST22
CLASS zcl_abgagt_command_dump DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES:
      BEGIN OF ty_dump_params,
        user      TYPE syuname,
        date_from TYPE sydatum,
        date_to   TYPE sydatum,
        time_from TYPE syuzeit,
        time_to   TYPE syuzeit,
        ts_from   TYPE string,    " UTC timestamp filter: YYYYMMDDhhmmss
        ts_to     TYPE string,    " UTC timestamp filter: YYYYMMDDhhmmss
        program   TYPE syrepid,
        error     TYPE s380errid,
        limit     TYPE i,
        detail    TYPE string,
      END OF ty_dump_params.

    TYPES:
      BEGIN OF ty_stack_entry,
        level     TYPE i,
        class     TYPE string,
        method    TYPE string,
        program   TYPE string,
        include   TYPE string,
        line      TYPE i,
      END OF ty_stack_entry.

    TYPES ty_stack_entries TYPE STANDARD TABLE OF ty_stack_entry WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_dump_item,
        id             TYPE string,
        utc_timestamp  TYPE string,    " UTC timestamp: YYYYMMDDhhmmss
        date           TYPE string,    " Server local date (YYYY-MM-DD)
        time           TYPE string,    " Server local time (HH:MM:SS)
        user           TYPE syuname,
        program        TYPE syrepid,
        object         TYPE sobj_name,
        error          TYPE string,
        exception      TYPE string,
        package        TYPE devclass,
        host           TYPE snap_syinst,
        what_happened  TYPE string,
        error_analysis TYPE string,
        source_line    TYPE i,
        source_include TYPE syrepid,
        call_stack     TYPE ty_stack_entries,
      END OF ty_dump_item.

    TYPES ty_dump_items TYPE STANDARD TABLE OF ty_dump_item WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_dump_result,
        success TYPE abap_bool,
        command TYPE string,
        message TYPE string,
        total   TYPE i,
        dumps   TYPE ty_dump_items,
        error   TYPE string,
      END OF ty_dump_result.

  PRIVATE SECTION.
    " Build a composite ID from SNAP_ADT key fields using : as separator
    METHODS build_id
      IMPORTING
        iv_datum TYPE sydatum
        iv_uzeit TYPE syuzeit
        iv_ahost TYPE snap_syinst
        iv_uname TYPE syuname
        iv_mandt TYPE mandt
        iv_modno TYPE sywpid
      RETURNING
        VALUE(rv_id) TYPE string.

    " Parse a composite ID back into a SNAP_KEY structure
    METHODS parse_id
      IMPORTING
        iv_id TYPE string
      RETURNING
        VALUE(rs_key) TYPE snap_key.

    TYPES:
      BEGIN OF ty_snap_row,
        datum         TYPE snap_adt-datum,
        uzeit         TYPE snap_adt-uzeit,
        ahost         TYPE snap_adt-ahost,
        uname         TYPE snap_adt-uname,
        mandt         TYPE snap_adt-mandt,
        modno         TYPE snap_adt-modno,
        timestamp     TYPE snap_adt-timestamp,
        runtime_error TYPE snap_adt-runtime_error,
        mainprog      TYPE snap_adt-mainprog,
        object_name   TYPE snap_adt-object_name,
        exc           TYPE snap_adt-exc,
        devclass      TYPE snap_adt-devclass,
      END OF ty_snap_row.

    TYPES ty_snap_rows TYPE STANDARD TABLE OF ty_snap_row WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_snap_detail,
        datum         TYPE snap_adt-datum,
        uzeit         TYPE snap_adt-uzeit,
        uname         TYPE snap_adt-uname,
        mainprog      TYPE snap_adt-mainprog,
        object_name   TYPE snap_adt-object_name,
        runtime_error TYPE snap_adt-runtime_error,
        exc           TYPE snap_adt-exc,
        devclass      TYPE snap_adt-devclass,
        ahost         TYPE snap_adt-ahost,
        timestamp     TYPE snap_adt-timestamp,
      END OF ty_snap_detail.
ENDCLASS.

CLASS zcl_abgagt_command_dump IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_dump.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA ls_params   TYPE ty_dump_params.
    DATA ls_result   TYPE ty_dump_result.

    ls_result-command = zif_abgagt_command=>gc_dump.

    IF is_param IS SUPPLIED.
      MOVE-CORRESPONDING is_param TO ls_params.
    ENDIF.

    " Enforce limit: default 20, max 100
    IF ls_params-limit <= 0 OR ls_params-limit > 100.
      ls_params-limit = 20.
    ENDIF.
    DATA lv_limit TYPE i.
    lv_limit = ls_params-limit.

    " Detail mode: load full dump text for a specific dump ID
    IF ls_params-detail IS NOT INITIAL.
      DATA ls_key TYPE snap_key.
      ls_key = parse_id( ls_params-detail ).

      DATA lt_keys TYPE snap_keys.
      APPEND ls_key TO lt_keys.

      DATA lt_entries TYPE snap_entries.
      TRY.
          cl_runtime_error=>create(
            EXPORTING p_i_t_snapkeys    = lt_keys
            IMPORTING p_e_t_snapentries = lt_entries ).

          IF lt_entries IS INITIAL.
            ls_result-success = abap_false.
            ls_result-error = 'Short dump not found'.
            rv_result = /ui2/cl_json=>serialize( data = ls_result ).
            RETURN.
          ENDIF.

          " SNAP_ENTRIES is a table of CL_RUNTIME_ERROR object references
          DATA lo_dump TYPE REF TO cl_runtime_error.
          lo_dump = lt_entries[ 1 ].
          DATA ls_item TYPE ty_dump_item.
          ls_item-id = ls_params-detail.

          lo_dump->get_what_happened_text(
            CHANGING p_text = ls_item-what_happened ).
          lo_dump->get_error_analysis_text(
            IMPORTING p_text = ls_item-error_analysis ).

          " Get structured call stack (frame list with line numbers)
          DATA lt_stack TYPE snap_abap_stack.
          lo_dump->get_abap_callstack(
            IMPORTING p_abap_stack = lt_stack ).
          DATA ls_frame TYPE snap_stackinfo.
          LOOP AT lt_stack INTO ls_frame.
            DATA ls_entry TYPE ty_stack_entry.
            ls_entry-level   = ls_frame-index.
            ls_entry-class   = ls_frame-classname.
            ls_entry-method  = ls_frame-event.
            ls_entry-program = ls_frame-program.
            ls_entry-include = ls_frame-include.
            ls_entry-line    = ls_frame-linenr.
            APPEND ls_entry TO ls_item-call_stack.
          ENDLOOP.

          " Get source code at error location with >>>>> marker on error line
          DATA lt_source TYPE sourcetable.
          DATA lv_error_lineno TYPE i.
          DATA lv_error_include TYPE syrepid.
          lo_dump->get_abap_sourceinfo(
            IMPORTING
              p_e_include    = lv_error_include
              p_e_lineno     = lv_error_lineno
              p_e_sourcetext = lt_source ).

          IF lt_source IS NOT INITIAL AND lv_error_lineno > 0.
            ls_item-source_line    = lv_error_lineno.
            ls_item-source_include = lv_error_include.
            DATA lv_source_with_marker TYPE string.
            DATA lv_src_line TYPE string.
            DATA lv_idx      TYPE i.
            LOOP AT lt_source INTO lv_src_line.
              lv_idx = sy-tabix.
              IF lv_idx = lv_error_lineno.
                lv_source_with_marker = lv_source_with_marker
                  && |>>>>> { lv_src_line }|
                  && cl_abap_char_utilities=>newline.
              ELSE.
                lv_source_with_marker = lv_source_with_marker
                  && |      { lv_src_line }|
                  && cl_abap_char_utilities=>newline.
              ENDIF.
            ENDLOOP.
            DATA ls_src_entry TYPE ty_stack_entry.
            ls_src_entry-method = lv_source_with_marker.
            APPEND ls_src_entry TO ls_item-call_stack.
          ELSEIF ls_item-call_stack IS INITIAL.
            " No source info available: fall back to section text
            DATA lv_stack_heading TYPE string.
            DATA lv_stack_text    TYPE string.
            lo_dump->get_section_text(
              EXPORTING section_id      = cl_runtime_error=>c_section_abap_eventstack
              IMPORTING section_heading = lv_stack_heading
                        section_text    = lv_stack_text ).
            IF lv_stack_text IS INITIAL.
              lo_dump->get_section_text(
                EXPORTING section_id      = cl_runtime_error=>c_section_abap_source
                IMPORTING section_heading = lv_stack_heading
                          section_text    = lv_stack_text ).
            ENDIF.
            IF lv_stack_text IS NOT INITIAL.
              DATA ls_text_entry TYPE ty_stack_entry.
              ls_text_entry-method = lv_stack_text.
              APPEND ls_text_entry TO ls_item-call_stack.
            ENDIF.
          ENDIF.

          " Get metadata from SNAP_ADT to populate the remaining fields
          DATA ls_adt TYPE ty_snap_detail.
          SELECT SINGLE datum, uzeit, uname, mainprog, object_name,
                         runtime_error, exc, devclass, ahost, timestamp
            FROM snap_adt
            WHERE mandt  = @sy-mandt
              AND datum  = @ls_key-datum
              AND uzeit  = @ls_key-uzeit
              AND ahost  = @ls_key-ahost
              AND uname  = @ls_key-uname
              AND modno  = @ls_key-modno
            INTO @ls_adt.

          IF sy-subrc = 0.
            ls_item-date          = |{ ls_adt-datum+0(4) }-{ ls_adt-datum+4(2) }-{ ls_adt-datum+6(2) }|.
            ls_item-time          = |{ ls_adt-uzeit+0(2) }:{ ls_adt-uzeit+2(2) }:{ ls_adt-uzeit+4(2) }|.
            ls_item-utc_timestamp = |{ ls_adt-timestamp }|.
            ls_item-user          = ls_adt-uname.
            ls_item-program       = ls_adt-mainprog.
            ls_item-object        = ls_adt-object_name.
            ls_item-error         = ls_adt-runtime_error.
            ls_item-exception     = ls_adt-exc.
            ls_item-package       = ls_adt-devclass.
            ls_item-host          = ls_adt-ahost.
          ENDIF.

          APPEND ls_item TO ls_result-dumps.
          ls_result-success = abap_true.
          ls_result-total   = 1.
          ls_result-message = 'Short dump detail retrieved'.

        CATCH cx_runtime_error_exc_auth.
          ls_result-success = abap_false.
          ls_result-error   = 'Not authorized to read short dumps'.
        CATCH cx_root INTO DATA(lx_error).
          ls_result-success = abap_false.
          ls_result-error   = lx_error->get_text( ).
      ENDTRY.

      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " List mode: query SNAP_ADT summary table
    DATA lt_adt TYPE ty_snap_rows.

    " Copy filter fields to scalar variables for SQL WHERE (7.40: struct fields
    " cannot be used with IS INITIAL in WHERE; scalars work correctly)
    DATA lv_filter_user    TYPE syuname.
    DATA lv_filter_program TYPE syrepid.
    DATA lv_filter_error   TYPE s380errid.
    lv_filter_user    = ls_params-user.
    lv_filter_program = ls_params-program.
    lv_filter_error   = ls_params-error.

    IF ls_params-ts_from IS NOT INITIAL.
      " Timezone-aware mode: filter by UTC TIMESTAMP field
      DATA lv_ts_from TYPE timestamp.
      DATA lv_ts_to   TYPE timestamp.
      lv_ts_from = ls_params-ts_from.
      lv_ts_to   = ls_params-ts_to.

      IF lv_filter_user IS NOT INITIAL
          AND lv_filter_program IS NOT INITIAL
          AND lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND timestamp     BETWEEN @lv_ts_from AND @lv_ts_to
            AND uname         = @lv_filter_user
            AND mainprog      = @lv_filter_program
            AND runtime_error = @lv_filter_error
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_user IS NOT INITIAL
          AND lv_filter_program IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt     = @sy-mandt
            AND timestamp BETWEEN @lv_ts_from AND @lv_ts_to
            AND uname     = @lv_filter_user
            AND mainprog  = @lv_filter_program
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_user IS NOT INITIAL
          AND lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND timestamp     BETWEEN @lv_ts_from AND @lv_ts_to
            AND uname         = @lv_filter_user
            AND runtime_error = @lv_filter_error
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_program IS NOT INITIAL
          AND lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND timestamp     BETWEEN @lv_ts_from AND @lv_ts_to
            AND mainprog      = @lv_filter_program
            AND runtime_error = @lv_filter_error
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_user IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt     = @sy-mandt
            AND timestamp BETWEEN @lv_ts_from AND @lv_ts_to
            AND uname     = @lv_filter_user
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_program IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt     = @sy-mandt
            AND timestamp BETWEEN @lv_ts_from AND @lv_ts_to
            AND mainprog  = @lv_filter_program
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND timestamp     BETWEEN @lv_ts_from AND @lv_ts_to
            AND runtime_error = @lv_filter_error
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSE.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt     = @sy-mandt
            AND timestamp BETWEEN @lv_ts_from AND @lv_ts_to
          ORDER BY timestamp DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ENDIF.
    ELSE.
      " Server-local-time mode: filter by DATUM / UZEIT
      DATA lv_date_from TYPE sydatum.
      DATA lv_date_to   TYPE sydatum.
      lv_date_from = ls_params-date_from.
      lv_date_to   = ls_params-date_to.
      IF lv_date_from IS INITIAL.
        lv_date_from = sy-datum - 7.
      ENDIF.
      IF lv_date_to IS INITIAL.
        lv_date_to = sy-datum.
      ENDIF.

      " Build WHERE dynamically based on which filters are active
      IF lv_filter_user IS NOT INITIAL
          AND lv_filter_program IS NOT INITIAL
          AND lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND datum         BETWEEN @lv_date_from AND @lv_date_to
            AND uname         = @lv_filter_user
            AND mainprog      = @lv_filter_program
            AND runtime_error = @lv_filter_error
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_user IS NOT INITIAL
          AND lv_filter_program IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt    = @sy-mandt
            AND datum    BETWEEN @lv_date_from AND @lv_date_to
            AND uname    = @lv_filter_user
            AND mainprog = @lv_filter_program
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_user IS NOT INITIAL
          AND lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND datum         BETWEEN @lv_date_from AND @lv_date_to
            AND uname         = @lv_filter_user
            AND runtime_error = @lv_filter_error
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_program IS NOT INITIAL
          AND lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND datum         BETWEEN @lv_date_from AND @lv_date_to
            AND mainprog      = @lv_filter_program
            AND runtime_error = @lv_filter_error
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_user IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt = @sy-mandt
            AND datum BETWEEN @lv_date_from AND @lv_date_to
            AND uname = @lv_filter_user
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_program IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt    = @sy-mandt
            AND datum    BETWEEN @lv_date_from AND @lv_date_to
            AND mainprog = @lv_filter_program
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSEIF lv_filter_error IS NOT INITIAL.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt         = @sy-mandt
            AND datum         BETWEEN @lv_date_from AND @lv_date_to
            AND runtime_error = @lv_filter_error
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ELSE.
        SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
               runtime_error, mainprog, object_name, exc, devclass
          FROM snap_adt
          WHERE mandt = @sy-mandt
            AND datum BETWEEN @lv_date_from AND @lv_date_to
          ORDER BY datum DESCENDING, uzeit DESCENDING
          INTO TABLE @lt_adt
          UP TO @lv_limit ROWS.
      ENDIF.

      " Apply time-of-day filter (no SQL equivalent for partial range)
      IF ls_params-time_from IS NOT INITIAL.
        DELETE lt_adt WHERE uzeit < ls_params-time_from.
      ENDIF.
      IF ls_params-time_to IS NOT INITIAL.
        DELETE lt_adt WHERE uzeit > ls_params-time_to.
      ENDIF.
    ENDIF.

    DATA ls_adt2 TYPE ty_snap_row.
    LOOP AT lt_adt INTO ls_adt2.
      DATA ls_row TYPE ty_dump_item.
      ls_row-id        = build_id(
                           iv_datum = ls_adt2-datum
                           iv_uzeit = ls_adt2-uzeit
                           iv_ahost = ls_adt2-ahost
                           iv_uname = ls_adt2-uname
                           iv_mandt = ls_adt2-mandt
                           iv_modno = ls_adt2-modno ).
      ls_row-utc_timestamp = |{ ls_adt2-timestamp }|.
      ls_row-date      = |{ ls_adt2-datum+0(4) }-{ ls_adt2-datum+4(2) }-{ ls_adt2-datum+6(2) }|.
      ls_row-time      = |{ ls_adt2-uzeit+0(2) }:{ ls_adt2-uzeit+2(2) }:{ ls_adt2-uzeit+4(2) }|.
      ls_row-user      = ls_adt2-uname.
      ls_row-program   = ls_adt2-mainprog.
      ls_row-object    = ls_adt2-object_name.
      ls_row-error     = ls_adt2-runtime_error.
      ls_row-exception = ls_adt2-exc.
      ls_row-package   = ls_adt2-devclass.
      ls_row-host      = ls_adt2-ahost.
      APPEND ls_row TO ls_result-dumps.
    ENDLOOP.

    ls_result-success = abap_true.
    ls_result-total   = lines( ls_result-dumps ).
    ls_result-message = |{ ls_result-total } short dump(s) found|.
    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD build_id.
    " Encode the 6-part SNAP_KEY as a colon-delimited string
    " Order matches SNAP_KEY: datum, uzeit, ahost, uname, mandt, modno
    rv_id = |{ iv_datum }:{ iv_uzeit }:{ iv_ahost }:{ iv_uname }:{ iv_mandt }:{ iv_modno }|.
  ENDMETHOD.

  METHOD parse_id.
    DATA lt_parts TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    SPLIT iv_id AT ':' INTO TABLE lt_parts.
    TRY.
        rs_key-datum  = lt_parts[ 1 ].
        rs_key-uzeit  = lt_parts[ 2 ].
        rs_key-ahost  = lt_parts[ 3 ].
        rs_key-uname  = lt_parts[ 4 ].
        rs_key-mandt  = lt_parts[ 5 ].
        rs_key-modno  = lt_parts[ 6 ].
    CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
