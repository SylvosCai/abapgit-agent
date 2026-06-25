*"*"use source
*"*"Local Interface:
*"**********************************************************************
"! <p class="shorttext synchronized">ABAP Git Agent - Command HANA</p>
CLASS zcl_abgagt_command_hana DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_object,
             filename TYPE string,
             type     TYPE string,
             content  TYPE string,
           END OF ty_object.
    TYPES ty_objects TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_request,
             action      TYPE string,
             hanapackage TYPE string,
             abappackage TYPE string,
             transport   TYPE trkorr,
             force       TYPE abap_bool,
             dryrun      TYPE abap_bool,
             subpackages TYPE abap_bool,
             subaction   TYPE string,
             schema      TYPE string,
             table       TYPE string,
             filter      TYPE string,
             limit       TYPE i,
             offset      TYPE i,
             where       TYPE string,
             columns     TYPE string_table,
             objectname  TYPE string,
             objectsuffix TYPE string,
             logicalschema TYPE string,
             objects     TYPE ty_objects,
           END OF ty_request.

    TYPES: BEGIN OF ty_result_item,
             filename TYPE string,
             type     TYPE string,
             status   TYPE string,
             message  TYPE string,
             valid    TYPE abap_bool,
           END OF ty_result_item.
    TYPES ty_results TYPE STANDARD TABLE OF ty_result_item WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_response,
             success               TYPE abap_bool,
             transport             TYPE trkorr,
             transport_required    TYPE abap_bool,
             deployed              TYPE i,
             skipped               TYPE i,
             dry_run               TYPE abap_bool,
             error                 TYPE string,
             unregistered_packages TYPE string,
             results               TYPE ty_results,
           END OF ty_response.

    TYPES: BEGIN OF ty_list_item,
             name         TYPE string,
             suffix       TYPE string,
             deploy_state TYPE string,
             hot_status   TYPE char1,
           END OF ty_list_item.
    TYPES ty_list_items TYPE STANDARD TABLE OF ty_list_item WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_list_response,
             success  TYPE abap_bool,
             package  TYPE string,
             error    TYPE string,
             objects  TYPE ty_list_items,
           END OF ty_list_response.

    TYPES: BEGIN OF ty_import_item,
             name    TYPE string,
             suffix  TYPE string,
             content TYPE string,
             status  TYPE string,
             message TYPE string,
           END OF ty_import_item.
    TYPES ty_import_items TYPE STANDARD TABLE OF ty_import_item WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_import_response,
             success TYPE abap_bool,
             package TYPE string,
             error   TYPE string,
             objects TYPE ty_import_items,
           END OF ty_import_response.

    TYPES: BEGIN OF ty_schema_row,
             schema_name TYPE string,
           END OF ty_schema_row.
    TYPES ty_schema_rows TYPE STANDARD TABLE OF ty_schema_row WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_table_row,
             table_name      TYPE string,
             is_column_table TYPE string,
           END OF ty_table_row.
    TYPES ty_table_rows TYPE STANDARD TABLE OF ty_table_row WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_field_row,
             column_name    TYPE string,
             data_type_name TYPE string,
             length         TYPE i,
             scale          TYPE i,
             is_nullable    TYPE string,
             position       TYPE i,
           END OF ty_field_row.
    TYPES ty_field_rows TYPE STANDARD TABLE OF ty_field_row WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_schema_list_response,
             success   TYPE abap_bool,
             subaction TYPE string,
             error     TYPE string,
             rows      TYPE ty_schema_rows,
           END OF ty_schema_list_response.

    TYPES: BEGIN OF ty_schema_tables_response,
             success   TYPE abap_bool,
             subaction TYPE string,
             error     TYPE string,
             rows      TYPE ty_table_rows,
           END OF ty_schema_tables_response.

    TYPES: BEGIN OF ty_schema_fields_response,
             success   TYPE abap_bool,
             subaction TYPE string,
             error     TYPE string,
             rows      TYPE ty_field_rows,
           END OF ty_schema_fields_response.

    TYPES: BEGIN OF ty_preview_column,
             name   TYPE string,
             type   TYPE string,
             length TYPE i,
           END OF ty_preview_column.
    TYPES ty_preview_columns TYPE STANDARD TABLE OF ty_preview_column WITH NON-UNIQUE DEFAULT KEY.

    TYPES ty_preview_cells TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.
    TYPES: BEGIN OF ty_preview_row,
             cells TYPE ty_preview_cells,
           END OF ty_preview_row.
    TYPES ty_preview_rows TYPE STANDARD TABLE OF ty_preview_row WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_preview_response,
             success   TYPE abap_bool,
             view      TYPE string,
             columns   TYPE ty_preview_columns,
             rows      TYPE ty_preview_rows,
             row_count TYPE i,
             limit     TYPE i,
             offset    TYPE i,
             error     TYPE string,
             message   TYPE string,
           END OF ty_preview_response.

    TYPES: BEGIN OF ty_table_preview_response,
             success   TYPE abap_bool,
             table     TYPE string,
             logical   TYPE string,
             resolved  TYPE string,
             columns   TYPE ty_preview_columns,
             rows      TYPE ty_preview_rows,
             row_count TYPE i,
             limit     TYPE i,
             offset    TYPE i,
             error     TYPE string,
             message   TYPE string,
           END OF ty_table_preview_response.

  PRIVATE SECTION.
    METHODS deploy
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS deploy_dry_run
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS check
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS list
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS import
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS drop
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS preview
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS table_preview
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS schema
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS schema_list
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS schema_tables
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS schema_fields
      IMPORTING is_req           TYPE ty_request
      RETURNING VALUE(rv_result) TYPE string.

    METHODS resolve_devclass
      IMPORTING is_req                TYPE ty_request
      EXPORTING ev_devclass           TYPE devclass
                ev_transport_required TYPE abap_bool
                ev_hota_key           TYPE string
      RETURNING VALUE(rv_success)     TYPE abap_bool.

ENDCLASS.

CLASS zcl_abgagt_command_hana IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_hana.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA ls_req TYPE ty_request.

    IF is_param IS SUPPLIED.
      ls_req = CORRESPONDING #( is_param ).
    ENDIF.

    CASE ls_req-action.
      WHEN 'check'.
        rv_result = check( ls_req ).
      WHEN 'deploy'.
        IF ls_req-dryrun = abap_true.
          rv_result = deploy_dry_run( ls_req ).
        ELSE.
          rv_result = deploy( ls_req ).
        ENDIF.
      WHEN 'list'.
        rv_result = list( ls_req ).
      WHEN 'import'.
        rv_result = import( ls_req ).
      WHEN 'drop'.
        rv_result = drop( ls_req ).
      WHEN 'preview'.
        rv_result = preview( ls_req ).
      WHEN 'tablePreview'.
        rv_result = table_preview( ls_req ).
      WHEN 'schema'.
        rv_result = schema( ls_req ).
      WHEN OTHERS.
        rv_result = '{"success":false,"error":"unknown_action"}'.
    ENDCASE.
  ENDMETHOD.

  METHOD resolve_devclass.
    DATA lv_korrflag TYPE korrflag.

    ev_hota_key = to_upper( replace( val  = is_req-hanapackage
                                     sub  = '.'
                                     with = '/'
                                     occ  = 0 ) ).

    IF is_req-abappackage IS NOT INITIAL.
      ev_devclass = is_req-abappackage.
    ELSE.
      SELECT SINGLE devclass
        FROM tadir
        WHERE pgmid    = 'R3TR'
          AND object   = 'HOTA'
          AND obj_name = @ev_hota_key
        INTO @ev_devclass.

      IF sy-subrc <> 0.
        rv_success = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    rv_success = abap_true.

    IF ev_devclass IS NOT INITIAL AND ev_devclass(1) <> '$'.
      SELECT SINGLE korrflag FROM tdevc
        WHERE devclass = @ev_devclass
        INTO @lv_korrflag.
      IF sy-subrc = 0 AND lv_korrflag = abap_true.
        ev_transport_required = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check.
    DATA ls_resp TYPE ty_response.
    DATA lv_devclass TYPE devclass.
    DATA lv_hota_key TYPE string.

    DATA(lv_ok) = resolve_devclass(
      EXPORTING is_req                = is_req
      IMPORTING ev_devclass           = lv_devclass
                ev_transport_required = ls_resp-transport_required
                ev_hota_key           = lv_hota_key ).

    IF lv_ok = abap_false.
      ls_resp-success               = abap_false.
      ls_resp-error                 = 'deploy_failed'.
      ls_resp-unregistered_packages = lv_hota_key.
    ELSE.
      ls_resp-success = abap_true.
    ENDIF.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD deploy_dry_run.
    DATA ls_resp TYPE ty_response.

    ls_resp-success = abap_true.
    ls_resp-dry_run = abap_true.

    LOOP AT is_req-objects INTO DATA(ls_obj).
      APPEND VALUE ty_result_item(
        filename = ls_obj-filename
        type     = ls_obj-type
        valid    = abap_true
        status   = 'would_deploy'
      ) TO ls_resp-results.
    ENDLOOP.

    ls_resp-deployed = lines( is_req-objects ).

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD deploy.
    DATA ls_resp      TYPE ty_response.
    DATA lv_transport TYPE trkorr.
    DATA ls_hot       TYPE cts_hot_object.
    DATA lv_devclass  TYPE devclass.
    DATA lv_hota_key  TYPE string.
    DATA lv_transport_required TYPE abap_bool.
    DATA lo_hot_pkg   TYPE REF TO cl_cts_hot_package.

    lv_transport = is_req-transport.

    DATA(lv_ok) = resolve_devclass(
      EXPORTING is_req                = is_req
      IMPORTING ev_devclass           = lv_devclass
                ev_transport_required = lv_transport_required
                ev_hota_key           = lv_hota_key ).

    IF lv_ok = abap_false.
      ls_resp-success               = abap_false.
      ls_resp-error                 = 'deploy_failed'.
      ls_resp-unregistered_packages = lv_hota_key.
      /ui2/cl_json=>serialize(
        EXPORTING
          data        = ls_resp
          pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
          r_json      = rv_result ).
      RETURN.
    ENDIF.

    IF lv_transport_required = abap_true AND lv_transport IS INITIAL.
      DATA(lv_tr_text) = |HANA deploy { is_req-hanapackage }|.
      DATA lv_retcode TYPE char3.
      CALL FUNCTION 'CTS_API_CREATE_CHANGE_REQUEST'
        EXPORTING
          description = CONV text60( lv_tr_text )
          category    = 'K'
          client      = sy-mandt
          owner       = sy-uname
        IMPORTING
          request     = lv_transport
          retcode     = lv_retcode
        EXCEPTIONS
          OTHERS      = 1.
    ENDIF.

    DATA(lo_hta_factory) = cl_cts_hta_api_factory=>create_instance( ).
    DATA(lo_db_access)   = NEW cl_cts_hot_db_access( ).

    " Register package in CTS_HOT_PACKAGE so HTA list API can discover it
    TRY.
        lo_hot_pkg = cl_cts_hot_package=>create_instance( iv_hana_package_id = is_req-hanapackage ).
        DATA ls_hot_pkg TYPE cts_hot_package.
        ls_hot_pkg-abap_hana_package_id = lo_hot_pkg->abap_hana_package_id.
        ls_hot_pkg-abap_status          = 'A'.
        ls_hot_pkg-hot_status           = 'A'.
        ls_hot_pkg-hana_package_id      = is_req-hanapackage.
        lo_db_access->if_cts_hot_db_access~modify_cts_hot_package( ls_hot_pkg ).
      CATCH cx_hana_object_transport ##NO_HANDLER.
        " Package registration is best-effort; if it fails (e.g. hashing error
        " for an over-long name), the subsequent object MODIFY will surface a
        " more actionable error. No-op here is intentional.
    ENDTRY.

    " Pre-fetch HOT status for all objects (avoid DB calls inside loop)
    DATA lt_hot_status TYPE STANDARD TABLE OF cts_hot_object WITH DEFAULT KEY.
    IF is_req-force = abap_false AND is_req-objects IS NOT INITIAL.
      SELECT hana_package_id, hana_object_name, hana_object_suffix, hot_status
        FROM cts_hot_object
        WHERE hana_package_id = @is_req-hanapackage
        INTO TABLE @lt_hot_status.
    ENDIF.

    " DB writes inside the loop are intentional: one CTS_HOT_OBJECT MODIFY +
    " one HTA deploy() per object is the natural unit (bulk-write is not an
    " option because the HTA API requires per-object roundtrips). Suppress
    " CL_CI_TEST_DB_OPS_IN_LOOPS/DBWRITE on this LOOP and its callees.
    LOOP AT is_req-objects INTO DATA(ls_obj). "#EC CI_IMUD_NESTED "#EC CI_EXEC_SQL_NESTED
      DATA(ls_item) = VALUE ty_result_item(
        filename = ls_obj-filename
        type     = ls_obj-type
      ).

      TRY.
          DATA(lv_dot)      = find( val = ls_obj-filename sub = '.' ).
          DATA(lv_obj_name) = ls_obj-filename(lv_dot).
          DATA(lv_suffix)   = ls_obj-type.

          IF is_req-force = abap_false.
            READ TABLE lt_hot_status WITH KEY
              hana_object_name   = lv_obj_name
              hana_object_suffix = lv_suffix
              ASSIGNING FIELD-SYMBOL(<ls_hot>).
            IF sy-subrc = 0 AND <ls_hot>-hot_status = 'A'.
              ls_item-status  = 'skipped'.
              ls_item-message = 'already active'.
              APPEND ls_item TO ls_resp-results.
              ls_resp-skipped += 1.
              CONTINUE.
            ENDIF.
          ENDIF.

          " Step 1: Write content into CTS_HOT_OBJECT with normalized ABAP key fields
          DATA(lo_hot_obj) = cl_cts_hot_object_v1=>create_instance(
            iv_hana_package_id    = is_req-hanapackage
            iv_hana_object_name   = lv_obj_name
            iv_hana_object_suffix = lv_suffix ).

          CLEAR ls_hot.
          ls_hot-abap_hana_package_id         = lo_hot_obj->abap_hana_package_id.
          ls_hot-abap_hana_object_name_suffix = lo_hot_obj->abap_hana_object_name_suffix.
          ls_hot-hana_package_id    = is_req-hanapackage.
          ls_hot-hana_object_name   = lv_obj_name.
          ls_hot-hana_object_suffix = lv_suffix.
          ls_hot-hana_content_cdata = ls_obj-content.
          ls_hot-abap_status        = 'A'.
          ls_hot-hot_status         = 'I'.
          lo_db_access->if_cts_hot_db_access~modify_cts_hot_object( ls_hot ).

          " Step 2: Deploy via HTA API (reads content from CTS_HOT_OBJECT)
          DATA(lo_hta_obj) = lo_hta_factory->create_object_by_hana_name(
            i_hana_package_name  = is_req-hanapackage
            i_hana_object_name   = lv_obj_name
            i_hana_object_suffix = lv_suffix ).

          DATA lv_deploy_status TYPE if_cts_hta_types=>ty_deploy_status.
          DATA lt_deploy_msgs TYPE if_cts_hta_types=>ty_deploy_messages.
          lo_hta_obj->deploy(
            IMPORTING
              e_overall_deploy_status = lv_deploy_status
              e_deploy_messages       = lt_deploy_msgs ).

          IF lv_deploy_status = 'E' OR lv_deploy_status = 'F'.
            " Resolve each error message through its own message class so the &1..&4
            " placeholders are substituted (e.g. SCTS_HOT/563 -> "HANA activation ID: 1715,
            " HANA return code: 40136"). For 50-char-chunked templates like SCTS_HOT/532
            " (&1&2&3&4) this reassembles the sentence cleanly.
            DATA lv_err_detail TYPE string.
            DATA lv_resolved   TYPE string.
            LOOP AT lt_deploy_msgs INTO DATA(ls_msg)
              WHERE severity = 'E' AND msgnr <> '555'.
              MESSAGE ID ls_msg-ag
                      TYPE ls_msg-severity
                      NUMBER ls_msg-msgnr
                      WITH ls_msg-var1 ls_msg-var2 ls_msg-var3 ls_msg-var4
                      INTO lv_resolved.
              lv_resolved = condense( val = lv_resolved del = ` ` ).
              IF lv_err_detail IS INITIAL.
                lv_err_detail = lv_resolved.
              ELSE.
                lv_err_detail = |{ lv_err_detail }\n  { lv_resolved }|.
              ENDIF.
            ENDLOOP.
            IF lv_err_detail IS INITIAL.
              READ TABLE lt_deploy_msgs INTO DATA(ls_first) INDEX 1.
              MESSAGE ID ls_first-ag
                      TYPE ls_first-severity
                      NUMBER ls_first-msgnr
                      WITH ls_first-var1 ls_first-var2 ls_first-var3 ls_first-var4
                      INTO lv_err_detail.
              lv_err_detail = condense( val = lv_err_detail del = ` ` ).
            ENDIF.
            ls_item-status  = 'failed'.
            ls_item-message = |Deploy failed ({ lv_deploy_status })\n  { lv_err_detail }|.
            ls_resp-error   = 'deploy_failed'.
          ELSE.
            " Step 3: Assign to transport request (only if package is transportable)
            IF lv_transport IS NOT INITIAL.
              CALL FUNCTION 'RS_CORR_INSERT'
                EXPORTING
                  object          = CONV sobj_name( ls_hot-hana_package_id && '/' && lv_obj_name && '.' && lv_suffix )
                  object_class    = 'HOTO'
                  devclass        = lv_devclass
                  korrnum         = lv_transport
                  master_language = sy-langu
                  suppress_dialog = abap_true
                EXCEPTIONS
                  cancelled           = 1
                  permission_failure  = 2
                  unknown_objectclass = 3
                  OTHERS              = 4.
            ENDIF.

            ls_item-status = 'deployed'.
            ls_resp-deployed += 1.
          ENDIF.

        CATCH cx_root INTO DATA(lx_err).
          ls_item-status  = 'failed'.
          ls_item-message = lx_err->get_text( ).
          ls_resp-error   = 'deploy_failed'.
      ENDTRY.

      APPEND ls_item TO ls_resp-results.
    ENDLOOP.

    ls_resp-success   = xsdbool( ls_resp-error IS INITIAL ).
    ls_resp-transport = lv_transport.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD list.
    DATA ls_resp TYPE ty_list_response.

    ls_resp-package = is_req-hanapackage.

    TRY.
        DATA(lo_factory) = cl_cts_hta_api_factory=>create_instance( ).
        DATA(lo_full_pkg) = lo_factory->create_full_package_hana_name(
          i_hana_package_name = is_req-hanapackage ).

        DATA(lt_components) = lo_full_pkg->get_components(
          i_component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object ).

        " Build item list and collect keys for bulk HOT status lookup
        TYPES: BEGIN OF ty_hot_key,
                 hana_object_name   TYPE if_cts_hta_types=>ty_hana_object_name,
                 hana_object_suffix TYPE if_cts_hta_types=>ty_hana_object_suffix,
               END OF ty_hot_key.
        DATA lt_hot_keys TYPE STANDARD TABLE OF ty_hot_key WITH DEFAULT KEY.

        LOOP AT lt_components INTO DATA(lo_comp). "#EC CI_EXEC_SQL_NESTED
          DATA(lo_obj) = CAST if_cts_hta_object( lo_comp ).
          DATA ls_item TYPE ty_list_item.
          ls_item-name   = lo_obj->object_key-hana_object_name.
          ls_item-suffix = lo_obj->object_key-hana_object_suffix.

          TRY.
              DATA(lo_state) = lo_obj->get_deploy_state( ).
              IF lo_state = ce_cts_hta_deploy_state=>deployed.
                ls_item-deploy_state = 'deployed'.
              ELSE.
                ls_item-deploy_state = 'not_deployed'.
              ENDIF.
            CATCH cx_cts_hta_not_found.
              ls_item-deploy_state = 'not_deployed'.
          ENDTRY.

          APPEND ls_item TO ls_resp-objects.
          APPEND VALUE ty_hot_key(
            hana_object_name   = lo_obj->object_key-hana_object_name
            hana_object_suffix = lo_obj->object_key-hana_object_suffix
          ) TO lt_hot_keys.
        ENDLOOP.

        " Bulk read HOT status for all objects in one SELECT
        IF lt_hot_keys IS NOT INITIAL.
          SELECT hana_package_id, hana_object_name, hana_object_suffix, hot_status
            FROM cts_hot_object
            FOR ALL ENTRIES IN @lt_hot_keys
            WHERE hana_package_id    = @is_req-hanapackage
              AND hana_object_name   = @lt_hot_keys-hana_object_name
              AND hana_object_suffix = @lt_hot_keys-hana_object_suffix
            INTO TABLE @DATA(lt_hot).

          LOOP AT ls_resp-objects ASSIGNING FIELD-SYMBOL(<ls_item>).
            READ TABLE lt_hot WITH KEY
              hana_object_name   = <ls_item>-name
              hana_object_suffix = <ls_item>-suffix
              ASSIGNING FIELD-SYMBOL(<ls_hot>).
            IF sy-subrc = 0.
              <ls_item>-hot_status = <ls_hot>-hot_status.
            ENDIF.
          ENDLOOP.
        ENDIF.

        ls_resp-success = abap_true.

      CATCH cx_cts_hta INTO DATA(lx_hta).
        ls_resp-success = abap_false.
        ls_resp-error   = lx_hta->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD import.
    DATA ls_resp TYPE ty_import_response.

    ls_resp-package = is_req-hanapackage.

    TRY.
        DATA(lo_factory)  = cl_cts_hta_api_factory=>create_instance( ).
        DATA(lo_full_pkg) = lo_factory->create_full_package_hana_name(
          i_hana_package_name = is_req-hanapackage ).

        DATA(lt_components) = lo_full_pkg->get_components(
          i_component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object ).

        DATA(lo_sql_conn) = cl_sql_connection=>get_connection( ).

        " ADBC query per object — one HANA roundtrip per imported view is
        " unavoidable (each object lives in its own _SYS_REPO.ACTIVE_OBJECT
        " row keyed by package+name+suffix; no bulk fetch matches our key set).
        LOOP AT lt_components INTO DATA(lo_comp). "#EC CI_EXEC_SQL_NESTED
          DATA(lo_obj) = CAST if_cts_hta_object( lo_comp ).
          DATA ls_item TYPE ty_import_item.
          ls_item-name   = lo_obj->object_key-hana_object_name.
          ls_item-suffix = lo_obj->object_key-hana_object_suffix.

          TRY.
              " Read design-time content directly from HANA repository via ADBC.
              " Bypasses synchronize() which hangs on calculation views due to
              " full content + texts fetch via NHI. _SYS_REPO.ACTIVE_OBJECT holds
              " the actual design-time XML/SQL of the activated object.
              DATA(lo_stmt) = lo_sql_conn->create_statement( ).
              DATA(lv_pkg_safe)    = cl_abap_dyn_prg=>escape_quotes( is_req-hanapackage ).
              DATA(lv_name_safe)   = cl_abap_dyn_prg=>escape_quotes( ls_item-name ).
              DATA(lv_suffix_safe) = cl_abap_dyn_prg=>escape_quotes( ls_item-suffix ).
              DATA(lv_sql) = |SELECT TOP 1 CDATA FROM "_SYS_REPO"."ACTIVE_OBJECT" | &&
                             |WHERE PACKAGE_ID = '{ lv_pkg_safe }' | &&
                             |AND OBJECT_NAME = '{ lv_name_safe }' | &&
                             |AND OBJECT_SUFFIX = '{ lv_suffix_safe }'|.
              DATA(lo_res) = lo_stmt->execute_query( lv_sql ). "#EC CI_EXEC_SQL_NESTED

              DATA: BEGIN OF ls_row,
                      cdata TYPE string,
                    END OF ls_row.
              DATA lr_row TYPE REF TO data.
              GET REFERENCE OF ls_row INTO lr_row.
              lo_res->set_param_struct( lr_row ).

              IF lo_res->next( ) <> 0.
                ls_item-content = ls_row-cdata.
                ls_item-status  = 'ok'.
              ELSE.
                ls_item-status  = 'error'.
                ls_item-message = 'Object not found in _SYS_REPO.ACTIVE_OBJECT'.
              ENDIF.
              lo_res->close( ).
            CATCH cx_sql_exception INTO DATA(lx_sql).
              ls_item-status  = 'error'.
              ls_item-message = lx_sql->get_text( ).
          ENDTRY.

          APPEND ls_item TO ls_resp-objects.
        ENDLOOP.

        ls_resp-success = abap_true.

      CATCH cx_cts_hta INTO DATA(lx_hta).
        ls_resp-success = abap_false.
        ls_resp-error   = lx_hta->get_text( ).
      CATCH cx_sql_exception INTO DATA(lx_sql_outer).
        " cl_sql_connection=>get_connection( ) raises this if no HANA
        " connection is configured; treat as fatal for the whole import.
        ls_resp-success = abap_false.
        ls_resp-error   = lx_sql_outer->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD drop.
    DATA ls_resp TYPE ty_response.
    DATA(ls_item) = VALUE ty_result_item(
      filename = |{ is_req-objectname }.{ is_req-objectsuffix }|
      type     = is_req-objectsuffix ).

    TRY.
        " Compute the normalized HOT key fields for the object
        DATA(lo_hot_obj) = cl_cts_hot_object_v1=>create_instance(
          iv_hana_package_id    = is_req-hanapackage
          iv_hana_object_name   = is_req-objectname
          iv_hana_object_suffix = is_req-objectsuffix ).

        " Read the existing HOT row; if absent, the object is already gone
        DATA ls_hot TYPE cts_hot_object.
        SELECT SINGLE * FROM cts_hot_object
          WHERE abap_hana_package_id         = @lo_hot_obj->abap_hana_package_id
            AND abap_hana_object_name_suffix = @lo_hot_obj->abap_hana_object_name_suffix
            AND abap_status                  = 'A'
          INTO @ls_hot.

        IF sy-subrc <> 0.
          ls_item-status  = 'skipped'.
          ls_item-message = 'not found in HOT'.
          APPEND ls_item TO ls_resp-results.
          ls_resp-success = abap_true.
          /ui2/cl_json=>serialize(
            EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING r_json = rv_result ).
          RETURN.
        ENDIF.

        IF is_req-dryrun = abap_true.
          ls_item-status = 'would_drop'.
          APPEND ls_item TO ls_resp-results.
          ls_resp-success = abap_true.
          ls_resp-dry_run = abap_true.
          ls_resp-deployed = 1.
          /ui2/cl_json=>serialize(
            EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING r_json = rv_result ).
          RETURN.
        ENDIF.

        " Mark for deletion and persist via the HOT DB-access API
        DATA(lo_db_access) = NEW cl_cts_hot_db_access( ).
        ls_hot-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted.
        lo_db_access->if_cts_hot_db_access~modify_cts_hot_object( ls_hot ).

        " Propagate the delete to HANA via the HTA API
        DATA(lo_hta_factory) = cl_cts_hta_api_factory=>create_instance( ).
        DATA(lo_hta_obj)     = lo_hta_factory->create_object_by_hana_name(
          i_hana_package_name  = is_req-hanapackage
          i_hana_object_name   = is_req-objectname
          i_hana_object_suffix = is_req-objectsuffix ).

        DATA lv_deploy_status TYPE if_cts_hta_types=>ty_deploy_status.
        DATA lt_deploy_msgs   TYPE if_cts_hta_types=>ty_deploy_messages.
        lo_hta_obj->deploy(
          IMPORTING
            e_overall_deploy_status = lv_deploy_status
            e_deploy_messages       = lt_deploy_msgs ).

        IF lv_deploy_status = 'E' OR lv_deploy_status = 'F'.
          DATA lv_err_detail TYPE string.
          DATA lv_resolved   TYPE string.
          LOOP AT lt_deploy_msgs INTO DATA(ls_msg)
            WHERE severity = 'E' AND msgnr <> '555'.
            MESSAGE ID ls_msg-ag
                    TYPE ls_msg-severity
                    NUMBER ls_msg-msgnr
                    WITH ls_msg-var1 ls_msg-var2 ls_msg-var3 ls_msg-var4
                    INTO lv_resolved.
            lv_resolved = condense( val = lv_resolved del = ` ` ).
            IF lv_err_detail IS INITIAL.
              lv_err_detail = lv_resolved.
            ELSE.
              lv_err_detail = |{ lv_err_detail }\n  { lv_resolved }|.
            ENDIF.
          ENDLOOP.
          ls_item-status  = 'failed'.
          ls_item-message = |Drop failed ({ lv_deploy_status })\n  { lv_err_detail }|.
          ls_resp-error   = 'drop_failed'.
        ELSE.
          ls_item-status   = 'dropped'.
          ls_resp-success  = abap_true.
          ls_resp-deployed = 1.
        ENDIF.
      CATCH cx_root INTO DATA(lx_err).
        ls_item-status  = 'failed'.
        ls_item-message = lx_err->get_text( ).
        ls_resp-error   = 'drop_failed'.
    ENDTRY.

    APPEND ls_item TO ls_resp-results.
    /ui2/cl_json=>serialize(
      EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING r_json = rv_result ).
  ENDMETHOD.

  METHOD preview.
    DATA ls_resp     TYPE ty_preview_response.
    DATA lv_view_key TYPE string.

    lv_view_key  = |{ is_req-hanapackage }/{ is_req-objectname }|.
    ls_resp-view = lv_view_key.

    " 1. Verify the view is active in _SYS_REPO.ACTIVE_OBJECT
    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( ).
        DATA(lo_check_stmt) = lo_conn->create_statement( ).

        DATA(lv_pkg_safe)    = cl_abap_dyn_prg=>escape_quotes( is_req-hanapackage ).
        DATA(lv_name_safe)   = cl_abap_dyn_prg=>escape_quotes( is_req-objectname ).
        DATA(lv_suffix_safe) = cl_abap_dyn_prg=>escape_quotes( is_req-objectsuffix ).
        DATA(lv_active_sql)  = |SELECT COUNT(*) AS CNT FROM "_SYS_REPO"."ACTIVE_OBJECT" | &&
                               |WHERE PACKAGE_ID = '{ lv_pkg_safe }' | &&
                               |AND OBJECT_NAME = '{ lv_name_safe }' | &&
                               |AND OBJECT_SUFFIX = '{ lv_suffix_safe }'|.

        DATA(lo_check_res) = lo_check_stmt->execute_query( lv_active_sql ). "#EC CI_EXEC_SQL_NESTED
        DATA: BEGIN OF ls_check, cnt TYPE i, END OF ls_check.
        DATA lr_check TYPE REF TO data.
        GET REFERENCE OF ls_check INTO lr_check.
        lo_check_res->set_param_struct( lr_check ).
        lo_check_res->next( ).
        lo_check_res->close( ).

        IF ls_check-cnt = 0.
          ls_resp-success = abap_false.
          ls_resp-error   = 'view_not_active'.
          ls_resp-message = |View "{ lv_view_key }" is not active in HANA. | &&
                            |Deploy it first: abapgit-agent hana view deploy --file <path>|.
          /ui2/cl_json=>serialize(
            EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING r_json = rv_result ).
          RETURN.
        ENDIF.

        " 2. Build the SELECT
        DATA(lv_limit)  = COND i( WHEN is_req-limit  > 0 AND is_req-limit <= 500 THEN is_req-limit
                                  WHEN is_req-limit  > 500 THEN 500
                                  ELSE 100 ).
        DATA(lv_offset) = COND i( WHEN is_req-offset > 0 THEN is_req-offset ELSE 0 ).
        ls_resp-limit  = lv_limit.
        ls_resp-offset = lv_offset.

        DATA lv_cols TYPE string.
        IF is_req-columns IS NOT INITIAL.
          LOOP AT is_req-columns INTO DATA(lv_col).
            DATA(lv_col_safe) = cl_abap_dyn_prg=>escape_quotes( lv_col ).
            IF lv_cols IS INITIAL.
              lv_cols = |"{ lv_col_safe }"|.
            ELSE.
              lv_cols = |{ lv_cols }, "{ lv_col_safe }"|.
            ENDIF.
          ENDLOOP.
        ELSE.
          lv_cols = '*'.
        ENDIF.

        DATA(lv_sql) = |SELECT { lv_cols } FROM "_SYS_BIC"."{ lv_pkg_safe }/{ lv_name_safe }"|.
        IF is_req-where IS NOT INITIAL.
          " The WHERE body is passed through — user is responsible for SQL quoting their own
          " literals. Escape embedded single quotes so they cannot break out of the statement.
          DATA(lv_where_safe) = cl_abap_dyn_prg=>escape_quotes( is_req-where ).
          lv_sql = |{ lv_sql } WHERE { lv_where_safe }|.
        ENDIF.
        lv_sql = |{ lv_sql } LIMIT { lv_limit } OFFSET { lv_offset }|.

        " 3. Execute and read result set
        DATA(lo_stmt) = lo_conn->create_statement( ).
        DATA(lo_res)  = lo_stmt->execute_query( lv_sql ). "#EC CI_EXEC_SQL_NESTED

        " Read column metadata and create a dynamic all-string structure for the row.
        DATA(lt_meta) = lo_res->get_metadata( ).
        FIELD-SYMBOLS <ls_meta_col> TYPE any.
        LOOP AT lt_meta ASSIGNING <ls_meta_col>.
          FIELD-SYMBOLS <lv_col_name> TYPE any.
          ASSIGN COMPONENT 'COLUMN_NAME' OF STRUCTURE <ls_meta_col> TO <lv_col_name>.
          FIELD-SYMBOLS <lv_col_type> TYPE any.
          ASSIGN COMPONENT 'DATA_TYPE'  OF STRUCTURE <ls_meta_col> TO <lv_col_type>.
          FIELD-SYMBOLS <lv_col_len>  TYPE any.
          ASSIGN COMPONENT 'LENGTH'     OF STRUCTURE <ls_meta_col> TO <lv_col_len>.
          APPEND VALUE ty_preview_column(
            name   = COND #( WHEN <lv_col_name> IS ASSIGNED THEN <lv_col_name> )
            type   = COND #( WHEN <lv_col_type> IS ASSIGNED THEN <lv_col_type> )
            length = COND #( WHEN <lv_col_len>  IS ASSIGNED THEN <lv_col_len> ) ) TO ls_resp-columns.
        ENDLOOP.

        " Build an all-string row structure via get_struct_ref( string_only = abap_true )
        DATA(lr_row) = lo_res->get_struct_ref( md_tab = lt_meta string_only = abap_true ).
        FIELD-SYMBOLS <lv_struc> TYPE any.
        ASSIGN lr_row->* TO <lv_struc>.
        lo_res->set_param_struct( lr_row ).

        DATA lv_row_count TYPE i.
        WHILE lo_res->next( ) <> 0.
          DATA ls_out_row TYPE ty_preview_row.
          CLEAR ls_out_row-cells.
          DO lines( ls_resp-columns ) TIMES.
            FIELD-SYMBOLS <lv_cell> TYPE any.
            ASSIGN COMPONENT sy-index OF STRUCTURE <lv_struc> TO <lv_cell>.
            IF sy-subrc = 0.
              APPEND CONV string( <lv_cell> ) TO ls_out_row-cells.
            ELSE.
              APPEND `` TO ls_out_row-cells.
            ENDIF.
          ENDDO.
          APPEND ls_out_row TO ls_resp-rows.
          lv_row_count = lv_row_count + 1.
        ENDWHILE.
        lo_res->close( ).
        ls_resp-row_count = lv_row_count.
        ls_resp-success   = abap_true.

      CATCH cx_sql_exception INTO DATA(lx_sql).
        ls_resp-success = abap_false.
        ls_resp-error   = 'invalid_sql'.
        ls_resp-message = lx_sql->get_text( ).
      CATCH cx_root INTO DATA(lx_root).
        ls_resp-success = abap_false.
        ls_resp-error   = 'preview_failed'.
        ls_resp-message = lx_root->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING r_json = rv_result ).
  ENDMETHOD.

  METHOD table_preview.
    DATA ls_resp TYPE ty_table_preview_response.

    IF is_req-logicalschema IS NOT INITIAL.
      ls_resp-logical = is_req-logicalschema.
      ls_resp-table   = |{ is_req-logicalschema }.{ is_req-table }|.
    ELSE.
      ls_resp-table = |{ is_req-schema }.{ is_req-table }|.
    ENDIF.

    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( ).

        " Resolve the physical schema name. For --schema, use it directly.
        " For --logical-schema, look it up in AMDP_SCHEMA_MAP first — ADBC sends
        " raw SQL to HANA which does NOT understand the ABAP-side $ABAP.schema()
        " macro (that's resolved by the AMDP compiler at activation time only).
        DATA lv_physical_schema TYPE string.
        IF is_req-logicalschema IS NOT INITIAL.
          DATA lv_phys TYPE amdp_physical_schema_name.
          SELECT SINGLE physical_schema_name FROM amdp_schema_map
            WHERE logical_schema_name = @( CONV db_schema( to_upper( is_req-logicalschema ) ) )
            INTO @lv_phys.
          IF sy-subrc <> 0 OR lv_phys IS INITIAL.
            ls_resp-success = abap_false.
            ls_resp-error   = 'schema_not_mapped'.
            ls_resp-message = |Logical schema "{ is_req-logicalschema }" is not mapped in DB_SCHEMA_MAP on this system.|.
            /ui2/cl_json=>serialize(
              EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
              RECEIVING r_json = rv_result ).
            RETURN.
          ENDIF.
          " :abap_db_schema is a special marker meaning "use the current ABAP schema".
          " Skip resolution for now and let the user re-run with an explicit --schema.
          IF lv_phys = ':abap_db_schema'.
            ls_resp-success = abap_false.
            ls_resp-error   = 'schema_not_mapped'.
            ls_resp-message = |Logical schema "{ is_req-logicalschema }" maps to ":abap_db_schema" (the local ABAP schema). Re-run with --schema <SAP{ sy-sysid }>.|.
            /ui2/cl_json=>serialize(
              EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
              RECEIVING r_json = rv_result ).
            RETURN.
          ENDIF.
          lv_physical_schema = lv_phys.
          ls_resp-resolved   = lv_phys.
          ls_resp-table      = |{ is_req-logicalschema } → { lv_phys }.{ is_req-table }|.
        ELSE.
          IF is_req-schema IS INITIAL.
            ls_resp-success = abap_false.
            ls_resp-error   = 'missing_schema'.
            ls_resp-message = 'Provide one of schema or logicalSchema.'.
            /ui2/cl_json=>serialize(
              EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
              RECEIVING r_json = rv_result ).
            RETURN.
          ENDIF.
          lv_physical_schema = to_upper( is_req-schema ).
        ENDIF.

        " From here both branches converge: build a normal "<phys>"."<table>" FROM clause
        " and pre-check the table exists in SYS.TABLES.
        DATA(lv_schema_safe)      = cl_abap_dyn_prg=>escape_quotes( lv_physical_schema ).
        DATA(lv_table_upper)      = to_upper( is_req-table ).
        DATA(lv_table_upper_safe) = cl_abap_dyn_prg=>escape_quotes( lv_table_upper ).

        DATA(lv_check_sql) = |SELECT COUNT(*) AS CNT FROM "SYS"."TABLES" | &&
                             |WHERE SCHEMA_NAME = '{ lv_schema_safe }' | &&
                             |AND TABLE_NAME  = '{ lv_table_upper_safe }'|.
        DATA(lo_check_stmt) = lo_conn->create_statement( ).
        DATA(lo_check_res)  = lo_check_stmt->execute_query( lv_check_sql ). "#EC CI_EXEC_SQL_NESTED
        DATA: BEGIN OF ls_check, cnt TYPE i, END OF ls_check.
        DATA lr_check TYPE REF TO data.
        GET REFERENCE OF ls_check INTO lr_check.
        lo_check_res->set_param_struct( lr_check ).
        lo_check_res->next( ).
        lo_check_res->close( ).
        IF ls_check-cnt = 0.
          ls_resp-success = abap_false.
          ls_resp-error   = 'table_not_found'.
          ls_resp-message = |Table { lv_table_upper } not found in schema { lv_physical_schema }.|.
          /ui2/cl_json=>serialize(
            EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING r_json = rv_result ).
          RETURN.
        ENDIF.

        DATA(lv_from_clause) = |"{ lv_schema_safe }"."{ lv_table_upper_safe }"|.

        " Build the SELECT
        DATA(lv_limit)  = COND i( WHEN is_req-limit > 0 AND is_req-limit <= 500 THEN is_req-limit
                                  WHEN is_req-limit > 500 THEN 500
                                  ELSE 100 ).
        DATA(lv_offset) = COND i( WHEN is_req-offset > 0 THEN is_req-offset ELSE 0 ).
        ls_resp-limit  = lv_limit.
        ls_resp-offset = lv_offset.

        DATA lv_cols TYPE string.
        IF is_req-columns IS NOT INITIAL.
          LOOP AT is_req-columns INTO DATA(lv_col).
            DATA(lv_col_safe) = cl_abap_dyn_prg=>escape_quotes( lv_col ).
            IF lv_cols IS INITIAL.
              lv_cols = |"{ lv_col_safe }"|.
            ELSE.
              lv_cols = |{ lv_cols }, "{ lv_col_safe }"|.
            ENDIF.
          ENDLOOP.
        ELSE.
          lv_cols = '*'.
        ENDIF.

        DATA(lv_sql) = |SELECT { lv_cols } FROM { lv_from_clause }|.
        IF is_req-where IS NOT INITIAL.
          DATA(lv_where_safe) = cl_abap_dyn_prg=>escape_quotes( is_req-where ).
          lv_sql = |{ lv_sql } WHERE { lv_where_safe }|.
        ENDIF.
        lv_sql = |{ lv_sql } LIMIT { lv_limit } OFFSET { lv_offset }|.

        " Execute
        DATA(lo_stmt) = lo_conn->create_statement( ).
        DATA(lo_res)  = lo_stmt->execute_query( lv_sql ). "#EC CI_EXEC_SQL_NESTED

        " Column metadata
        DATA(lt_meta) = lo_res->get_metadata( ).
        FIELD-SYMBOLS <ls_meta_col> TYPE any.
        LOOP AT lt_meta ASSIGNING <ls_meta_col>.
          FIELD-SYMBOLS <lv_col_name> TYPE any.
          ASSIGN COMPONENT 'COLUMN_NAME' OF STRUCTURE <ls_meta_col> TO <lv_col_name>.
          FIELD-SYMBOLS <lv_col_type> TYPE any.
          ASSIGN COMPONENT 'DATA_TYPE'  OF STRUCTURE <ls_meta_col> TO <lv_col_type>.
          FIELD-SYMBOLS <lv_col_len>   TYPE any.
          ASSIGN COMPONENT 'LENGTH'     OF STRUCTURE <ls_meta_col> TO <lv_col_len>.
          APPEND VALUE ty_preview_column(
            name   = COND #( WHEN <lv_col_name> IS ASSIGNED THEN <lv_col_name> )
            type   = COND #( WHEN <lv_col_type> IS ASSIGNED THEN <lv_col_type> )
            length = COND #( WHEN <lv_col_len>  IS ASSIGNED THEN <lv_col_len> ) ) TO ls_resp-columns.
        ENDLOOP.

        " Dynamic all-string row struct
        DATA(lr_row) = lo_res->get_struct_ref( md_tab = lt_meta string_only = abap_true ).
        FIELD-SYMBOLS <lv_struc> TYPE any.
        ASSIGN lr_row->* TO <lv_struc>.
        lo_res->set_param_struct( lr_row ).

        DATA lv_row_count TYPE i.
        WHILE lo_res->next( ) <> 0.
          DATA ls_out_row TYPE ty_preview_row.
          CLEAR ls_out_row-cells.
          DO lines( ls_resp-columns ) TIMES.
            FIELD-SYMBOLS <lv_cell> TYPE any.
            ASSIGN COMPONENT sy-index OF STRUCTURE <lv_struc> TO <lv_cell>.
            IF sy-subrc = 0.
              APPEND CONV string( <lv_cell> ) TO ls_out_row-cells.
            ELSE.
              APPEND `` TO ls_out_row-cells.
            ENDIF.
          ENDDO.
          APPEND ls_out_row TO ls_resp-rows.
          lv_row_count = lv_row_count + 1.
        ENDWHILE.
        lo_res->close( ).
        ls_resp-row_count = lv_row_count.
        ls_resp-success   = abap_true.

      CATCH cx_sql_exception INTO DATA(lx_sql).
        ls_resp-success = abap_false.
        DATA(lv_msg)    = lx_sql->get_text( ).
        " Classify a few common HANA errors so the CLI can render hints
        IF lv_msg CS 'not mapped'.
          ls_resp-error = 'schema_not_mapped'.
        ELSEIF lv_msg CS 'invalid table name'.
          ls_resp-error = 'table_not_found'.
        ELSE.
          ls_resp-error = 'invalid_sql'.
        ENDIF.
        ls_resp-message = lv_msg.
      CATCH cx_root INTO DATA(lx_root).
        ls_resp-success = abap_false.
        ls_resp-error   = 'preview_failed'.
        ls_resp-message = lx_root->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING r_json = rv_result ).
  ENDMETHOD.

  METHOD schema.
    CASE is_req-subaction.
      WHEN 'list'.
        rv_result = schema_list( is_req ).
      WHEN 'tables'.
        rv_result = schema_tables( is_req ).
      WHEN 'fields'.
        rv_result = schema_fields( is_req ).
      WHEN OTHERS.
        rv_result = '{"success":false,"error":"unknown_subaction"}'.
    ENDCASE.
  ENDMETHOD.

  METHOD schema_list.
    DATA ls_resp TYPE ty_schema_list_response.
    ls_resp-subaction = 'list'.

    DATA(lv_limit) = COND i( WHEN is_req-limit > 0 THEN is_req-limit ELSE 100 ).

    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( ).
        DATA(lo_stmt) = lo_conn->create_statement( ).

        " User-supplied filter goes through ESCAPE_QUOTES so embedded quotes
        " cannot break out of the literal — SLIN treats the result as safe.
        DATA(lv_sql) = |SELECT SCHEMA_NAME FROM "SYS"."SCHEMAS"|.
        IF is_req-filter IS NOT INITIAL.
          DATA(lv_filter) = cl_abap_dyn_prg=>escape_quotes( is_req-filter ).
          lv_sql = |{ lv_sql } WHERE SCHEMA_NAME LIKE '{ lv_filter }'|.
        ENDIF.
        lv_sql = |{ lv_sql } ORDER BY SCHEMA_NAME LIMIT { lv_limit }|.

        DATA(lo_res) = lo_stmt->execute_query( lv_sql ). "#EC CI_EXEC_SQL_NESTED

        DATA: BEGIN OF ls_row,
                schema_name TYPE c LENGTH 256,
              END OF ls_row.
        DATA lr_row TYPE REF TO data.
        GET REFERENCE OF ls_row INTO lr_row.
        lo_res->set_param_struct( lr_row ).

        WHILE lo_res->next( ) <> 0.
          APPEND VALUE ty_schema_row( schema_name = ls_row-schema_name ) TO ls_resp-rows.
        ENDWHILE.
        lo_res->close( ).

        ls_resp-success = abap_true.
      CATCH cx_sql_exception INTO DATA(lx_sql).
        ls_resp-success = abap_false.
        ls_resp-error   = lx_sql->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD schema_tables.
    DATA ls_resp TYPE ty_schema_tables_response.
    ls_resp-subaction = 'tables'.

    IF is_req-schema IS INITIAL.
      ls_resp-success = abap_false.
      ls_resp-error   = 'schema is required'.
      /ui2/cl_json=>serialize(
        EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING r_json = rv_result ).
      RETURN.
    ENDIF.

    DATA(lv_schema) = to_upper( is_req-schema ).
    DATA(lv_limit)  = COND i( WHEN is_req-limit > 0 THEN is_req-limit ELSE 100 ).

    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( ).
        DATA(lo_stmt) = lo_conn->create_statement( ).

        " Escape embedded quotes so SLIN accepts the dynamic SQL as safe.
        DATA(lv_schema_safe) = cl_abap_dyn_prg=>escape_quotes( lv_schema ).
        DATA(lv_sql) = |SELECT TABLE_NAME, IS_COLUMN_TABLE FROM "SYS"."TABLES" | &&
                       |WHERE SCHEMA_NAME = '{ lv_schema_safe }'|.
        IF is_req-filter IS NOT INITIAL.
          DATA(lv_filter) = cl_abap_dyn_prg=>escape_quotes( is_req-filter ).
          lv_sql = |{ lv_sql } AND TABLE_NAME LIKE '{ lv_filter }'|.
        ENDIF.
        lv_sql = |{ lv_sql } ORDER BY TABLE_NAME LIMIT { lv_limit }|.

        DATA(lo_res) = lo_stmt->execute_query( lv_sql ). "#EC CI_EXEC_SQL_NESTED

        DATA: BEGIN OF ls_row,
                table_name      TYPE c LENGTH 256,
                is_column_table TYPE c LENGTH 5,
              END OF ls_row.
        DATA lr_row TYPE REF TO data.
        GET REFERENCE OF ls_row INTO lr_row.
        lo_res->set_param_struct( lr_row ).

        WHILE lo_res->next( ) <> 0.
          APPEND VALUE ty_table_row(
            table_name      = ls_row-table_name
            is_column_table = ls_row-is_column_table ) TO ls_resp-rows.
        ENDWHILE.
        lo_res->close( ).

        ls_resp-success = abap_true.
      CATCH cx_sql_exception INTO DATA(lx_sql).
        ls_resp-success = abap_false.
        ls_resp-error   = lx_sql->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

  METHOD schema_fields.
    DATA ls_resp TYPE ty_schema_fields_response.
    ls_resp-subaction = 'fields'.

    IF is_req-schema IS INITIAL OR is_req-table IS INITIAL.
      ls_resp-success = abap_false.
      ls_resp-error   = 'schema and table are required'.
      /ui2/cl_json=>serialize(
        EXPORTING data = ls_resp pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING r_json = rv_result ).
      RETURN.
    ENDIF.

    DATA(lv_schema) = to_upper( is_req-schema ).
    DATA(lv_table)  = to_upper( is_req-table ).

    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( ).
        DATA(lo_stmt) = lo_conn->create_statement( ).

        DATA(lv_schema_safe) = cl_abap_dyn_prg=>escape_quotes( lv_schema ).
        DATA(lv_table_safe)  = cl_abap_dyn_prg=>escape_quotes( lv_table ).
        DATA(lv_sql) = |SELECT COLUMN_NAME, DATA_TYPE_NAME, LENGTH, SCALE, IS_NULLABLE, POSITION | &&
                       |FROM "SYS"."TABLE_COLUMNS" | &&
                       |WHERE SCHEMA_NAME = '{ lv_schema_safe }' AND TABLE_NAME = '{ lv_table_safe }' | &&
                       |ORDER BY POSITION|.

        DATA(lo_res) = lo_stmt->execute_query( lv_sql ). "#EC CI_EXEC_SQL_NESTED

        DATA: BEGIN OF ls_row,
                column_name    TYPE c LENGTH 256,
                data_type_name TYPE c LENGTH 30,
                length         TYPE i,
                scale          TYPE i,
                is_nullable    TYPE c LENGTH 5,
                position       TYPE i,
              END OF ls_row.
        DATA lr_row TYPE REF TO data.
        GET REFERENCE OF ls_row INTO lr_row.
        lo_res->set_param_struct( lr_row ).

        WHILE lo_res->next( ) <> 0.
          APPEND VALUE ty_field_row(
            column_name    = ls_row-column_name
            data_type_name = ls_row-data_type_name
            length         = ls_row-length
            scale          = ls_row-scale
            is_nullable    = ls_row-is_nullable
            position       = ls_row-position ) TO ls_resp-rows.
        ENDWHILE.
        lo_res->close( ).

        ls_resp-success = abap_true.
      CATCH cx_sql_exception INTO DATA(lx_sql).
        ls_resp-success = abap_false.
        ls_resp-error   = lx_sql->get_text( ).
    ENDTRY.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_resp
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_result ).
  ENDMETHOD.

ENDCLASS.
