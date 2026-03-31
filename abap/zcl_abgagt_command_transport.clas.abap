*"*"use source
*"*"Local Interface:
*"**********************************************************************
"! <p class="shorttext synchronized">ABAP Git Agent - Transport Command</p>
CLASS zcl_abgagt_command_transport DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    METHODS constructor
      IMPORTING io_cts_api TYPE REF TO zif_abgagt_cts_api OPTIONAL.

    TYPES: BEGIN OF ty_transport_params,
             action      TYPE string,
             scope       TYPE string,
             description TYPE string,
             type        TYPE string,
             number      TYPE trkorr,
           END OF ty_transport_params.

    TYPES: BEGIN OF ty_transport_item,
             number      TYPE trkorr,
             description TYPE string,
             owner       TYPE as4user,
             date        TYPE string,
           END OF ty_transport_item.
    TYPES ty_transport_list TYPE STANDARD TABLE OF ty_transport_item
                             WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_issue_item,
             type     TYPE string,
             obj_type TYPE string,
             obj_name TYPE string,
             text     TYPE string,
           END OF ty_issue_item.
    TYPES ty_issue_list TYPE STANDARD TABLE OF ty_issue_item
                         WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_transport_result,
             success    TYPE abap_bool,
             action     TYPE string,
             scope      TYPE string,
             transports TYPE ty_transport_list,
             number     TYPE trkorr,
             passed     TYPE abap_bool,
             issues     TYPE ty_issue_list,
             message    TYPE string,
             error      TYPE string,
           END OF ty_transport_result.

  PRIVATE SECTION.

    DATA mo_cts_api TYPE REF TO zif_abgagt_cts_api.

    METHODS list_transports
      IMPORTING iv_scope            TYPE string
      RETURNING VALUE(rs_result)    TYPE ty_transport_result.

    METHODS create_transport
      IMPORTING iv_description      TYPE string
                iv_type             TYPE string
      RETURNING VALUE(rs_result)    TYPE ty_transport_result.

    METHODS check_transport
      IMPORTING iv_number           TYPE trkorr
      RETURNING VALUE(rs_result)    TYPE ty_transport_result.

    METHODS release_transport
      IMPORTING iv_number           TYPE trkorr
      RETURNING VALUE(rs_result)    TYPE ty_transport_result.

    METHODS format_date
      IMPORTING iv_date             TYPE d
      RETURNING VALUE(rv_formatted) TYPE string.

ENDCLASS.

CLASS zcl_abgagt_command_transport IMPLEMENTATION.

  METHOD constructor.
    IF io_cts_api IS BOUND.
      mo_cts_api = io_cts_api.
    ELSE.
      CREATE OBJECT mo_cts_api TYPE zcl_abgagt_cts_api.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_transport.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_transport_params,
          ls_result TYPE ty_transport_result.

    IF is_param IS SUPPLIED.
      MOVE-CORRESPONDING is_param TO ls_params.
    ENDIF.

    CASE ls_params-action.
      WHEN 'LIST'.
        DATA(lv_scope) = ls_params-scope.
        IF lv_scope IS INITIAL.
          lv_scope = 'mine'.
        ENDIF.
        ls_result = list_transports( lv_scope ).
      WHEN 'CREATE'.
        ls_result = create_transport( iv_description = ls_params-description
                                      iv_type        = ls_params-type ).
      WHEN 'CHECK'.
        ls_result = check_transport( ls_params-number ).
      WHEN 'RELEASE'.
        ls_result = release_transport( ls_params-number ).
      WHEN OTHERS.
        ls_result-success = abap_false.
        ls_result-error   = |Unknown action: { ls_params-action }. Valid: LIST, CREATE, CHECK, RELEASE|.
    ENDCASE.

    rv_result = /ui2/cl_json=>serialize(
      data        = ls_result
      pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

  METHOD list_transports.
    DATA: lt_items        TYPE ty_transport_list,
          ls_item         TYPE ty_transport_item,
          lt_range        TYPE RANGE OF trkorr,
          ls_range_entry  LIKE LINE OF lt_range.

    rs_result-action = 'LIST'.
    rs_result-scope  = iv_scope.

    CASE iv_scope.

      WHEN 'mine'.
        SELECT e070~trkorr, e070~as4user, e070~as4date, e07t~as4text
          FROM e070
          INNER JOIN e07t ON e07t~trkorr = e070~trkorr
                         AND e07t~langu   = @sy-langu
          WHERE e070~trstatus   = 'D'
            AND e070~trfunction = 'K'
            AND e070~as4user    = @sy-uname
          ORDER BY e070~as4date DESCENDING
          INTO TABLE @DATA(lt_raw_mine)
          UP TO 50 ROWS.

        LOOP AT lt_raw_mine INTO DATA(ls_raw_mine).
          ls_item-number      = ls_raw_mine-trkorr.
          ls_item-description = ls_raw_mine-as4text.
          ls_item-owner       = ls_raw_mine-as4user.
          ls_item-date        = format_date( ls_raw_mine-as4date ).
          APPEND ls_item TO lt_items.
        ENDLOOP.

      WHEN 'task'.
        " Step 1: find parent transport orders of user's tasks
        " Note: task types include T (Development/Correction), S (Repair), X (Unclassified), etc.
        " Do not filter by trfunction — any task type belonging to the user qualifies.
        " The parent transport (strkorr) is what matters; Step 4 filters for K (orders) only.
        SELECT DISTINCT strkorr
          FROM e070
          WHERE trstatus   = 'D'
            AND trfunction <> 'K'
            AND strkorr    <> ''
            AND as4user    = @sy-uname
          INTO TABLE @DATA(lt_from_tasks).

        " Step 2: find transport orders owned by user
        SELECT trkorr
          FROM e070
          WHERE trstatus   = 'D'
            AND trfunction = 'K'
            AND as4user    = @sy-uname
          INTO TABLE @DATA(lt_owned).

        " Step 3: build combined range
        ls_range_entry-sign   = 'I'.
        ls_range_entry-option = 'EQ'.
        LOOP AT lt_from_tasks INTO DATA(ls_task_parent).
          ls_range_entry-low = ls_task_parent-strkorr.
          APPEND ls_range_entry TO lt_range.
        ENDLOOP.
        LOOP AT lt_owned INTO DATA(ls_owned_entry).
          ls_range_entry-low = ls_owned_entry-trkorr.
          APPEND ls_range_entry TO lt_range.
        ENDLOOP.

        IF lt_range IS INITIAL.
          rs_result-success = abap_true.
          rs_result-message = 'No transports found'.
          RETURN.
        ENDIF.

        " Step 4: fetch transport details
        SELECT e070~trkorr, e070~as4user, e070~as4date, e07t~as4text
          FROM e070
          INNER JOIN e07t ON e07t~trkorr = e070~trkorr
                         AND e07t~langu   = @sy-langu
          WHERE e070~trstatus   = 'D'
            AND e070~trfunction = 'K'
            AND e070~trkorr IN @lt_range
          ORDER BY e070~as4date DESCENDING
          INTO TABLE @DATA(lt_raw_tasks)
          UP TO 50 ROWS.

        LOOP AT lt_raw_tasks INTO DATA(ls_raw_tasks).
          ls_item-number      = ls_raw_tasks-trkorr.
          ls_item-description = ls_raw_tasks-as4text.
          ls_item-owner       = ls_raw_tasks-as4user.
          ls_item-date        = format_date( ls_raw_tasks-as4date ).
          APPEND ls_item TO lt_items.
        ENDLOOP.

      WHEN 'all'.
        SELECT e070~trkorr, e070~as4user, e070~as4date, e07t~as4text
          FROM e070
          INNER JOIN e07t ON e07t~trkorr = e070~trkorr
                         AND e07t~langu   = @sy-langu
          WHERE e070~trstatus   = 'D'
            AND e070~trfunction = 'K'
          ORDER BY e070~as4date DESCENDING
          INTO TABLE @DATA(lt_raw_all)
          UP TO 50 ROWS.

        LOOP AT lt_raw_all INTO DATA(ls_raw_all).
          ls_item-number      = ls_raw_all-trkorr.
          ls_item-description = ls_raw_all-as4text.
          ls_item-owner       = ls_raw_all-as4user.
          ls_item-date        = format_date( ls_raw_all-as4date ).
          APPEND ls_item TO lt_items.
        ENDLOOP.

      WHEN OTHERS.
        rs_result-success = abap_false.
        rs_result-error   = |Unknown scope: { iv_scope }. Valid: mine, task, all|.
        RETURN.
    ENDCASE.

    rs_result-success    = abap_true.
    rs_result-transports = lt_items.
    rs_result-message    = |{ lines( lt_items ) } transport(s) found|.
  ENDMETHOD.

  METHOD create_transport.
    rs_result-action = 'CREATE'.

    DATA(lv_category) = COND char01(
      WHEN iv_type = 'customizing' THEN 'W'
      ELSE 'K' ).

    DATA(ls_create) = mo_cts_api->create_transport(
                        iv_description = iv_description
                        iv_category    = lv_category ).

    IF ls_create-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error   = |Failed to create transport request (sy-subrc: { ls_create-subrc })|.
      RETURN.
    ENDIF.

    DATA(lv_type_label) = COND string(
      WHEN iv_type = 'customizing' THEN 'Customizing'
      ELSE 'Workbench' ).

    rs_result-success = abap_true.
    rs_result-number  = ls_create-trkorr.
    rs_result-message = |{ lv_type_label } request { ls_create-trkorr } created successfully|.
  ENDMETHOD.

  METHOD check_transport.
    rs_result-action = 'CHECK'.
    rs_result-number = iv_number.

    IF iv_number IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error   = 'Transport number is required for CHECK action'.
      RETURN.
    ENDIF.

    " Use EXCEPTIONS-only approach — CTS_API_CHECK_TRANSPORT returns sy-subrc = 0 on pass
    DATA(lv_check_subrc) = mo_cts_api->check_transport( iv_number ).

    rs_result-success = abap_true.
    rs_result-passed  = xsdbool( lv_check_subrc = 0 ).
    rs_result-message = COND #(
      WHEN lv_check_subrc = 0
      THEN |Transport { iv_number } passed check|
      ELSE |Transport { iv_number } check found issues (sy-subrc: { lv_check_subrc })| ).
  ENDMETHOD.

  METHOD release_transport.
    rs_result-action = 'RELEASE'.
    rs_result-number = iv_number.

    IF iv_number IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error   = 'Transport number is required for RELEASE action'.
      RETURN.
    ENDIF.

    DATA(lv_release_subrc) = mo_cts_api->release_transport( iv_number ).

    IF lv_release_subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error   = |Failed to release transport { iv_number } (sy-subrc: { lv_release_subrc })|.
      RETURN.
    ENDIF.

    rs_result-success = abap_true.
    rs_result-message = |Transport { iv_number } released successfully|.
  ENDMETHOD.

  METHOD format_date.
    DATA(lv_year)  = iv_date+0(4).
    DATA(lv_month) = iv_date+4(2).
    DATA(lv_day)   = iv_date+6(2).
    rv_formatted   = |{ lv_year }-{ lv_month }-{ lv_day }|.
  ENDMETHOD.

ENDCLASS.
