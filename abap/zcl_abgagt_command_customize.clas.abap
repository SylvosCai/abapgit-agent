"! <p class="shorttext synchronized">ABAP Git Agent - Customize Command</p>
"! Writes a single row (insert-or-update) into a customizing table (delivery class C or E).
"! Validates delivery class, package transport flag, transport type, and field values before writing.
CLASS zcl_abgagt_command_customize DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_field_value,
             field TYPE string,
             value TYPE string,
           END OF ty_field_value.

    TYPES ty_field_values TYPE STANDARD TABLE OF ty_field_value WITH EMPTY KEY.

    TYPES: BEGIN OF ty_customize_params,
             table_name   TYPE string,
             field_values TYPE STANDARD TABLE OF ty_field_value WITH EMPTY KEY,
             transport    TYPE trkorr,
             no_transport TYPE abap_bool,
           END OF ty_customize_params.

    TYPES: BEGIN OF ty_customize_result,
             success        TYPE abap_bool,
             command        TYPE string,
             message        TYPE string,
             error          TYPE string,
             table_name     TYPE string,
             action         TYPE string,
             transport      TYPE trkorr,
             delivery_class TYPE string,
           END OF ty_customize_result.

  PRIVATE SECTION.
    "! Returns delivery class (CONTFLAG) from DD02L, or '' if table not found
    METHODS get_delivery_class
      IMPORTING iv_tabname        TYPE string
      RETURNING VALUE(rv_contflag) TYPE char1.

    "! Returns abap_true if table's package requires a transport request
    METHODS is_transport_required
      IMPORTING iv_tabname        TYPE string
      RETURNING VALUE(rv_required) TYPE abap_bool.

    "! Validates transport: exists, modifiable, customizing type. Returns error string or ''.
    METHODS validate_transport
      IMPORTING iv_trkorr        TYPE trkorr
      RETURNING VALUE(rv_error)  TYPE string.

    "! Writes entry to table using RTTI + MODIFY. Returns error string or ''.
    METHODS write_entry
      IMPORTING iv_tabname        TYPE string
                it_field_values   TYPE ty_field_values
      EXPORTING ev_action         TYPE string
      RETURNING VALUE(rv_error)   TYPE string.

ENDCLASS.

CLASS zcl_abgagt_command_customize IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_customize.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA ls_params TYPE ty_customize_params.
    DATA ls_result TYPE ty_customize_result.

    ls_result-command = 'Customize'.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate required inputs
    IF ls_params-table_name IS INITIAL.
      ls_result-error = 'table_name is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.
    IF ls_params-field_values IS INITIAL.
      ls_result-error = 'field_values is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.

    DATA(lv_tabname) = to_upper( ls_params-table_name ).

    " Step 1: Check delivery class — only C and E are allowed
    DATA(lv_delivery_class) = get_delivery_class( lv_tabname ).
    IF lv_delivery_class IS INITIAL.
      ls_result-error = |Table { lv_tabname } not found in ABAP data dictionary|.
      rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.
    ls_result-delivery_class = lv_delivery_class.

    IF lv_delivery_class <> 'C' AND lv_delivery_class <> 'E'.
      ls_result-error = |Table { lv_tabname } is not a customizing table (delivery class: { lv_delivery_class })|.
      rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.

    " Step 2: Check if table's package requires transport
    DATA(lv_transport_required) = is_transport_required( lv_tabname ).
    IF lv_transport_required = abap_true
      AND ls_params-no_transport = abap_false
      AND ls_params-transport IS INITIAL.
      ls_result-error = |Table { lv_tabname } is in a transportable package. Provide --transport <TRKORR> or use --no-transport.|.
      rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.

    " Step 3: Validate transport type if provided
    IF ls_params-transport IS NOT INITIAL.
      DATA(lv_transport_error) = validate_transport( ls_params-transport ).
      IF lv_transport_error IS NOT INITIAL.
        ls_result-error = lv_transport_error.
        rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
        RETURN.
      ENDIF.
    ENDIF.

    " Step 4: Write entry with type validation
    DATA lv_action TYPE string.
    DATA(lv_write_error) = write_entry( EXPORTING iv_tabname      = lv_tabname
                                                  it_field_values = ls_params-field_values
                                        IMPORTING ev_action       = lv_action ).
    IF lv_write_error IS NOT INITIAL.
      ls_result-error = lv_write_error.
      rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.
    ls_result-action = lv_action.

    " Step 5: Record on transport
    IF ls_params-transport IS NOT INITIAL.
      DATA(lo_cts_api) = NEW zcl_abgagt_cts_api( ).
      DATA(lv_add_subrc) = lo_cts_api->zif_abgagt_cts_api~add_to_transport(
        iv_tabname = lv_tabname
        iv_trkorr  = ls_params-transport ).
      IF lv_add_subrc <> 0.
        ls_result-error = |Failed to record table { lv_tabname } on transport { ls_params-transport } (sy-subrc: { lv_add_subrc })|.
        rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
        RETURN.
      ENDIF.
      ls_result-transport = ls_params-transport.
    ENDIF.

    ls_result-success    = abap_true.
    ls_result-table_name = lv_tabname.
    ls_result-message = COND #(
      WHEN ls_params-transport IS NOT INITIAL
        THEN |Customizing entry { ls_result-action } in { lv_tabname } and recorded on transport { ls_params-transport }|
      ELSE
        |Customizing entry { ls_result-action } in { lv_tabname }| ).

    rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

  METHOD get_delivery_class.
    SELECT SINGLE contflag FROM dd02l
      WHERE tabname  = @iv_tabname
        AND as4local = 'A'
      INTO @rv_contflag.
    IF sy-subrc <> 0.
      rv_contflag = ''.
    ENDIF.
  ENDMETHOD.

  METHOD is_transport_required.
    rv_required = abap_false.

    " Get the table's package from TADIR
    SELECT SINGLE devclass FROM tadir
      WHERE pgmid    = 'R3TR'
        AND object   = 'TABL'
        AND obj_name = @iv_tabname
      INTO @DATA(lv_package).
    IF sy-subrc <> 0 OR lv_package IS INITIAL.
      RETURN.
    ENDIF.

    " Local packages (starting with $) never require transport
    IF lv_package(1) = '$'.
      RETURN.
    ENDIF.

    " Check package transport flag
    SELECT SINGLE korrflag FROM tdevc
      WHERE devclass = @lv_package
      INTO @DATA(lv_korrflag).
    IF sy-subrc = 0 AND lv_korrflag = abap_true.
      rv_required = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD validate_transport.
    " Check transport exists and is a modifiable customizing request
    SELECT SINGLE trfunction, trstatus FROM e070
      WHERE trkorr = @iv_trkorr
      INTO @DATA(ls_e070).

    IF sy-subrc <> 0.
      rv_error = |Transport { iv_trkorr } not found|.
      RETURN.
    ENDIF.

    IF ls_e070-trstatus <> 'D'.
      rv_error = |Transport { iv_trkorr } is already released (not modifiable)|.
      RETURN.
    ENDIF.

    IF ls_e070-trfunction <> 'W'.
      rv_error = |Transport { iv_trkorr } is a workbench request. Customizing entries require a customizing transport. Create one with: abapgit-agent transport create --type customizing|.
    ENDIF.
  ENDMETHOD.

  METHOD write_entry.
    ev_action = ''.

    " Get structure descriptor via RTTI
    DATA lo_descr_ref TYPE REF TO cl_abap_typedescr.
    cl_abap_structdescr=>describe_by_name(
      EXPORTING p_name      = iv_tabname
      RECEIVING p_descr_ref = lo_descr_ref
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0.
      rv_error = |Table { iv_tabname } not found in ABAP data dictionary|.
      RETURN.
    ENDIF.

    DATA(lo_strucdescr) = CAST cl_abap_structdescr( lo_descr_ref ).
    DATA(lt_components) = lo_strucdescr->get_components( ).

    " Create dynamic work area
    DATA lr_row TYPE REF TO data.
    CREATE DATA lr_row TYPE (iv_tabname).
    ASSIGN lr_row->* TO FIELD-SYMBOL(<ls_row>).
    CLEAR <ls_row>.

    " Assign each field value with type validation
    LOOP AT it_field_values INTO DATA(ls_fv).
      DATA(lv_field_upper) = to_upper( ls_fv-field ).

      READ TABLE lt_components WITH KEY name = lv_field_upper
        ASSIGNING FIELD-SYMBOL(<comp>).
      IF sy-subrc <> 0.
        rv_error = |Field { lv_field_upper } does not exist in table { iv_tabname }|.
        RETURN.
      ENDIF.

      " Type-specific validation before assignment
      IF <comp>-type IS BOUND.
        DATA(lo_type) = <comp>-type.
        CASE lo_type->type_kind.
          WHEN 'D'.
            IF strlen( ls_fv-value ) <> 8 OR ls_fv-value CN '0123456789'.
              rv_error = |Field { lv_field_upper } has type D (date, format YYYYMMDD), cannot assign '{ ls_fv-value }'|.
              RETURN.
            ENDIF.
          WHEN 'T'.
            IF strlen( ls_fv-value ) <> 6 OR ls_fv-value CN '0123456789'.
              rv_error = |Field { lv_field_upper } has type T (time, format HHMMSS), cannot assign '{ ls_fv-value }'|.
              RETURN.
            ENDIF.
          WHEN 'N'.
            IF ls_fv-value CN '0123456789'.
              rv_error = |Field { lv_field_upper } has type N (numeric char), value must contain only digits|.
              RETURN.
            ENDIF.
          WHEN 'I' OR 'b' OR 's'.
            DATA(lv_trimmed) = condense( ls_fv-value ).
            DATA(lv_check)   = lv_trimmed.
            IF lv_check IS NOT INITIAL AND lv_check(1) = '-'.
              lv_check = lv_check+1.
            ENDIF.
            IF lv_check IS INITIAL OR lv_check CN '0123456789'.
              rv_error = |Field { lv_field_upper } has type I (integer), value must be a whole number|.
              RETURN.
            ENDIF.
          WHEN 'X'.
            IF strlen( ls_fv-value ) MOD 2 <> 0
              OR ls_fv-value CN '0123456789ABCDEFabcdef'.
              rv_error = |Field { lv_field_upper } has type X (hex), value must be even-length hexadecimal (e.g., 1A2B)|.
              RETURN.
            ENDIF.
        ENDCASE.
      ENDIF.

      ASSIGN COMPONENT lv_field_upper OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_field>).
      IF sy-subrc = 0.
        <lv_field> = ls_fv-value.
      ENDIF.
    ENDLOOP.

    " Write using MODIFY (insert-or-update semantics)
    MODIFY (iv_tabname) FROM <ls_row>.
    IF sy-subrc <> 0.
      rv_error = |Failed to write to table { iv_tabname } (sy-subrc: { sy-subrc })|.
      RETURN.
    ENDIF.

    ev_action = 'MODIFIED'.
  ENDMETHOD.

ENDCLASS.
