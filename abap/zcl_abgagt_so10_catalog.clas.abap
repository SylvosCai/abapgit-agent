CLASS zcl_abgagt_so10_catalog DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_catalog.
    TYPES ty_keys TYPE STANDARD TABLE OF zif_abgagt_so10_api=>ty_key WITH EMPTY KEY.
    TYPES: BEGIN OF ty_result,
             keys TYPE ty_keys,
             max_hits TYPE i,
             at_least TYPE i,
             has_more TYPE abap_bool,
           END OF ty_result.
    CLASS-METHODS data_client IMPORTING iv_client TYPE string
      RETURNING VALUE(rv_client) TYPE sy-mandt RAISING zcx_abapgit_exception.
    " The raw bounded query is internal to the authorized provider below.
    CLASS-METHODS search
      IMPORTING iv_data_client TYPE string it_prefixes TYPE string_table
                iv_name TYPE string OPTIONAL iv_id TYPE string OPTIONAL iv_language TYPE string OPTIONAL
                iv_max_hits TYPE i DEFAULT 100
      RETURNING VALUE(rs_result) TYPE ty_result RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abgagt_so10_catalog IMPLEMENTATION.
  METHOD zif_abgagt_so10_catalog~search.
    IF strlen( iv_id ) > 4.
      zcx_abapgit_exception=>raise( 'Invalid SO10 discovery Text ID' ).
    ENDIF.
    DATA(lv_client) = data_client( iv_data_client ).
    DATA lv_special TYPE abap_bool.
    DATA lv_upgrade TYPE abap_bool.
    DATA(lv_default) = abap_false.
    TEST-SEAM storage_mode.
      cl_rstx_table_view=>init( IMPORTING e_use_special_tables = lv_special e_on_upgrade_server = lv_upgrade ).
      lv_default = cl_rstxt_persistence_manager=>is_only_default_handler_used(
        object = CONV tdobject( 'TEXT' ) id = CONV tdid( iv_id ) ).
    END-TEST-SEAM.
    IF lv_special = abap_true OR lv_upgrade = abap_true OR lv_default = abap_false.
      zcx_abapgit_exception=>raise( 'SO10 discovery requires standard database storage without upgrade overlays' ).
    ENDIF.
    rs_result = CORRESPONDING #( search( iv_data_client = CONV #( lv_client ) it_prefixes = it_prefixes
      iv_name = iv_name iv_id = iv_id iv_language = iv_language iv_max_hits = iv_max_hits ) ).
    LOOP AT rs_result-keys INTO DATA(ls_key).
      CALL FUNCTION 'CHECK_TEXT_AUTHORITY'
        EXPORTING activity = 'SHOW' id = CONV tdid( ls_key-id ) language = CONV spras( ls_key-language )
                  name = CONV thead-tdname( ls_key-name ) object = CONV tdobject( ls_key-object )
        EXCEPTIONS no_authority = 1 OTHERS = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'SO10 discovery SHOW authorization denied' ).
      ENDIF.
    ENDLOOP.
    IF rs_result-has_more = abap_true.
      CLEAR rs_result-keys.
    ENDIF.
  ENDMETHOD.

  METHOD data_client.
    IF iv_client IS INITIAL.
      rv_client = sy-mandt.
    ELSE.
      FIND PCRE `\A[0-9]{3}\z` IN iv_client.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'SO10 data client must contain exactly three digits' ).
      ENDIF.
      rv_client = iv_client.
    ENDIF.
    SELECT SINGLE mandt FROM t000 WHERE mandt = @rv_client INTO @DATA(lv_configured).
    IF sy-subrc <> 0 OR lv_configured IS INITIAL.
      zcx_abapgit_exception=>raise( 'SO10 data client is not configured' ).
    ENDIF.
    IF rv_client <> sy-mandt.
      CALL FUNCTION 'AUTHORITY_CHECK'
        EXPORTING user = sy-uname object = 'S_TABU_CLI' field1 = 'CLIIDMAINT' value1 = 'X'
        EXCEPTIONS user_is_authorized = 1 OTHERS = 2.
      IF sy-subrc <> 1.
        zcx_abapgit_exception=>raise( 'Cross-client SO10 administrative authorization denied' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD search.
    IF strlen( iv_data_client ) <> 3 OR iv_data_client CN '0123456789'
       OR iv_max_hits < 1 OR iv_max_hits > 1000 OR it_prefixes IS INITIAL.
      zcx_abapgit_exception=>raise( 'SO10 discovery requires a data client, literal prefix and max-hits between 1 and 1000' ).
    ENDIF.
    DATA lr_prefixes TYPE RANGE OF stxh-tdname.
    LOOP AT it_prefixes INTO DATA(lv_prefix).
      IF lv_prefix IS INITIAL OR strlen( lv_prefix ) > 70.
        zcx_abapgit_exception=>raise( 'SO10 discovery prefix must be nonempty and at most 70 characters' ).
      ENDIF.
      FIND PCRE `[\x00-\x1f\x7f*+?#%]` IN lv_prefix.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'SO10 discovery prefixes must be literal, not wildcard patterns' ).
      ENDIF.
      FIND PCRE `\S` IN lv_prefix.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'SO10 discovery prefix cannot be blank' ).
      ENDIF.
      lr_prefixes = VALUE #( BASE lr_prefixes ( sign = 'I'
        option = COND #( WHEN strlen( lv_prefix ) = 70 THEN 'EQ' ELSE 'CP' )
        low = COND #( WHEN strlen( lv_prefix ) = 70 THEN lv_prefix ELSE |{ lv_prefix }*| ) ) ).
    ENDLOOP.
    IF strlen( iv_name ) > 70 OR strlen( iv_id ) > 4 OR strlen( iv_language ) > 1
       OR iv_id CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'
       OR iv_language CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.
      zcx_abapgit_exception=>raise( 'Invalid exact SO10 discovery filter' ).
    ENDIF.
    FIND PCRE `[\x00-\x1f\x7f*]` IN iv_name.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( 'SO10 exact-name filter cannot contain controls or wildcards' ).
    ENDIF.
    DATA(lv_native_name) = CONV thead-tdname( iv_name ).
    IF strlen( lv_native_name ) <> strlen( iv_name ).
      zcx_abapgit_exception=>raise( 'SO10 exact-name filter cannot contain trailing name blanks' ).
    ENDIF.
    DATA lr_names TYPE RANGE OF stxh-tdname.
    DATA lr_ids TYPE RANGE OF stxh-tdid.
    DATA lr_languages TYPE RANGE OF stxh-tdspras.
    IF iv_name IS NOT INITIAL.
      lr_names = VALUE #( ( sign = 'I' option = 'EQ' low = iv_name ) ).
    ENDIF.
    IF iv_id IS NOT INITIAL.
      lr_ids = VALUE #( ( sign = 'I' option = 'EQ' low = iv_id ) ).
    ENDIF.
    IF iv_language IS NOT INITIAL.
      lr_languages = VALUE #( ( sign = 'I' option = 'EQ' low = iv_language ) ).
    ENDIF.
    " abaplint does not count the USING CLIENT reference as a variable use.
    DATA(lv_client) = CONV sy-mandt( iv_data_client ) ##NEEDED.
    DATA(lv_rows) = iv_max_hits + 1.
    SELECT FROM stxh USING CLIENT @lv_client
      FIELDS tdobject, tdname, tdid, tdspras
      WHERE tdobject = 'TEXT' AND tdname IN @lr_prefixes AND tdname IN @lr_names
        AND tdid IN @lr_ids AND tdspras IN @lr_languages
      ORDER BY tdobject, tdname, tdid, tdspras
      INTO TABLE @DATA(lt_headers) UP TO @lv_rows ROWS.
    rs_result-max_hits = iv_max_hits.
    rs_result-at_least = lines( lt_headers ).
    rs_result-has_more = xsdbool( rs_result-at_least > iv_max_hits ).
    " Keep bounded candidates internal so the service can check SHOW before rejecting overflow.
    rs_result-keys = VALUE #( FOR ls_header IN lt_headers
      ( object = ls_header-tdobject name = ls_header-tdname id = ls_header-tdid language = ls_header-tdspras ) ).
  ENDMETHOD.
ENDCLASS.
