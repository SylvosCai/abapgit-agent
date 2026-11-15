CLASS ltcl_so10_catalog DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA mo_environment TYPE REF TO if_osql_test_environment.
    CLASS-DATA mo_functions TYPE REF TO if_function_test_environment.
    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS seed IMPORTING iv_count TYPE i.
    METHODS zero_and_overlapping_prefixes FOR TESTING RAISING zcx_abapgit_exception.
    METHODS default_limit_boundary FOR TESTING RAISING zcx_abapgit_exception.
    METHODS upper_limit_boundary FOR TESTING RAISING zcx_abapgit_exception.
    METHODS exact_filters FOR TESTING RAISING zcx_abapgit_exception.
    METHODS literal_underscore FOR TESTING RAISING zcx_abapgit_exception.
    METHODS missing_prefix_rejected FOR TESTING.
    METHODS wildcard_prefix_rejected FOR TESTING.
    METHODS invalid_limit_rejected FOR TESTING.
    METHODS port_rejects_truncated_id FOR TESTING.
    METHODS authorized_headers FOR TESTING RAISING zcx_abapgit_exception.
    METHODS overflow_exposes_no_keys FOR TESTING RAISING zcx_abapgit_exception.
    METHODS denied_header_aborts FOR TESTING.
    METHODS unsupported_storage FOR TESTING.
    METHODS client_admin_denied FOR TESTING.
ENDCLASS.

CLASS ltcl_so10_catalog IMPLEMENTATION.
  METHOD class_setup.
    mo_environment = cl_osql_test_environment=>create( VALUE #( ( 'STXH' ) ( 'T000' ) ) ).
    mo_functions = cl_function_test_environment=>create( VALUE #(
      ( 'CHECK_TEXT_AUTHORITY' ) ( 'AUTHORITY_CHECK' ) ( 'READ_TEXT' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_functions->disable_doubles( ).
    mo_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_environment->clear_doubles( ).
    mo_functions->clear_doubles( ).
    DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
    lt_clients = VALUE #( ( mandt = sy-mandt ) ).
    mo_environment->insert_test_data( lt_clients ).
  ENDMETHOD.

  METHOD seed.
    DATA lt_headers TYPE STANDARD TABLE OF stxh WITH EMPTY KEY.
    lt_headers = VALUE #( FOR n = 1 UNTIL n > iv_count
      ( mandt = sy-mandt tdobject = 'TEXT' tdname = |ZAPP_{ n WIDTH = 4 ALIGN = RIGHT PAD = '0' NUMBER = RAW }| tdid = 'ST' tdspras = 'E' ) ).
    mo_environment->insert_test_data( lt_headers ).
  ENDMETHOD.

  METHOD zero_and_overlapping_prefixes.
    DATA(ls_empty) = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_empty-at_least exp = 0 ).
    seed( 3 ).
    DATA(ls_result) = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ( `ZAPP_0` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( ls_result-keys ) exp = 3 ).
    cl_abap_unit_assert=>assert_false( ls_result-has_more ).
  ENDMETHOD.

  METHOD default_limit_boundary.
    seed( 100 ).
    DATA(ls_result) = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( ls_result-keys ) exp = 100 ).
    cl_abap_unit_assert=>assert_false( ls_result-has_more ).
    mo_environment->clear_doubles( ).
    seed( 101 ).
    ls_result = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-at_least exp = 101 ).
    cl_abap_unit_assert=>assert_true( ls_result-has_more ).
  ENDMETHOD.

  METHOD upper_limit_boundary.
    seed( 1000 ).
    DATA(ls_result) = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) iv_max_hits = 1000 ).
    cl_abap_unit_assert=>assert_false( ls_result-has_more ).
    mo_environment->clear_doubles( ).
    seed( 1001 ).
    ls_result = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) iv_max_hits = 1000 ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-at_least exp = 1001 ).
    cl_abap_unit_assert=>assert_true( ls_result-has_more ).
  ENDMETHOD.

  METHOD exact_filters.
    seed( 3 ).
    DATA(ls_result) = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) )
      iv_name = 'ZAPP_0002' iv_id = 'ST' iv_language = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = lines( ls_result-keys ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-keys[ 1 ]-name exp = 'ZAPP_0002' ).
    ls_result = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) iv_language = 'D' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-at_least exp = 0 ).
  ENDMETHOD.

  METHOD literal_underscore.
    DATA lt_headers TYPE STANDARD TABLE OF stxh WITH EMPTY KEY.
    lt_headers = VALUE #( ( mandt = sy-mandt tdobject = 'TEXT' tdname = 'ZAPPX0001' tdid = 'ST' tdspras = 'E' ) ).
    mo_environment->insert_test_data( lt_headers ).
    DATA(ls_result) = zcl_abgagt_so10_catalog=>search(
      iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-at_least exp = 0 ).
  ENDMETHOD.

  METHOD authorized_headers.
    TEST-INJECTION storage_mode.
      lv_default = abap_true.
    END-TEST-INJECTION.
    seed( 3 ).
    DATA(lo_catalog) = CAST zif_abgagt_so10_catalog( NEW zcl_abgagt_so10_catalog( ) ).
    DATA(ls_result) = lo_catalog->search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( ls_result-keys ) exp = 3 ).
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->verify( )->is_called_times( 3 ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.

  METHOD overflow_exposes_no_keys.
    TEST-INJECTION storage_mode.
      lv_default = abap_true.
    END-TEST-INJECTION.
    seed( 101 ).
    DATA(lo_catalog) = CAST zif_abgagt_so10_catalog( NEW zcl_abgagt_so10_catalog( ) ).
    DATA(ls_result) = lo_catalog->search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
    cl_abap_unit_assert=>assert_true( ls_result-has_more ).
    cl_abap_unit_assert=>assert_initial( ls_result-keys ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-at_least exp = 101 ).
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->verify( )->is_called_times( 101 ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.

  METHOD denied_header_aborts.
    TEST-INJECTION storage_mode.
      lv_default = abap_true.
    END-TEST-INJECTION.
    seed( 1 ).
    DATA(lo_auth) = mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' ).
    lo_auth->configure_call( )->when( lo_auth->create_input_configuration( ) )->then_raise_classic_exception( 'NO_AUTHORITY' ).
    DATA(lo_catalog) = CAST zif_abgagt_so10_catalog( NEW zcl_abgagt_so10_catalog( ) ).
    TRY.
        lo_catalog->search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
        cl_abap_unit_assert=>fail( 'A denied header must abort listing' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'SHOW authorization denied' ) ).
    ENDTRY.
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.

  METHOD unsupported_storage.
    TEST-INJECTION storage_mode.
      lv_default = abap_false.
      lv_upgrade = abap_true.
    END-TEST-INJECTION.
    DATA(lo_catalog) = CAST zif_abgagt_so10_catalog( NEW zcl_abgagt_so10_catalog( ) ).
    TRY.
        lo_catalog->search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) ) ).
        cl_abap_unit_assert=>fail( 'Unsupported storage must fail before listing' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'standard database storage' ) ).
    ENDTRY.
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.

  METHOD client_admin_denied.
    DATA(lv_other) = CONV sy-mandt( COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ) ).
    DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
    lt_clients = VALUE #( ( mandt = lv_other ) ).
    mo_environment->insert_test_data( lt_clients ).
    DATA(lo_auth) = mo_functions->get_double( 'AUTHORITY_CHECK' ).
    lo_auth->configure_call( )->when( lo_auth->create_input_configuration( ) )->then_raise_classic_exception( 'USER_NOT_AUTHORIZED' ).
    TRY.
        zcl_abgagt_so10_catalog=>data_client( CONV #( lv_other ) ).
        cl_abap_unit_assert=>fail( 'Other data client requires administrative authorization' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'authorization denied' ) ).
    ENDTRY.
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->verify( )->is_never_called( ).
  ENDMETHOD.

  METHOD missing_prefix_rejected.
    TRY.
        zcl_abgagt_so10_catalog=>search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ) ).
        cl_abap_unit_assert=>fail( 'Missing prefix must not list every text' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'prefix' ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD invalid_limit_rejected.
    TRY.
        zcl_abgagt_so10_catalog=>search( iv_data_client = CONV #( sy-mandt )
          it_prefixes = VALUE #( ( `ZAPP_` ) ) iv_max_hits = 1001 ).
        cl_abap_unit_assert=>fail( 'Unbounded discovery must not be allowed' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'max-hits' ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD port_rejects_truncated_id.
    DATA(lo_catalog) = CAST zif_abgagt_so10_catalog( NEW zcl_abgagt_so10_catalog( ) ).
    TRY.
        lo_catalog->search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `ZAPP_` ) )
          iv_id = 'ST_BAD' ).
        cl_abap_unit_assert=>fail( 'Reject overlong ID before native handler lookup or truncation' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'Text ID' ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD wildcard_prefix_rejected.
    TRY.
        zcl_abgagt_so10_catalog=>search( iv_data_client = CONV #( sy-mandt ) it_prefixes = VALUE #( ( `Z*` ) ) ).
        cl_abap_unit_assert=>fail( 'Prefix cannot contain a wildcard' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'literal' ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
