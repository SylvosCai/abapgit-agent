CLASS ltcl_source_file_policy DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_policy TYPE REF TO zif_abgagt_source_file_policy.
    METHODS setup.
    METHODS accepts_existing_extensions FOR TESTING.
    METHODS accepts_native_sources      FOR TESTING RAISING zcx_abapgit_exception.
    METHODS accepts_legacy_source       FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_xml_only            FOR TESTING.
    METHODS rejects_duplicate           FOR TESTING.
    METHODS rejects_malformed           FOR TESTING.
ENDCLASS.

CLASS ltcl_source_file_policy IMPLEMENTATION.
  METHOD setup.
    mo_policy = NEW zcl_abgagt_source_file_policy( ).
  ENDMETHOD.

  METHOD accepts_existing_extensions.
    cl_abap_unit_assert=>assert_true( mo_policy->accepts_source_extension( '.ABAP' ) ).
    cl_abap_unit_assert=>assert_true( mo_policy->accepts_source_extension( '.ASDDLS' ) ).
    cl_abap_unit_assert=>assert_true( mo_policy->accepts_source_extension( '.XML' ) ).
  ENDMETHOD.

  METHOD accepts_native_sources.
    mo_policy->validate_remote( VALUE #(
      ( filename = 'zrap_bdef.bdef.asbdef' )
      ( filename = 'zrap_bdef.bdef.xml' )
      ( filename = 'zrap_srvd.srvd.srvdsrv' )
      ( filename = 'zrap_ddlx.ddlx.asddlxs' ) ) ).
  ENDMETHOD.

  METHOD accepts_legacy_source.
    mo_policy->validate_remote(
      VALUE #( ( filename = 'zrap_srvd.srvd.assrvd' ) ) ).
  ENDMETHOD.

  METHOD rejects_xml_only.
    TRY.
        mo_policy->validate_local(
          it_files = VALUE #(
            ( item-obj_type = 'BDEF' item-obj_name = 'ZRAP_BDEF'
              file-filename = 'zrap_bdef.bdef.xml' ) )
          it_objects = VALUE #( ( obj_type = 'BDEF' obj_name = 'ZRAP_BDEF' ) ) ).
        cl_abap_unit_assert=>fail( 'XML-only strict object must be rejected' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( ) exp = '*requires exactly one native source*' ).
    ENDTRY.
  ENDMETHOD.

  METHOD rejects_duplicate.
    TRY.
        mo_policy->validate_remote( VALUE #(
          ( filename = 'zrap_srvd.srvd.srvdsrv' )
          ( filename = 'zrap_srvd.srvd.assrvd' ) ) ).
        cl_abap_unit_assert=>fail( 'Duplicate strict sources must be rejected' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( ) exp = '*found 2*' ).
    ENDTRY.
  ENDMETHOD.

  METHOD rejects_malformed.
    TRY.
        mo_policy->validate_remote( VALUE #( ( filename = 'broken.bdef.srvdsrv' ) ) ).
        cl_abap_unit_assert=>fail( 'Malformed strict filename must be rejected' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( ) exp = '*Malformed source file*' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
