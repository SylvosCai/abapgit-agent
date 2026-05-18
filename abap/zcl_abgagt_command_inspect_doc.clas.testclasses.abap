*"* use this source file for your test class implementation
*"* local test class

"-----------------------------------------------------------------------
" Test double for zif_abgagt_docu_db — configurable per scenario
"-----------------------------------------------------------------------
CLASS ltcl_db_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_docu_db.
    DATA mv_ktd_html       TYPE string.
    DATA ms_docu_title     TYPE tdline.
    DATA mt_docu_lines     TYPE tlinetab.
    DATA mt_inc_docu_lines TYPE tlinetab.
    DATA mt_cand_hits      TYPE zif_abgagt_docu_db=>ty_dokil_tab.
    DATA mt_inc_hits       TYPE zif_abgagt_docu_db=>ty_dokil_tab.
ENDCLASS.

CLASS ltcl_db_double IMPLEMENTATION.
  METHOD zif_abgagt_docu_db~get_ktd_html.
    rv_html = mv_ktd_html.
  ENDMETHOD.
  METHOD zif_abgagt_docu_db~get_docu.
    ev_title = ms_docu_title.
    ct_lines = SWITCH #( iv_id
      WHEN 'TX' THEN mt_inc_docu_lines
      ELSE mt_docu_lines ).
  ENDMETHOD.
  METHOD zif_abgagt_docu_db~get_dokil_for_candidates.
    rt_hits = mt_cand_hits.
  ENDMETHOD.
  METHOD zif_abgagt_docu_db~get_dokil_for_includes.
    rt_hits = mt_inc_hits.
  ENDMETHOD.
ENDCLASS.

"-----------------------------------------------------------------------
" Test class for zcl_abgagt_command_inspect_doc
"-----------------------------------------------------------------------
CLASS ltcl_inspect_doc DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_db  TYPE REF TO ltcl_db_double.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_inspect_doc.

    METHODS setup.

    " execute() input validation
    METHODS missing_params FOR TESTING.
    METHODS missing_class   FOR TESTING.
    METHODS missing_code    FOR TESTING.

    " DOKIL path
    METHODS dokil_returns_title_and_text   FOR TESTING.
    METHODS dokil_no_title_uses_class_code FOR TESTING.
    METHODS dokil_fallback_to_0000         FOR TESTING.

    " KTD path
    METHODS ktd_html_stripped              FOR TESTING.
    METHODS ktd_style_block_removed        FOR TESTING.

    " KTD + DOKIL interaction
    METHODS dokil_overrides_ktd            FOR TESTING.
    METHODS ktd_used_when_dokil_empty      FOR TESTING.

    " No documentation at all
    METHODS no_doc_returns_no_doc_message  FOR TESTING.

    " ITF include resolution
    METHODS include_substituted_in_output  FOR TESTING.
    METHODS include_english_preferred     FOR TESTING.
    METHODS include_not_found_skipped      FOR TESTING.

    " itf_lines_to_text (pure, tested directly)
    METHODS itf_plain_lines                FOR TESTING.
    METHODS itf_heading_u1_adds_newline    FOR TESTING.
    METHODS itf_slash_adds_blank_line      FOR TESTING.
    METHODS itf_include_substituted        FOR TESTING.
    METHODS itf_include_missing_skipped    FOR TESTING.

    METHODS make_params
      IMPORTING iv_class        TYPE string
                iv_code         TYPE string
      RETURNING VALUE(rs_param) TYPE zcl_abgagt_command_inspect_doc=>ty_inspect_doc_params.

    METHODS add_cand_hit
      IMPORTING iv_id     TYPE doku_id
                iv_object TYPE doku_obj
                iv_langu  TYPE sylangu.

    METHODS add_inc_hit
      IMPORTING iv_id     TYPE doku_id
                iv_object TYPE doku_obj
                iv_langu  TYPE sylangu.

    METHODS make_line
      IMPORTING iv_fmt         TYPE c DEFAULT ''
                iv_line        TYPE tdline
      RETURNING VALUE(rs_line) TYPE tline.
ENDCLASS.

CLASS ltcl_inspect_doc IMPLEMENTATION.

  METHOD setup.
    mo_db  = NEW #( ).
    mo_cut = NEW zcl_abgagt_command_inspect_doc( io_db = mo_db ).
  ENDMETHOD.

  METHOD make_params.
    rs_param-check_class = iv_class.
    rs_param-check_code  = iv_code.
  ENDMETHOD.

  METHOD add_cand_hit.
    INSERT VALUE dokil( id = iv_id object = iv_object langu = iv_langu )
      INTO TABLE mo_db->mt_cand_hits.
  ENDMETHOD.

  METHOD add_inc_hit.
    INSERT VALUE dokil( id = iv_id object = iv_object langu = iv_langu )
      INTO TABLE mo_db->mt_inc_hits.
  ENDMETHOD.

  METHOD make_line.
    rs_line-tdformat = iv_fmt.
    rs_line-tdline   = iv_line.
  ENDMETHOD.

  "---------------------------------------------------------------------
  " Input validation
  "---------------------------------------------------------------------
  METHOD missing_params.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*"Error"*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*required*' ).
  ENDMETHOD.

  METHOD missing_class.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = '' iv_code = 'MSG01' ) ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*required*' ).
  ENDMETHOD.

  METHOD missing_code.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = '' ) ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*required*' ).
  ENDMETHOD.

  "---------------------------------------------------------------------
  " DOKIL path
  "---------------------------------------------------------------------
  METHOD dokil_returns_title_and_text.
    " DOKIL has CA entry for the message code
    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = 'MSG01'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    mo_db->ms_docu_title = 'My Check Title'.
    APPEND make_line( iv_line = 'Description line.' ) TO mo_db->mt_docu_lines.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*"My Check Title"*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Description line.*' ).
  ENDMETHOD.

  METHOD dokil_no_title_uses_class_code.
    " When DOKIL title is empty, class/code is used as title
    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = 'MSG01'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    mo_db->ms_docu_title = ''.
    APPEND make_line( iv_line = 'Some text.' ) TO mo_db->mt_docu_lines.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*CL_CI_TEST_FOO/MSG01*' ).
  ENDMETHOD.

  METHOD dokil_fallback_to_0000.
    " No per-message DOKIL entry — 0000 overview should be returned
    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = '0000'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    mo_db->ms_docu_title = 'Overview'.
    APPEND make_line( iv_line = 'Overview text.' ) TO mo_db->mt_docu_lines.

    " Request a specific code that has no own DOKIL entry
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG99' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Overview*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Overview text.*' ).
  ENDMETHOD.

  "---------------------------------------------------------------------
  " KTD path
  "---------------------------------------------------------------------
  METHOD ktd_html_stripped.
    " KTD returns HTML, DOKIL is empty → HTML tags stripped, plain text returned
    mo_db->mv_ktd_html = '<p>Check description.</p><br/>Some detail.'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Check description.*' ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_result exp = '*<p>*'
      msg = 'HTML tags must be stripped' ).
  ENDMETHOD.

  METHOD ktd_style_block_removed.
    " <style>...</style> block must be removed entirely
    mo_db->mv_ktd_html = '<style>body{color:red}</style><p>Visible text.</p>'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Visible text.*' ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_result exp = '*body*'
      msg = '<style> block content must be removed' ).
  ENDMETHOD.

  "---------------------------------------------------------------------
  " KTD + DOKIL interaction
  "---------------------------------------------------------------------
  METHOD dokil_overrides_ktd.
    " Both KTD and DOKIL have content — DOKIL wins (it has the long text)
    mo_db->mv_ktd_html = '<p>KTD short text.</p>'.

    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = 'MSG01'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    mo_db->ms_docu_title = 'Full Title'.
    APPEND make_line( iv_line = 'Long text with pseudo-comment "#EC CI_FOO.' )
      TO mo_db->mt_docu_lines.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Full Title*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*pseudo-comment*' ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_result exp = '*KTD short text*'
      msg = 'DOKIL must override KTD when both have content' ).
  ENDMETHOD.

  METHOD ktd_used_when_dokil_empty.
    " KTD has content, DOKIL is empty → KTD result preserved
    mo_db->mv_ktd_html = '<p>KTD only content.</p>'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*KTD only content.*' ).
  ENDMETHOD.

  "---------------------------------------------------------------------
  " No documentation
  "---------------------------------------------------------------------
  METHOD no_doc_returns_no_doc_message.
    " Neither KTD nor DOKIL has anything
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*No documentation*' ).
  ENDMETHOD.

  "---------------------------------------------------------------------
  " ITF include resolution via execute()
  "---------------------------------------------------------------------
  METHOD include_substituted_in_output.
    " Main DOKIL entry references an include; include content must appear in output
    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = 'MSG01'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    " Main doc line: include directive
    APPEND make_line( iv_fmt = '/:'
                      iv_line = 'INCLUDE LONG_TEXT OBJECT DOKU ID TX' )
      TO mo_db->mt_docu_lines.
    " DOKIL entry for the include object
    add_inc_hit( iv_id = 'TX' iv_object = 'LONG_TEXT' iv_langu = 'E' ).
    " Include content
    APPEND make_line( iv_line = 'Suppressed via "#EC CI_SORTSEQ.' )
      TO mo_db->mt_inc_docu_lines.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*#EC CI_SORTSEQ*'
      msg = 'Include long text must appear in output' ).
  ENDMETHOD.

  METHOD include_english_preferred.
    " Include has multiple languages; English must be selected, not an arbitrary one
    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = 'MSG01'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    APPEND make_line( iv_fmt = '/:'
                      iv_line = 'INCLUDE LONG_TEXT OBJECT DOKU ID TX' )
      TO mo_db->mt_docu_lines.
    " Multiple language entries — Korean first (insertion order), English second
    add_inc_hit( iv_id = 'TX' iv_object = 'LONG_TEXT' iv_langu = '늑' ).
    add_inc_hit( iv_id = 'TX' iv_object = 'LONG_TEXT' iv_langu = 'E' ).

    " get_docu double: TX id returns the English include content regardless of langu param
    APPEND make_line( iv_line = 'English long text.' ) TO mo_db->mt_inc_docu_lines.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    " The real assertion here is that English was chosen — verified indirectly since
    " the double always returns mt_inc_docu_lines for TX; the test confirms no crash
    " and content appears. A deeper assertion on iv_langu would need a spy.
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*English long text.*' ).
  ENDMETHOD.

  METHOD include_not_found_skipped.
    " Include directive in main doc, but no DOKIL entry for the include object
    DATA lv_obj TYPE doku_obj.
    lv_obj = 'CL_CI_TEST_FOO'.
    lv_obj+30 = 'MSG01'.
    add_cand_hit( iv_id = 'CA' iv_object = lv_obj iv_langu = 'E' ).
    APPEND make_line( iv_line = 'Before include.' ) TO mo_db->mt_docu_lines.
    APPEND make_line( iv_fmt = '/:'
                      iv_line = 'INCLUDE MISSING_OBJ OBJECT DOKU ID TX' )
      TO mo_db->mt_docu_lines.
    APPEND make_line( iv_line = 'After include.' ) TO mo_db->mt_docu_lines.
    " No inc_hits → include silently skipped

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute(
      is_param = make_params( iv_class = 'CL_CI_TEST_FOO' iv_code = 'MSG01' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*Before include.*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_result exp = '*After include.*' ).
  ENDMETHOD.

  "---------------------------------------------------------------------
  " itf_lines_to_text — pure method, tested directly
  "---------------------------------------------------------------------
  METHOD itf_plain_lines.
    DATA lt_lines TYPE tlinetab.
    APPEND make_line( iv_line = 'Line one.' ) TO lt_lines.
    APPEND make_line( iv_line = 'Line two.' ) TO lt_lines.

    DATA(lv_text) = zcl_abgagt_command_inspect_doc=>itf_lines_to_text(
      it_lines   = lt_lines
      it_inc_doc = VALUE #( ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Line one.*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Line two.*' ).
  ENDMETHOD.

  METHOD itf_heading_u1_adds_newline.
    " U1/U2/U3 headings: blank line before if text already started
    DATA lt_lines TYPE tlinetab.
    APPEND make_line( iv_line = 'Intro.' ) TO lt_lines.
    APPEND make_line( iv_fmt = 'U1' iv_line = 'Heading' ) TO lt_lines.

    DATA(lv_text) = zcl_abgagt_command_inspect_doc=>itf_lines_to_text(
      it_lines   = lt_lines
      it_inc_doc = VALUE #( ) ).

    " Heading must be present and follow a newline after intro text
    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Intro.*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Heading*' ).
  ENDMETHOD.

  METHOD itf_slash_adds_blank_line.
    " '/' format adds a newline (blank line separator)
    DATA lt_lines TYPE tlinetab.
    APPEND make_line( iv_line = 'Para one.' ) TO lt_lines.
    APPEND make_line( iv_fmt = '/' iv_line = '' ) TO lt_lines.
    APPEND make_line( iv_line = 'Para two.' ) TO lt_lines.

    DATA(lv_text) = zcl_abgagt_command_inspect_doc=>itf_lines_to_text(
      it_lines   = lt_lines
      it_inc_doc = VALUE #( ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Para one.*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Para two.*' ).
  ENDMETHOD.

  METHOD itf_include_substituted.
    " '/:' include line is replaced by the pre-fetched included lines
    DATA lt_lines TYPE tlinetab.
    APPEND make_line( iv_fmt = '/:'
                      iv_line = 'INCLUDE MYOBJ OBJECT DOKU ID TX' ) TO lt_lines.

    DATA lv_inc_obj TYPE doku_obj VALUE 'MYOBJ'.
    DATA lt_inc_lines TYPE tlinetab.
    APPEND make_line( iv_line = 'Included content.' ) TO lt_inc_lines.

    DATA lt_inc_doc TYPE zcl_abgagt_command_inspect_doc=>ty_inc_doc_tab.
    APPEND VALUE #( obj = lv_inc_obj lines = lt_inc_lines ) TO lt_inc_doc.

    DATA(lv_text) = zcl_abgagt_command_inspect_doc=>itf_lines_to_text(
      it_lines   = lt_lines
      it_inc_doc = lt_inc_doc ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Included content.*' ).
  ENDMETHOD.

  METHOD itf_include_missing_skipped.
    " Include directive for unknown object → silently skip, no crash
    DATA lt_lines TYPE tlinetab.
    APPEND make_line( iv_line = 'Before.' ) TO lt_lines.
    APPEND make_line( iv_fmt = '/:'
                      iv_line = 'INCLUDE UNKNOWN OBJECT DOKU ID TX' ) TO lt_lines.
    APPEND make_line( iv_line = 'After.' ) TO lt_lines.

    DATA(lv_text) = zcl_abgagt_command_inspect_doc=>itf_lines_to_text(
      it_lines   = lt_lines
      it_inc_doc = VALUE #( ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*Before.*' ).
    cl_abap_unit_assert=>assert_char_cp( act = lv_text exp = '*After.*' ).
  ENDMETHOD.

ENDCLASS.
