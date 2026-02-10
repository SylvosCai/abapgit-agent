*----------------------------------------------------------------------*
*       CLASS ltcl_util DEFINITION
*----------------------------------------------------------------------*
CLASS ltcl_util DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS setup.
    DATA mo_util TYPE REF TO zcl_abgagt_util.

    METHODS parse_class_file FOR TESTING.
    METHODS parse_interface_file FOR TESTING.
    METHODS parse_program_file FOR TESTING.
    METHODS parse_file_with_path FOR TESTING.
    METHODS parse_file_with_leading_slash FOR TESTING.
    METHODS parse_invalid_file FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_util IMPLEMENTATION.

  METHOD setup.
    mo_util = zcl_abgagt_util=>get_instance( ).
  ENDMETHOD.

  METHOD parse_class_file.
    DATA lv_file TYPE string VALUE 'zcl_my_class.clas.abap'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name
      exp = 'ZCL_MY_CLASS'
      msg = 'Object name should be ZCL_MY_CLASS' ).
  ENDMETHOD.

  METHOD parse_interface_file.
    DATA lv_file TYPE string VALUE 'zif_my_interface.intf.abap'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'INTF'
      msg = 'Object type should be INTF' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name
      exp = 'ZIF_MY_INTERFACE'
      msg = 'Object name should be ZIF_MY_INTERFACE' ).
  ENDMETHOD.

  METHOD parse_program_file.
    DATA lv_file TYPE string VALUE 'zmy_report.prog.abap'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'PROG'
      msg = 'Object type should be PROG' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name
      exp = 'ZMY_REPORT'
      msg = 'Object name should be ZMY_REPORT' ).
  ENDMETHOD.

  METHOD parse_file_with_path.
    DATA lv_file TYPE string VALUE 'src/zcl_my_class.clas.abap'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name
      exp = 'ZCL_MY_CLASS'
      msg = 'Object name should be ZCL_MY_CLASS' ).
  ENDMETHOD.

  METHOD parse_file_with_leading_slash.
    DATA lv_file TYPE string VALUE '/src/zcl_my_class.clas.abap'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name
      exp = 'ZCL_MY_CLASS'
      msg = 'Object name should be ZCL_MY_CLASS' ).
  ENDMETHOD.

  METHOD parse_invalid_file.
    DATA lv_file TYPE string VALUE 'zcl_my_class.clas.txt'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_type
      msg = 'Object type should be initial for invalid file' ).
    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_name
      msg = 'Object name should be initial for invalid file' ).
  ENDMETHOD.

ENDCLASS.
