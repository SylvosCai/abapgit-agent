" Test class for unit testing
CLASS zcl_abapgit_agent_test DEFINITION FOR TESTING.

  PRIVATE SECTION.
    DATA: mv_value TYPE i.

  METHODS: setup.

  METHODS: test_initial_value FOR TESTING.
  METHODS: test_set_value FOR TESTING.
  METHODS: test_increment FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_agent_test IMPLEMENTATION.

  METHOD setup.
    mv_value = 0.
  ENDMETHOD.

  METHOD test_initial_value.
    cl_abap_unit_assert=>assert_initial( mv_value ).
  ENDMETHOD.

  METHOD test_set_value.
    mv_value = 42.
    cl_abap_unit_assert=>assert_equals( act = mv_value exp = 42 ).
  ENDMETHOD.

  METHOD test_increment.
    mv_value = mv_value + 1.
    cl_abap_unit_assert=>assert_equals( act = mv_value exp = 1 ).
  ENDMETHOD.

ENDCLASS.
