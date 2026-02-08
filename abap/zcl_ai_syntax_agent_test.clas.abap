" Unit test for syntax agent
CLASS zcl_ai_syntax_agent_test DEFINITION FOR TESTING PUBLIC.

  PRIVATE SECTION.
    DATA: mv_value TYPE i.

    METHODS: setup.

    METHODS: test_initial_value FOR TESTING.
    METHODS: test_string_concat FOR TESTING.

ENDCLASS.

CLASS zcl_ai_syntax_agent_test IMPLEMENTATION.

  METHOD setup.
    mv_value = 0.
  ENDMETHOD.

  METHOD test_initial_value.
    cl_abap_unit_assert=>assert_initial( mv_value ).
  ENDMETHOD.

  METHOD test_string_concat.
    DATA: lv_result TYPE string.
    CONCATENATE 'Hello' 'World' INTO lv_result SEPARATED BY space.
    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'Hello World' ).
  ENDMETHOD.

ENDCLASS.
