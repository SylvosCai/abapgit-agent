" Unit test for syntax agent
CLASS zcl_ai_syntax_agent_test DEFINITION PUBLIC FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS setup.
    METHODS: test_initial_value FOR TESTING,
      test_string_concat FOR TESTING.

ENDCLASS.

CLASS zcl_ai_syntax_agent_test IMPLEMENTATION.

  METHOD setup.
    DATA lv_init TYPE i.
    lv_init = 0.
  ENDMETHOD.

  METHOD test_initial_value.
    DATA lv_value TYPE i.
    cl_abap_unit_assert=>assert_initial( lv_value ).
  ENDMETHOD.

  METHOD test_string_concat.
    DATA lv_result TYPE string.
    CONCATENATE 'Hello' 'World' INTO lv_result SEPARATED BY space.
    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'Hello World' ).
  ENDMETHOD.

ENDCLASS.
