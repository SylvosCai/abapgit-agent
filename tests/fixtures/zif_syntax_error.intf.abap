INTERFACE zif_syntax_error PUBLIC.
  " Intentional syntax error - unknown type
  DATA mv_test TYPE zunknown_type.

  " Missing period after method declaration
  METHODS test_method

  " Invalid syntax
  UNKNOWN_KEYWORD something.
ENDINTERFACE.
