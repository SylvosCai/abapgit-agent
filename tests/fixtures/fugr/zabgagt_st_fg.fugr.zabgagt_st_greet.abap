FUNCTION zabgagt_st_greet.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_NAME) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_GREETING) TYPE  STRING
*"----------------------------------------------------------------------

  ev_greeting = |Hello { iv_name }|.

ENDFUNCTION.
