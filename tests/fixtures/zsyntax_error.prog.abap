REPORT zsyntax_error_prog.

" Intentional syntax error - unknown statement
UNKNOWN_STATEMENT this_will_fail.

" Missing period
DATA lv_test TYPE string

" Undefined variable
lv_undefined = 'test'.

" Invalid WRITE statement
WRITE / lv_nonexistent.
