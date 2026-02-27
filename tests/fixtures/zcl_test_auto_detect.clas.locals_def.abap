CLASS lcl_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS multiply
      IMPORTING iv_a TYPE i
                iv_b TYPE i
      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.
