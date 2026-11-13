INTERFACE zif_abgagt_source_compare PUBLIC.
  TYPES: BEGIN OF ty_source,
           comparable_source TYPE string,
           baseline_candidates TYPE string_table,
         END OF ty_source.

  METHODS prepare
    IMPORTING iv_obj_type TYPE tadir-object iv_source TYPE string
    RETURNING VALUE(rs_source) TYPE ty_source
    RAISING zcx_abapgit_exception.
ENDINTERFACE.
