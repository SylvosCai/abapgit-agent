INTERFACE zif_abgagt_docu_db PUBLIC.
  TYPES ty_dokil_tab TYPE SORTED TABLE OF dokil WITH NON-UNIQUE KEY object id.

  TYPES: BEGIN OF ty_cand,
           id     TYPE doku_id,
           object TYPE doku_obj,
         END OF ty_cand.
  TYPES ty_cand_tab TYPE STANDARD TABLE OF ty_cand WITH DEFAULT KEY.

  METHODS get_ktd_html
    IMPORTING iv_class       TYPE sci_chk
              iv_code        TYPE satc_ci_message_code
    RETURNING VALUE(rv_html) TYPE string.

  METHODS get_docu
    IMPORTING iv_id       TYPE doku_id
              iv_object   TYPE doku_obj
              iv_langu    TYPE sylangu
    EXPORTING ev_title    TYPE tdline
    CHANGING  ct_lines    TYPE tlinetab.

  METHODS get_dokil_for_candidates
    IMPORTING it_cands         TYPE ty_cand_tab
    RETURNING VALUE(rt_hits)   TYPE ty_dokil_tab.

  METHODS get_dokil_for_includes
    IMPORTING it_objects        TYPE STANDARD TABLE  "#EC NOTYP
    RETURNING VALUE(rt_hits)    TYPE ty_dokil_tab.
ENDINTERFACE.
