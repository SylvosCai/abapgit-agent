*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

"-----------------------------------------------------------------------
" Production implementation — declaration needed before class body
"-----------------------------------------------------------------------
CLASS lcl_docu_db DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_docu_db.
ENDCLASS.
