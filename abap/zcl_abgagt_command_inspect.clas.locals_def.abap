CLASS lcl_ddl_handler_default DEFINITION FINAL.
  " Default DDL handler wrapper that uses real SAP DDL handler
  PUBLIC SECTION.
    INTERFACES zif_abgagt_ddl_handler.

  PRIVATE SECTION.
    DATA mt_warnings TYPE zif_abgagt_ddl_handler=>ty_warnings.
ENDCLASS.
