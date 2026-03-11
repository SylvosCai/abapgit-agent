*"* use this source file for your ABAP unit test classes

"----------------------------------------------------------------------
" RTTI provider — wraps the static cl_abap_typedescr call so tests can
" inject a mock that returns a controlled cl_abap_objectdescr.
"----------------------------------------------------------------------
INTERFACE lif_rtti_provider.
  METHODS describe_object
    IMPORTING io_object      TYPE REF TO object
    RETURNING VALUE(ro_desc) TYPE REF TO cl_abap_objectdescr.
ENDINTERFACE.

CLASS lcl_rtti_provider DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_rtti_provider.
ENDCLASS.
