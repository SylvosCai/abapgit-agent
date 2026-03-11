*"* use this source file for your ABAP unit test classes

"----------------------------------------------------------------------
" RTTI provider — real implementation; wraps the static call so tests
" can inject a mock via the zif_abgagt_rtti_provider interface.
"----------------------------------------------------------------------
CLASS lcl_rtti_provider DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_rtti_provider.
ENDCLASS.
