"----------------------------------------------------------------------
" Test Doubles - Class Definitions
"----------------------------------------------------------------------

" Simple command stub (does NOT implement progressable)
CLASS lcl_command_stub DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.

" Progressable command stub (DOES implement progressable)
CLASS lcl_progressable_command_stub DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    INTERFACES zif_abgagt_progressable.
ENDCLASS.
