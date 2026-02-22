"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Code Inspector Interface for ABAP Git Agent</p>
"! Interface for dependency injection to enable testing with mocks
INTERFACE zif_abgagt_code_inspector PUBLIC.

  " Object key structure (same as in command class)
  TYPES ty_object_keys TYPE TABLE OF scir_objs WITH NON-UNIQUE DEFAULT KEY.

  " Create object set for inspection
  METHODS create_object_set
    IMPORTING iv_name     TYPE sci_objs
              it_objects  TYPE ty_object_keys
    RETURNING VALUE(ro_objset) TYPE REF TO cl_ci_objectset.

  " Get check variant reference (returns empty if not found)
  METHODS get_check_variant
    IMPORTING iv_variant TYPE string
    RETURNING VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant.

  " Create and run inspection
  METHODS create_and_run_inspection
    IMPORTING iv_name     TYPE sci_objs
              io_variant  TYPE REF TO cl_ci_checkvariant
              io_objset   TYPE REF TO cl_ci_objectset
    RETURNING VALUE(ro_inspection) TYPE REF TO cl_ci_inspection.

  " Get plain list results from inspection
  METHODS get_results
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
    RETURNING VALUE(rt_list) TYPE scit_alvlist.

  " Cleanup inspection and object set
  METHODS cleanup
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
              io_objset     TYPE REF TO cl_ci_objectset.

ENDINTERFACE.
