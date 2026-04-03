"! <p class="shorttext synchronized">Code Inspector Interface for ABAP Git Agent</p>
"! Interface for dependency injection to enable testing with mocks
INTERFACE zif_abgagt_code_inspector PUBLIC.

  TYPES ty_object_keys TYPE TABLE OF scir_objs WITH NON-UNIQUE DEFAULT KEY.

  "! Create object set for inspection
  "! @parameter iv_name | Object set name
  "! @parameter it_objects | List of object keys to inspect
  "! @parameter ro_objset | Created object set
  "! @raising cx_static_check | On creation failure
  METHODS create_object_set
    IMPORTING iv_name     TYPE sci_objs
              it_objects  TYPE ty_object_keys
    RETURNING VALUE(ro_objset) TYPE REF TO cl_ci_objectset
    RAISING   cx_static_check.

  "! Get check variant reference
  "! @parameter iv_variant | Variant name
  "! @parameter ro_variant | Check variant reference (unbound if not found)
  "! @raising cx_static_check | On lookup failure
  METHODS get_check_variant
    IMPORTING iv_variant TYPE string
    RETURNING VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant
    RAISING   cx_static_check.

  "! Create and run an inspection
  "! @parameter iv_name | Inspection name
  "! @parameter io_variant | Check variant to use
  "! @parameter io_objset | Object set to inspect
  "! @parameter ro_inspection | Created and executed inspection
  "! @raising cx_static_check | On inspection failure
  METHODS create_and_run_inspection
    IMPORTING iv_name     TYPE sci_objs
              io_variant  TYPE REF TO cl_ci_checkvariant
              io_objset   TYPE REF TO cl_ci_objectset
    RETURNING VALUE(ro_inspection) TYPE REF TO cl_ci_inspection
    RAISING   cx_static_check.

  "! Get plain list results from a completed inspection
  "! @parameter io_inspection | Completed inspection instance
  "! @parameter rt_list | Flat list of all inspection findings
  METHODS get_results
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
    RETURNING VALUE(rt_list) TYPE scit_alvlist.

  "! Cleanup inspection and object set resources
  "! @parameter io_inspection | Inspection to clean up
  "! @parameter io_objset | Object set to clean up
  METHODS cleanup
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
              io_objset     TYPE REF TO cl_ci_objectset.

ENDINTERFACE.
