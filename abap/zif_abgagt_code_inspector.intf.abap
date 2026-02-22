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

  " Get check variant reference
  " @parameter iv_user | User (empty for global variants)
  " @parameter iv_name | Variant name
  " @raising cx_static_check | If variant not found
  METHODS get_variant_ref
    IMPORTING iv_user     TYPE syuname DEFAULT ''
              iv_name     TYPE sci_chkv
    RETURNING VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant
    RAISING   cx_static_check.

  " Create inspection object
  METHODS create_inspection
    IMPORTING iv_user     TYPE syuname
              iv_name     TYPE sci_objs
    RETURNING VALUE(ro_inspection) TYPE REF TO cl_ci_inspection.

  " Run inspection
  " @raising cx_static_check | If run fails
  METHODS run_inspection
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
    RAISING   cx_static_check.

  " Get plain list results from inspection
  METHODS get_results
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
    RETURNING VALUE(rt_list) TYPE scit_alvlist.

  " Cleanup inspection and object set
  METHODS cleanup
    IMPORTING io_inspection TYPE REF TO cl_ci_inspection
              io_objset     TYPE REF TO cl_ci_objectset.

ENDINTERFACE.
