"&lt;class ZCL_ABAPGIT_AGENT_ODATA
"&gt; Purpose: OData Model Provider for ABAP Git Agent
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

CLASS zcl_abapgit_agent_odata DEFINITION
  PUBLIC
  INHERITING FROM cl_sadl_gtx_standard_mpc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_metadata
        REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      define REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_abapgit_agent_odata IMPLEMENTATION.

  METHOD get_metadata.
    me->mr_model = me->create_model( ).
  ENDMETHOD.

  METHOD define.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property.

    super->define( ).

    " PullCommand Entity
    lo_entity_type = me->get_entity_type( iv_entity_name = 'PullCommand' ).
    IF lo_entity_type IS BOUND.
      " Properties are defined via annotation in DDIC
    ENDIF.

    " JobStatus Entity
    lo_entity_type = me->get_entity_type( iv_entity_name = 'JobStatus' ).
    IF lo_entity_type IS BOUND.
    ENDIF.

    " LogEntry Entity
    lo_entity_type = me->get_entity_type( iv_entity_name = 'LogEntry' ).
    IF lo_entity_type IS BOUND.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
