INTERFACE zif_abgagt_rtti_provider PUBLIC.
  METHODS describe_object
    IMPORTING io_object      TYPE REF TO object
    RETURNING VALUE(ro_desc) TYPE REF TO cl_abap_objectdescr.
  METHODS describe_class
    IMPORTING iv_class_name  TYPE string
    RETURNING VALUE(ro_desc) TYPE REF TO cl_abap_classdescr.
ENDINTERFACE.
