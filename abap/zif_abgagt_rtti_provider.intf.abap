"! <p class="shorttext synchronized">RTTI Provider Interface</p>
INTERFACE zif_abgagt_rtti_provider PUBLIC.

  "! Get ABAP object descriptor for a live object instance
  "! @parameter io_object | Object instance to describe
  "! @parameter ro_desc | Object type descriptor
  METHODS describe_object
    IMPORTING io_object      TYPE REF TO object
    RETURNING VALUE(ro_desc) TYPE REF TO cl_abap_objectdescr.

  "! Get ABAP class descriptor by class name
  "! @parameter iv_class_name | Fully qualified class name
  "! @parameter ro_desc | Class type descriptor
  METHODS describe_class
    IMPORTING iv_class_name  TYPE string
    RETURNING VALUE(ro_desc) TYPE REF TO cl_abap_classdescr.

ENDINTERFACE.
