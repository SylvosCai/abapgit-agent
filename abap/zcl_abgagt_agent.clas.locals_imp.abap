*"* use this source file for the implementation part of your local classes

CLASS lcl_rtti_provider IMPLEMENTATION.
  METHOD zif_abgagt_rtti_provider~describe_object.
    ro_desc = CAST cl_abap_objectdescr(
                cl_abap_typedescr=>describe_by_object_ref( io_object ) ).
  ENDMETHOD.
ENDCLASS.
