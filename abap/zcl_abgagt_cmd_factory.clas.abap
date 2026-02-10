CLASS zcl_abgagt_cmd_factory DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_factory) TYPE REF TO zif_abgagt_cmd_factory.
  PRIVATE SECTION.
    METHODS constructor.
    TYPES: BEGIN OF ty_command_map,
             command TYPE string,
             class_name TYPE string,
           END OF ty_command_map.
    DATA mt_command_map TYPE TABLE OF ty_command_map.
ENDCLASS.

CLASS zcl_abgagt_cmd_factory IMPLEMENTATION.
  METHOD get_instance.
    DATA lo_factory TYPE REF TO zcl_abgagt_cmd_factory.
    CREATE OBJECT lo_factory.
    ro_factory = lo_factory.
  ENDMETHOD.

  METHOD constructor.
    " Build command to class name mapping
    mt_command_map = VALUE #(
      ( command = zif_abgagt_command=>gc_pull     class_name = 'ZCL_ABGAGT_COMMAND_PULL' )
      ( command = zif_abgagt_command=>gc_inspect class_name = 'ZCL_ABGAGT_COMMAND_INSPECT' )
      ( command = zif_abgagt_command=>gc_unit   class_name = 'ZCL_ABGAGT_COMMAND_UNIT' )
    ).
  ENDMETHOD.

  METHOD zif_abgagt_cmd_factory~get_command.
    " Find class name for command
    READ TABLE mt_command_map WITH KEY command = iv_command
      ASSIGNING FIELD-SYMBOL(<ls_map>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Create command instance dynamically
    CREATE OBJECT ro_command TYPE (<ls_map>-class_name).
  ENDMETHOD.

ENDCLASS.
