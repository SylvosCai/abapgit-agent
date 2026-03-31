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
    DATA ls_map TYPE ty_command_map.
    ls_map-command = zif_abgagt_command=>gc_pull.    ls_map-class_name = 'ZCL_ABGAGT_COMMAND_PULL'.    APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_inspect. ls_map-class_name = 'ZCL_ABGAGT_COMMAND_INSPECT'. APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_unit.    ls_map-class_name = 'ZCL_ABGAGT_COMMAND_UNIT'.    APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_create.  ls_map-class_name = 'ZCL_ABGAGT_COMMAND_CREATE'.  APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_delete.  ls_map-class_name = 'ZCL_ABGAGT_COMMAND_DELETE'.  APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_status.  ls_map-class_name = 'ZCL_ABGAGT_COMMAND_STATUS'.  APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_import.  ls_map-class_name = 'ZCL_ABGAGT_COMMAND_IMPORT'.  APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_tree.    ls_map-class_name = 'ZCL_ABGAGT_COMMAND_TREE'.    APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_view.    ls_map-class_name = 'ZCL_ABGAGT_COMMAND_VIEW'.    APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_preview. ls_map-class_name = 'ZCL_ABGAGT_COMMAND_PREVIEW'. APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_list.    ls_map-class_name = 'ZCL_ABGAGT_COMMAND_LIST'.    APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_where.   ls_map-class_name = 'ZCL_ABGAGT_COMMAND_WHERE'.   APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_syntax.  ls_map-class_name = 'ZCL_ABGAGT_COMMAND_SYNTAX'.  APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_dump.    ls_map-class_name = 'ZCL_ABGAGT_COMMAND_DUMP'.    APPEND ls_map TO mt_command_map.
    ls_map-command = zif_abgagt_command=>gc_transport. ls_map-class_name = 'ZCL_ABGAGT_COMMAND_TRANSPORT'. APPEND ls_map TO mt_command_map.
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
