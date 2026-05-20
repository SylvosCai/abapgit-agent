"! <p class="shorttext synchronized">ABAP Git Agent: Run AUnit + Coverage</p>
"! Backend command for the abapgit-agent CLI `unit` command.
"! Runs ABAP unit tests with optional statement-coverage measurement using the
"! standard SAP_BASIS APIs (CL_AUNIT_FACTORY, CL_AUCV_TASK, CL_SCV_*).
"! Returns per-method test results AND per-method coverage as JSON.
CLASS zcl_abgagt_command_unit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    INTERFACES if_aunit_listener.

    " Request payload: list of test source files + coverage flag.
    TYPES:
      BEGIN OF ts_unit_params,
        files    TYPE string_table,
        coverage TYPE abap_bool,
      END OF ts_unit_params.

    " Single failure stack frame.
    TYPES:
      BEGIN OF ts_stack_entry,
        name TYPE string,
        line TYPE i,
      END OF ts_stack_entry,
      tt_stack TYPE STANDARD TABLE OF ts_stack_entry WITH EMPTY KEY.

    " Single test method outcome.
    TYPES:
      BEGIN OF ts_method_result,
        program         TYPE string,        "owning class pool, e.g. CL_FOO===========CP
        test_class_name TYPE string,
        name            TYPE string,
        passed          TYPE abap_bool,
        execution_time  TYPE decfloat16,
        kind            TYPE string,        "failedAssertion / cxFailure / rtFailure
        title           TYPE string,        "header description first line
        details         TYPE string_table,  "extra description lines
        stack           TYPE tt_stack,
      END OF ts_method_result,
      tt_method_results TYPE STANDARD TABLE OF ts_method_result WITH EMPTY KEY.

    " Per-method statement coverage entry.
    TYPES:
      BEGIN OF ts_method_coverage,
        name                TYPE string,
        total_statements    TYPE i,
        executed_statements TYPE i,
        rate                TYPE decfloat16,
      END OF ts_method_coverage,
      tt_method_coverage TYPE STANDARD TABLE OF ts_method_coverage WITH EMPTY KEY.

    " Coverage summary for one production class.
    TYPES:
      BEGIN OF ts_class_coverage,
        total_statements    TYPE i,
        executed_statements TYPE i,
        statement_rate      TYPE decfloat16,
        methods             TYPE tt_method_coverage,
      END OF ts_class_coverage.

    " Result for one production class (one entry in the response array).
    TYPES:
      BEGIN OF ts_class_result,
        class_name       TYPE seoclsname,
        file             TYPE string,
        methods          TYPE tt_method_results,
        coverage_present TYPE abap_bool,
        coverage         TYPE ts_class_coverage,
      END OF ts_class_result,
      tt_class_results TYPE STANDARD TABLE OF ts_class_result WITH EMPTY KEY.

    " Top-level response (`{ "results": [...] }`).
    TYPES:
      BEGIN OF ts_response,
        results TYPE tt_class_results,
      END OF ts_response.

    TYPES tt_class_names TYPE STANDARD TABLE OF seoclsname WITH EMPTY KEY.

  PRIVATE SECTION.
    "! Map a file path like `abap/zcl_foo.clas.testclasses.abap` to `ZCL_FOO`.
    "! Returns an empty class name if the file is not a CLAS source/include.
    METHODS class_name_from_file
      IMPORTING iv_file         TYPE string
      RETURNING VALUE(rv_class) TYPE seoclsname.

    "! Build the deduplicated list of (class_name, originating_file) pairs.
    METHODS resolve_classes
      IMPORTING it_files          TYPE string_table
      RETURNING VALUE(rt_results) TYPE tt_class_results.

    " Run AUnit on the given classes (optionally with coverage).
    " Returns the listener (containing per-method results) and the coverage
    " measurement (when iv_coverage = abap_true). Either reference may be
    " null if the run failed early or coverage was not enabled.
    TYPES:
      BEGIN OF ts_run_result,
        listener    TYPE REF TO zcl_abgagt_command_unit,
        measurement TYPE REF TO if_scv_measurement,
      END OF ts_run_result.

    METHODS run_tests
      IMPORTING it_classes     TYPE tt_class_names
                iv_coverage    TYPE abap_bool
      RETURNING VALUE(rs_run)  TYPE ts_run_result.

    " Walk the SCV result tree and append per-class coverage to results.
    METHODS attach_coverage
      IMPORTING io_root           TYPE REF TO if_scv_result_node
      CHANGING  ct_class_results  TYPE tt_class_results.

    " Collect the listener's recorded test results into the per-class entries.
    METHODS attach_test_results
      IMPORTING io_listener       TYPE REF TO zcl_abgagt_command_unit
      CHANGING  ct_class_results  TYPE tt_class_results.

    " Listener buffer — populated during AUnit run, drained by attach_test_results.
    DATA:
      mt_listener_results TYPE tt_method_results,
      mv_current_program  TYPE string,
      mv_current_class    TYPE string,
      mv_current_idx      TYPE i,
      mv_current_start    TYPE timestampl.

    " Helper — render a description struct (id + params) as a single string.
    CLASS-METHODS describe
      IMPORTING is_desc          TYPE if_aunit_text_description=>ty_s_description
      RETURNING VALUE(rv_string) TYPE string.

    "! Extract a clean program/class name from a stack description.
    "! Params look like ['ZCL_FOO===========CCAU', '61', 'METHOD']; we return
    "! the part of params[0] before the first '=' (or the whole thing if none),
    "! e.g. 'ZCL_FOO'. Falls back to describe() output if extraction fails.
    CLASS-METHODS extract_stack_name
      IMPORTING is_desc        TYPE if_aunit_text_description=>ty_s_description
      RETURNING VALUE(rv_name) TYPE string.

    "! Try parse 'PROGRAM_INCLUDE LINE METHOD' style stack to extract a line number.
    CLASS-METHODS extract_line
      IMPORTING is_desc        TYPE if_aunit_text_description=>ty_s_description
      RETURNING VALUE(rv_line) TYPE i.

ENDCLASS.

CLASS zcl_abgagt_command_unit IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_unit.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params   TYPE ts_unit_params,
          ls_response TYPE ts_response.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-files IS INITIAL.
      ls_response-results = VALUE #( ).
      rv_result = /ui2/cl_json=>serialize( data         = ls_response
                                           pretty_name  = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.

    " 1) Resolve files → classes (deduplicated)
    DATA(lt_class_results) = resolve_classes( ls_params-files ).
    IF lt_class_results IS INITIAL.
      ls_response-results = VALUE #( ).
      rv_result = /ui2/cl_json=>serialize( data         = ls_response
                                           pretty_name  = /ui2/cl_json=>pretty_mode-low_case ).
      RETURN.
    ENDIF.

    " 2) Build the class list for the AUnit task
    DATA: lt_class_names TYPE tt_class_names.
    LOOP AT lt_class_results INTO DATA(ls_cr).
      APPEND ls_cr-class_name TO lt_class_names.
    ENDLOOP.

    " 3) Run tests + collect coverage
    DATA(ls_run) = run_tests( it_classes  = lt_class_names
                              iv_coverage = ls_params-coverage ).
    DATA(lo_listener)    = ls_run-listener.
    DATA(lo_measurement) = ls_run-measurement.

    " 4) Attach test method results from listener
    attach_test_results( EXPORTING io_listener      = lo_listener
                         CHANGING  ct_class_results = lt_class_results ).

    " 5) Attach coverage if measurement was created
    IF ls_params-coverage = abap_true AND lo_measurement IS BOUND.
      DATA: lo_factory   TYPE REF TO if_scv_factory,
            lo_selection TYPE REF TO if_scv_selection,
            lo_result    TYPE REF TO if_scv_result.

      lo_factory   = cl_scv_coverage_api=>get_factory( ).
      lo_selection = lo_factory->create_selection( ).

      LOOP AT lt_class_names INTO DATA(lv_clsname).
        DATA(lv_pool) = cl_oo_classname_service=>get_classpool_name( lv_clsname ).
        lo_selection->include_program( i_name = lv_pool ).
      ENDLOOP.

      TRY.
          lo_result = lo_measurement->build_selection_result(
                        i_result_name = 'ABAPGIT_AGENT_UNIT_RESULT'
                        i_selection   = lo_selection ).
          attach_coverage( EXPORTING io_root          = lo_result->get_root_node( )
                           CHANGING  ct_class_results = lt_class_results ).
        CATCH cx_scv_call_error cx_scv_execution_error ##NO_HANDLER.
          " Coverage unavailable — leave coverage_present=false on each class entry.
      ENDTRY.
    ENDIF.

    ls_response-results = lt_class_results.
    rv_result = /ui2/cl_json=>serialize( data         = ls_response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

  METHOD class_name_from_file.
    " Use the standard util to parse <path>/<name>.clas{.testclasses}.abap → CLAS, <NAME>
    DATA(lo_util) = zcl_abgagt_util=>get_instance( ).
    DATA: lv_obj_type TYPE string,
          lv_obj_name TYPE string.
    lo_util->parse_file_to_object(
      EXPORTING iv_file     = iv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).
    IF lv_obj_type = 'CLAS' AND lv_obj_name IS NOT INITIAL.
      rv_class = lv_obj_name.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_classes.
    DATA: lt_seen TYPE STANDARD TABLE OF seoclsname WITH EMPTY KEY.
    LOOP AT it_files INTO DATA(lv_file).
      DATA(lv_class) = class_name_from_file( lv_file ).
      CHECK lv_class IS NOT INITIAL.
      " Dedup
      READ TABLE lt_seen WITH KEY table_line = lv_class TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND lv_class TO lt_seen.
        APPEND VALUE #( class_name = lv_class
                        file       = lv_file ) TO rt_results.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_tests.
    " Build TADIR object directory for all target classes
    DATA: lt_object_dir TYPE cl_aucv_task=>ty_object_directory_elements.
    LOOP AT it_classes INTO DATA(lv_class).
      INSERT VALUE #( object   = 'CLAS'
                      obj_name = lv_class ) INTO TABLE lt_object_dir.
    ENDLOOP.

    " Build the listener (use a fresh instance — same class implements IF_AUNIT_LISTENER)
    rs_run-listener = NEW #( ).

    " Create coverage-enabled task
    DATA(lo_task) = cl_aucv_task=>create(
                      i_listener         = rs_run-listener
                      i_measure_coverage = iv_coverage ).

    DATA(lv_added) = lo_task->add_associated_unit_tests( lt_object_dir ).
    IF lv_added = 0.
      RETURN.
    ENDIF.

    TRY.
        lo_task->run( i_mode = if_aunit_task=>c_run_mode-external ).
      CATCH cx_dynamic_check ##NO_HANDLER.
    ENDTRY.

    IF iv_coverage = abap_true.
      TRY.
          rs_run-measurement = lo_task->get_coverage_measurement( ).
        CATCH cx_scv_execution_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD attach_test_results.
    " Build a class-pool → class-name lookup once. The listener captures the
    " program (class pool) name in program_start; we use that to attribute
    " each method result to the correct production class — independent of
    " the local test class naming convention (LTC_, LTCL_, LT_, ...).
    DATA: BEGIN OF ls_pool_class,
            pool_name  TYPE string,
            class_name TYPE seoclsname,
          END OF ls_pool_class,
          lt_pool_class LIKE STANDARD TABLE OF ls_pool_class.

    LOOP AT ct_class_results INTO DATA(ls_seed).
      ls_pool_class-pool_name  = cl_oo_classname_service=>get_classpool_name( ls_seed-class_name ).
      ls_pool_class-class_name = ls_seed-class_name.
      APPEND ls_pool_class TO lt_pool_class.
    ENDLOOP.

    LOOP AT io_listener->mt_listener_results INTO DATA(ls_method).
      DATA: lv_match_class TYPE seoclsname.
      CLEAR lv_match_class.

      " Primary path: look up via the captured program name.
      IF ls_method-program IS NOT INITIAL.
        READ TABLE lt_pool_class INTO ls_pool_class WITH KEY pool_name = ls_method-program.
        IF sy-subrc = 0.
          lv_match_class = ls_pool_class-class_name.
        ENDIF.
      ENDIF.

      " Fallback: substring match against the test class name (legacy convention).
      IF lv_match_class IS INITIAL.
        DATA(lv_test_upper) = ls_method-test_class_name.
        TRANSLATE lv_test_upper TO UPPER CASE.
        LOOP AT ct_class_results ASSIGNING FIELD-SYMBOL(<ls_lookup>).
          IF lv_test_upper CS <ls_lookup>-class_name.
            lv_match_class = <ls_lookup>-class_name.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_match_class IS INITIAL.
        " Couldn't attribute — drop the result; better than corrupting an
        " unrelated class entry. (Could also be logged as a warning later.)
        CONTINUE.
      ENDIF.

      LOOP AT ct_class_results ASSIGNING FIELD-SYMBOL(<ls_cr>) WHERE class_name = lv_match_class.
        APPEND ls_method TO <ls_cr>-methods.
        EXIT.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD attach_coverage.
    " Walk the SCV result tree depth-first using an explicit frame stack so we
    " can carry the current "program pool → class" context down to each block
    " (method) descendant.

    " 1) Build a pool→class lookup once
    DATA: BEGIN OF ls_pool_class,
            pool_name  TYPE string,
            class_name TYPE seoclsname,
          END OF ls_pool_class,
          lt_pool_class LIKE STANDARD TABLE OF ls_pool_class.

    LOOP AT ct_class_results INTO DATA(ls_seed).
      ls_pool_class-pool_name  = cl_oo_classname_service=>get_classpool_name( ls_seed-class_name ).
      ls_pool_class-class_name = ls_seed-class_name.
      APPEND ls_pool_class TO lt_pool_class.
    ENDLOOP.

    " 2) Frame stack — track the active program pool / class as we descend
    DATA: BEGIN OF ls_frame,
            node       TYPE REF TO if_scv_result_node,
            pool_name  TYPE string,
            class_name TYPE seoclsname,
          END OF ls_frame,
          lt_frames LIKE STANDARD TABLE OF ls_frame.

    APPEND VALUE #( node       = io_root
                    pool_name  = ``
                    class_name = '' ) TO lt_frames.

    " 3) DFS
    WHILE lines( lt_frames ) > 0.
      DATA(lv_idx) = lines( lt_frames ).
      READ TABLE lt_frames INDEX lv_idx INTO ls_frame.
      DELETE lt_frames INDEX lv_idx.

      IF ls_frame-node IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(lo_node)    = ls_frame-node.
      DATA(lv_pool)    = ls_frame-pool_name.
      DATA(lv_clsname) = ls_frame-class_name.

      " Update context if we just entered a program pool
      IF lo_node->type = ce_scv_node_type=>program.
        lv_pool = lo_node->name.
        READ TABLE lt_pool_class INTO ls_pool_class WITH KEY pool_name = lv_pool.
        IF sy-subrc = 0.
          lv_clsname = ls_pool_class-class_name.
        ELSE.
          CLEAR lv_clsname.
        ENDIF.
      ENDIF.

      " Block (method) under a known class → record coverage
      IF lo_node->type = ce_scv_node_type=>block AND lv_clsname IS NOT INITIAL.
        DATA(lo_stmt) = lo_node->get_coverage( ce_scv_coverage_type=>statement ).
        IF lo_stmt IS BOUND.
          DATA(lv_total) = lo_stmt->get_total( ).
          DATA(lv_exec)  = lo_stmt->get_executed( ).

          DATA(ls_mc) = VALUE ts_method_coverage(
                         name                = lo_node->name
                         total_statements    = lv_total
                         executed_statements = lv_exec
                         rate                = COND #( WHEN lv_total > 0
                                                       THEN ( lv_exec * 100 ) / lv_total
                                                       ELSE 0 ) ).

          LOOP AT ct_class_results ASSIGNING FIELD-SYMBOL(<ls_cr>) WHERE class_name = lv_clsname.
            <ls_cr>-coverage_present = abap_true.
            APPEND ls_mc TO <ls_cr>-coverage-methods.
            <ls_cr>-coverage-total_statements    += lv_total.
            <ls_cr>-coverage-executed_statements += lv_exec.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

      " Push children
      DATA(lt_children) = lo_node->get_children( ).
      LOOP AT lt_children INTO DATA(lo_child).
        APPEND VALUE #( node       = lo_child
                        pool_name  = lv_pool
                        class_name = lv_clsname ) TO lt_frames.
      ENDLOOP.
    ENDWHILE.

    " 4) Compute per-class statement_rate
    LOOP AT ct_class_results ASSIGNING FIELD-SYMBOL(<ls_cr2>) WHERE coverage_present = abap_true.
      IF <ls_cr2>-coverage-total_statements > 0.
        <ls_cr2>-coverage-statement_rate =
          ( <ls_cr2>-coverage-executed_statements * 100 ) / <ls_cr2>-coverage-total_statements.
      ELSE.
        <ls_cr2>-coverage-statement_rate = 0.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD describe.
    " Use the standard AUnit text renderer to get the user-readable string
    " from a (id, params) description tuple. Falls back to "[id] params"
    " if rendering fails.
    TRY.
        DATA(lo_renderer) = cl_aunit_text_description=>get_instance( ).
        rv_string = lo_renderer->get_string( is_desc ).
        IF rv_string IS INITIAL.
          rv_string = is_desc-id.
        ENDIF.
      CATCH cx_root.
        DATA(lv_text) = is_desc-id.
        DATA: lv_params TYPE string.
        LOOP AT is_desc-params INTO DATA(lv_p).
          IF lv_params IS INITIAL.
            lv_params = lv_p.
          ELSE.
            lv_params = |{ lv_params } { lv_p }|.
          ENDIF.
        ENDLOOP.
        rv_string = |[{ lv_text }] { lv_params }|.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_line.
    " Stack params look like: 'PROGRAM_INCLUDE LINE METHOD' — second param is the line.
    DATA(lt_p) = is_desc-params.
    READ TABLE lt_p INDEX 2 INTO DATA(lv_line_str).
    IF sy-subrc = 0 AND lv_line_str CO '0123456789'.
      rv_line = lv_line_str.
    ENDIF.
  ENDMETHOD.

  METHOD extract_stack_name.
    DATA(lt_p) = is_desc-params.
    READ TABLE lt_p INDEX 1 INTO DATA(lv_first).
    IF sy-subrc = 0 AND lv_first IS NOT INITIAL.
      DATA(lv_pos) = find( val = lv_first sub = '=' ).
      IF lv_pos > 0.
        rv_name = lv_first(lv_pos).
      ELSE.
        rv_name = lv_first.
      ENDIF.
    ELSE.
      rv_name = describe( is_desc ).
    ENDIF.
  ENDMETHOD.

  "------------------------------------------------------------------*
  " IF_AUNIT_LISTENER implementation
  "------------------------------------------------------------------*

  METHOD if_aunit_listener~task_start.
  ENDMETHOD.

  METHOD if_aunit_listener~task_end.
  ENDMETHOD.

  METHOD if_aunit_listener~class_start.
    mv_current_class = info->name.
  ENDMETHOD.

  METHOD if_aunit_listener~class_end.
  ENDMETHOD.

  METHOD if_aunit_listener~program_start.
    mv_current_program = info->name.
  ENDMETHOD.

  METHOD if_aunit_listener~program_end.
  ENDMETHOD.

  METHOD if_aunit_listener~method_start.
    DATA: ls_row TYPE ts_method_result.
    ls_row-program         = mv_current_program.
    ls_row-test_class_name = mv_current_class.
    ls_row-name            = info->name.
    ls_row-passed          = abap_true.
    APPEND ls_row TO mt_listener_results.
    mv_current_idx = lines( mt_listener_results ).
    mv_current_start = COND #( WHEN occurred_on IS SUPPLIED THEN occurred_on ELSE 0 ).
  ENDMETHOD.

  METHOD if_aunit_listener~method_end.
    IF mv_current_idx > 0 AND mv_current_idx <= lines( mt_listener_results ).
      ASSIGN mt_listener_results[ mv_current_idx ] TO FIELD-SYMBOL(<ls_row>).
      DATA(lv_end) = COND timestampl( WHEN occurred_on IS SUPPLIED THEN occurred_on ELSE 0 ).
      IF lv_end IS NOT INITIAL AND mv_current_start IS NOT INITIAL.
        <ls_row>-execution_time = lv_end - mv_current_start.
      ENDIF.
    ENDIF.
    CLEAR: mv_current_idx, mv_current_start.
  ENDMETHOD.

  METHOD if_aunit_listener~assert_failure.
    IF mv_current_idx > 0 AND mv_current_idx <= lines( mt_listener_results ).
      ASSIGN mt_listener_results[ mv_current_idx ] TO FIELD-SYMBOL(<ls_row>).
      <ls_row>-passed = abap_false.
      <ls_row>-kind   = 'failedAssertion'.
      <ls_row>-title  = describe( failure->get_header_description( ) ).
      " Detail lines: analysis description (e.g. "Expected [7] Actual [13]")
      DATA(lt_analysis) = failure->get_analysis_description( ).
      LOOP AT lt_analysis INTO DATA(ls_ad).
        APPEND describe( ls_ad ) TO <ls_row>-details.
      ENDLOOP.
      DATA(lt_stack_desc) = failure->get_stack_description( ).
      LOOP AT lt_stack_desc INTO DATA(ls_sd).
        APPEND VALUE #( name = extract_stack_name( ls_sd )
                        line = extract_line( ls_sd ) ) TO <ls_row>-stack.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD if_aunit_listener~cx_failure.
    IF mv_current_idx > 0 AND mv_current_idx <= lines( mt_listener_results ).
      ASSIGN mt_listener_results[ mv_current_idx ] TO FIELD-SYMBOL(<ls_row>).
      <ls_row>-passed = abap_false.
      <ls_row>-kind   = 'cxFailure'.
      <ls_row>-title  = describe( failure->get_header_description( ) ).
      DATA(lt_analysis) = failure->get_analysis_description( ).
      LOOP AT lt_analysis INTO DATA(ls_ad).
        APPEND describe( ls_ad ) TO <ls_row>-details.
      ENDLOOP.
      DATA(lt_stack_desc) = failure->get_stack_description( ).
      LOOP AT lt_stack_desc INTO DATA(ls_sd).
        APPEND VALUE #( name = extract_stack_name( ls_sd )
                        line = extract_line( ls_sd ) ) TO <ls_row>-stack.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD if_aunit_listener~rt_failure.
    IF mv_current_idx > 0 AND mv_current_idx <= lines( mt_listener_results ).
      ASSIGN mt_listener_results[ mv_current_idx ] TO FIELD-SYMBOL(<ls_row>).
      <ls_row>-passed = abap_false.
      <ls_row>-kind   = 'rtFailure'.
      <ls_row>-title  = describe( failure->get_header_description( ) ).
      DATA(lt_analysis) = failure->get_analysis_description( ).
      LOOP AT lt_analysis INTO DATA(ls_ad).
        APPEND describe( ls_ad ) TO <ls_row>-details.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD if_aunit_listener~warning.
  ENDMETHOD.

  METHOD if_aunit_listener~execution_event.
  ENDMETHOD.

ENDCLASS.
