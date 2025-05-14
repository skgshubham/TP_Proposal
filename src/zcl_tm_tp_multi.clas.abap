class ZCL_TM_TP_MULTI definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF lty_sol_amount,
             sol_id     TYPE /scmtms/vsr_opt_int,
             amount     TYPE /scmtms/amount,
             new_sol_id TYPE /scmtms/vsr_opt_int,
           END OF lty_sol_amount .
  types:
    lt_sol_amount TYPE TABLE OF lty_sol_amount .
  types LS_SOL_AMOUNT type LTY_SOL_AMOUNT .

  class-data GV_TP_MULTI type FLAG .

  methods TP_MULTI
    importing
      !IO_METHPAR type ref to /SCTM/CL_METH_PARAMETER
      !IT_REQUEST type /SCTM/TT_REQUEST .
  class-methods POST_SYNCUP_DATA
    importing
      !IV_UPDATE_TBO_NODES type BOOLE_D optional
      !IO_CHANGE type ref to /BOBF/IF_TRA_CHANGE
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE .
  class-methods RECREATE_RANKLIST
    importing
      !IT_MAP_SOL type /SCMTMS/T_PLN_MAP_SOL_BUF_VAR
    exporting
      !ET_RANKLIST type LT_SOL_AMOUNT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TM_TP_MULTI IMPLEMENTATION.


  METHOD post_syncup_data.

    DATA: mo_controller TYPE REF TO /bofu/if_fbi_controller_new.

     mo_controller ?= /bofu/cl_fbi_controller_base=>get_instance( ).

    IF iv_update_tbo_nodes = abap_true AND io_change IS BOUND.
      " the call of this method is needed in case the service manager is used to execute the respective
      /scmtms/cl_pln_tbo_factory=>update_tbo_from_changenot( EXPORTING io_change        = io_change
                                                             IMPORTING eo_change_tr_tbo = DATA(lo_chg_tbo) ).
      io_change->merge( io_change = lo_chg_tbo ).
    ENDIF.

    IF io_change IS BOUND OR io_message IS BOUND.
      mo_controller->/bofu/if_fbi_controller~post_syncup_data( iv_bo_key  = /scmtms/if_pln_c=>sc_bo_key
                                                               io_change  = io_change
                                                               io_message = io_message ).
    ENDIF.
  ENDMETHOD.


  METHOD recreate_ranklist.


    DATA: lv_current_variant     TYPE /scmtms/buffer_variant,
          lo_srvmgr_tor          TYPE REF TO /bobf/if_tra_service_manager,
          ls_key                 TYPE /bobf/s_frw_key,
          lt_key                 TYPE /bobf/t_frw_key,
          lt_tor_root            TYPE /scmtms/t_tor_root_k,
          lt_ranklist            TYPE /scmtms/t_tor_rl_k,
          lo_result              TYPE REF TO /scmtms/cl_pc_result,
          ls_sol_amount          TYPE ls_sol_amount,
          lt_sol_amount          TYPE lt_sol_amount,
          lt_no_display_stop_key TYPE /bobf/t_frw_key,
          lv_new_sol_id          TYPE /scmtms/vsr_opt_int VALUE 1.

    CLEAR et_ranklist.
    CHECK it_map_sol IS NOT INITIAL.
*--------------------------------------------------------------------*
* Current buffer variant
*--------------------------------------------------------------------*
    CALL METHOD /scmtms/cl_bufvar_helper=>get_buffer_variant
      IMPORTING
        ev_buffer_variant = lv_current_variant.
*--------------------------------------------------------------------*
* Service manager for TOR, Location
*--------------------------------------------------------------------*
    lo_srvmgr_tor = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).

*------------------------------------------------------------------------------
* Loop at solutions
*------------------------------------------------------------------------------
    LOOP AT it_map_sol INTO DATA(ls_map_sol).
*--------------------------------------------------------------------*
* Set buffer variant complete or routing
*--------------------------------------------------------------------*
      IF NOT ls_map_sol-buffer_complete_flag IS INITIAL.
        CALL METHOD /scmtms/cl_bufvar_helper=>set_buffer_variant
          EXPORTING
            iv_target_buffer_variant = ls_map_sol-buffer_complete.
        lo_result = ls_map_sol-result_complete.
      ELSE.
        IF NOT ls_map_sol-buffer_routing_flag IS INITIAL.
          CALL METHOD /scmtms/cl_bufvar_helper=>set_buffer_variant
            EXPORTING
              iv_target_buffer_variant = ls_map_sol-buffer_routing.
          lo_result = ls_map_sol-result_routing.
        ELSE.
          CONTINUE. "no solution
        ENDIF.
      ENDIF.

      CLEAR lt_no_display_stop_key.
      LOOP AT lo_result->mt_reqstg_input ASSIGNING FIELD-SYMBOL(<ls_reqstg>) WHERE stage_planned = /scmtms/if_tor_const=>sc_succ_planning_status-planned.
        ls_key-key = <ls_reqstg>-start_stop_key.
        INSERT ls_key INTO TABLE  lt_no_display_stop_key.
        ls_key-key = <ls_reqstg>-end_stop_key.
        INSERT ls_key INTO TABLE  lt_no_display_stop_key.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lt_no_display_stop_key USING KEY key_sort.
*--------------------------------------------------------------------*
* TOR stop to prepare determination of Freight Units
*--------------------------------------------------------------------*
      CLEAR: lt_key.
      LOOP AT lo_result->mt_tor_stop INTO DATA(ls_tor_stop).
        READ TABLE lt_no_display_stop_key WITH KEY key_sort COMPONENTS key = ls_tor_stop-key TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ls_key-key = ls_tor_stop-root_key.
          INSERT ls_key INTO TABLE lt_key.
        ENDIF.
      ENDLOOP.

*--------------------------------------------------------------------*
* DSO Handling
*--------------------------------------------------------------------*
      LOOP AT lo_result->mt_tor_shp
           ASSIGNING FIELD-SYMBOL(<ls_tor_shp>)
           WHERE selected = /scmtms/if_tor_const=>sc_dir_shp_optsel.
* freight units should be displayed for DSO case as well
        ls_key-key = <ls_tor_shp>-root_key.
        INSERT ls_key INTO TABLE lt_key.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lt_key USING KEY key_sort.
*--------------------------------------------------------------------*
* TOR Roots
*--------------------------------------------------------------------*
      CALL METHOD lo_srvmgr_tor->retrieve               "#EC CI_INTF_LOOP
        EXPORTING                                        "BOPF in loop needed because of buffer variant use
          iv_node_key  = /scmtms/if_tor_c=>sc_node-root
          it_key       = lt_key
          iv_fill_data = abap_true
        IMPORTING
          et_data      = lt_tor_root.
*--------------------------------------------------------------------*
* Read ranking list of TORs
*--------------------------------------------------------------------*
      CALL METHOD lo_srvmgr_tor->retrieve_by_association "#EC CI_INTF_LOOP
        EXPORTING                                         "BOPF in loop needed because of buffer variant use
          iv_node_key    = /scmtms/if_tor_c=>sc_node-root
          it_key         = lt_key
          iv_association = /scmtms/if_tor_c=>sc_association-root-rankinglist
          iv_fill_data   = abap_true
        IMPORTING
          et_data        = lt_ranklist.


* sum the amount of ranking_number eq to 1
      LOOP AT lt_ranklist ASSIGNING FIELD-SYMBOL(<lfs_ranklist>) WHERE ranking_number = 1.
        IF <lfs_ranklist>-amount IS NOT INITIAL.
          ls_sol_amount-amount = ls_sol_amount-amount + <lfs_ranklist>-amount.
        ELSE.
          ls_sol_amount-amount = 99999.
        ENDIF.
      ENDLOOP.

      ls_sol_amount-sol_id = ls_map_sol-sol_id.
* if amount is not availalbe then set the default amount value as 99999
      IF ls_sol_amount-amount IS INITIAL.
        ls_sol_amount-amount = 99999.
      ENDIF.
      APPEND ls_sol_amount TO lt_sol_amount.
      CLEAR: ls_sol_amount, lt_key, ls_key, lt_ranklist, lt_tor_root, lt_no_display_stop_key.
    ENDLOOP. "solution

* sort the lt_sol_amount in ascending order based on amount
    SORT lt_sol_amount BY amount ASCENDING.

* update the new sol id field value
    LOOP AT lt_sol_amount ASSIGNING FIELD-SYMBOL(<lfs_sol_amount>).
      <lfs_sol_amount>-new_sol_id = lv_new_sol_id.
      lv_new_sol_id = lv_new_sol_id + 1.
    ENDLOOP.

    et_ranklist = lt_sol_amount[].

  ENDMETHOD.


  METHOD tp_multi.

    DATA: lr_par_optimize       TYPE REF TO /scmtms/s_pln_a_optimize_par,
          lo_cus_req            TYPE REF TO /scmtms/cl_cus_request,
          lo_message_opt_status TYPE REF TO /bobf/if_frw_message,
          lo_controller         TYPE REF TO /bofu/if_fbi_controller_new,
          lo_controller_frwfd   TYPE REF TO /scmtms/if_ui_controller_frwfd,
          lo_controller_appfd   TYPE REF TO /scmtms/if_ui_controller_appfd,
          lv_current_tp_layout  TYPE /scmtms/ui_layout_id,
          ev_result             TYPE fpm_event_result,
          lt_map_sol            TYPE TABLE OF /scmtms/s_pln_map_sol_buf_var,
          lt_keys               TYPE /bobf/t_frw_key,
          lr_parameters         TYPE REF TO data,
          lr_par_accept         TYPE REF TO /scmtms/s_pln_a_accept_par,
          lv_solution_id        TYPE /scmtms/vsr_opt_int VALUE 1,
          lt_comp_changes_tab   TYPE STANDARD TABLE OF REF TO /bobf/if_tra_change.

*   get Instance of UI Controller
    lo_controller       ?= /bofu/cl_fbi_controller_base=>get_instance( ).
* copy the instance of UI Controller to controller of Cockpit Framework UI feeder class
    lo_controller_frwfd ?= lo_controller.
* copy the instance of UI Controller to controller of transportation cockpit application feeder class
    lo_controller_appfd ?= lo_controller.


    LOOP AT it_request INTO DATA(lo_request).
      lo_cus_req ?= lo_request.
    ENDLOOP.

    CHECK lo_cus_req IS BOUND.

    CREATE DATA lr_par_optimize.
*   Planning parameters
    lr_par_optimize->plan_process = /scmtms/if_pln_const=>sc_process_vrg.
    lr_par_optimize->plan_prof_uuid = /scmtms/cl_pln_tbo_factory=>get_plan_prof_key( ).
    lr_par_optimize->sel_entr_only = /scmtms/if_pln_const=>sc_sel_only_sel_res. " selected entries with all resources
    lr_par_optimize->fu_succ = lo_cus_req->mt_tor_fu.

    TRY.
*  Start the Transportation Proposal Optimizer
        DATA(lo_tp_facade) = /scmtms/cl_pln_tp_factory=>get_tp_facade( ).
* set the gv_tp_multi indicator as abap_true to start the proposal optimizer for multiple Freight unit
* we are using the below indicator GV_TP_MULTI in the method CHK_REQ_DOC_FRM_ONE_PREDEC of class /SCMTMS/CL_VSR_CHECK_VRG.
        gv_tp_multi = abap_true.
        lo_tp_facade->start_proposal( EXPORTING is_tbo_key          = /scmtms/cl_pln_tbo_factory=>get_planning_tbo_key( )
                                                ir_par_optimize     = lr_par_optimize
                                      IMPORTING eo_change           = DATA(lo_change)
                                                eo_message          = DATA(lo_message) ).

* set the gv_tp_multi indicator as abap_false after using in the medhod CHK_REQ_DOC_FRM_ONE_PREDEC of class /SCMTMS/CL_VSR_CHECK_VRG.
        gv_tp_multi = abap_false.

* call the post_syncup_data method of ui controller to syncronising the data
        CALL METHOD post_syncup_data
          EXPORTING
            iv_update_tbo_nodes = abap_true
            io_change           = lo_change
            io_message          = lo_message.

      CATCH /bobf/cx_frw INTO DATA(lx_exception).
        lo_controller->show_error_page( lx_exception ).
    ENDTRY.


    DATA(lo_vsr_state)        = /scmtms/cl_pln_ol_factory=>get_factory( )->get_vsr_state( ).
    DATA(lv_optimizer_failed) = lo_vsr_state->is_optimizer_run_failed( ).

*  Check whether a solution is determined
    IF lv_optimizer_failed EQ abap_true.

      DATA(lo_vsr_message_helper)        = /scmtms/cl_pln_ol_factory=>get_factory( )->get_message_helper( ).
      lo_vsr_message_helper->get_opt_status_msg_last_run( EXPORTING iv_process          = /scmtms/if_pln_const=>sc_process_vrg
                                                           CHANGING co_message          = lo_message_opt_status ).
      /scmtms/cl_helper_ui=>msg_convert_bopf_2_ui( EXPORTING io_message  = lo_message_opt_status
                                                   IMPORTING et_messages = DATA(lt_ui_messages) ).
    ENDIF.

* recreate the ranklist based on amount of FO
    CALL METHOD recreate_ranklist
      EXPORTING
        it_map_sol  = /scmtms/cl_pln_tbo_helper=>mt_map_sol_buf_var                " Map solution to buffer variants
      IMPORTING
        et_ranklist = DATA(lt_ranklist).
* interchange the Sol id of MT_MAP_SOL_BUF_VAR based on the rank list which we created above
    LOOP AT /scmtms/cl_pln_tbo_helper=>mt_map_sol_buf_var INTO DATA(ls_map_sol).
      READ TABLE lt_ranklist INTO DATA(ls_ranklist) WITH KEY sol_id = ls_map_sol-sol_id.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      ls_map_sol-sol_id = ls_ranklist-new_sol_id.
      APPEND ls_map_sol TO lt_map_sol.
      CLEAR ls_map_sol.
    ENDLOOP.

* sort lt_map_sol by ascending order with sol id.
    SORT lt_map_sol BY sol_id ASCENDING.
* clear the original mt_map_sol_buf_var table
    CLEAR /scmtms/cl_pln_tbo_helper=>mt_map_sol_buf_var.
* appending the lt_map_sol data to original table  mt_map_sol_buf_var table
    /scmtms/cl_pln_tbo_helper=>mt_map_sol_buf_var = lt_map_sol[].

    IF lo_cus_req->mv_strategy = 'ZTP_MULTI'.

* Refresh the application specific buffers and caches
      lo_controller_appfd->refresh_application_buffer( ).

      IF lv_optimizer_failed EQ abap_false.

*   Get the current Page Layout for Transportation proposals
        lo_controller->get_tag_value( EXPORTING iv_key   = /scmtms/if_pl=>gc_appl_tag_current_tp_layout
                                      IMPORTING ev_value = lv_current_tp_layout ).
*  Change the page content
        lo_controller_appfd->set_ind_result_screen_active( abap_true ).

* Creates an instance of this class based on an event ID
        DATA(lo_event) = cl_fpm_event=>create_by_id( /scmtms/cl_pl_proposal=>sc_proposal_action-select_layout ).

        lo_event->mo_event_data->set_value( iv_key   = /scmtms/if_ui_pln_c=>sc_action_parameter-layout_id
                                            iv_value = lv_current_tp_layout ).

* raise event
        lo_controller_frwfd->raise_event( io_event = lo_event ).

* set the Proposal screen tag value in UI controller
        lo_controller->set_tag_value( EXPORTING iv_key   = /scmtms/cl_uih_pln_proposal=>co_tag_proposal_screen
                                                iv_value = abap_true ).

        ev_result = if_fpm_constants=>gc_event_result-ok.
      ELSE.
        lo_tp_facade->end_proposal( ).
        ev_result = if_fpm_constants=>gc_event_result-failed.
      ENDIF.
    ELSEIF lo_cus_req->mv_strategy = 'ZTP_SINGLE'.
* create accept parameter
      CREATE DATA lr_par_accept.
      lr_par_accept->sol_id = lv_solution_id.
      lr_par_accept->storage_mode = /scmtms/if_pln_const=>sc_save_mode_complete.
      lr_par_accept->plan_process = /scmtms/if_pln_const=>sc_process_vrg.
      lr_parameters = lr_par_accept.

* get accept action key
      DATA(lv_action_key) = /scmtms/if_pln_c=>sc_action-root-accept.
* get planning key
      DATA(ls_key) = /scmtms/cl_pln_tbo_factory=>get_planning_tbo_key( ).
      ASSERT ls_key-key IS NOT INITIAL. "TBO Must exist
      APPEND ls_key TO lt_keys.


      READ TABLE lo_controller->/bofu/if_fbi_controller~mt_bo_srvmgr_map WITH TABLE KEY bo_key = /scmtms/if_pln_c=>sc_bo_key
                                                                     ASSIGNING FIELD-SYMBOL(<ls_srvmgr>).
      IF sy-subrc EQ 0.
        DATA(lo_tbo_srvmgr) = <ls_srvmgr>-o_srv_mgr.
      ENDIF.
* call action
      TRY.
          lo_tbo_srvmgr->do_action( EXPORTING
                                           iv_act_key    = lv_action_key
                                           it_key        = lt_keys
                                           is_parameters = lr_parameters
                                         IMPORTING
                                           eo_change     = lo_change
                                           eo_message    = lo_message
                                           et_data       = lt_comp_changes_tab ).


          lo_tp_facade->cleanup_proposal( ).

        CATCH /bobf/cx_frw INTO lx_exception.
          lo_controller->/bofu/if_fbi_controller~show_error_page( lx_exception ). "Show error page for exceptional conditions
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
