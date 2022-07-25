*&---------------------------------------------------------------------*
*& Report  SBAL_DISPLAY_2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zr_ballog_display.

TABLES balhdr.
TABLES   t100.

* application data
SELECTION-SCREEN BEGIN OF BLOCK app WITH FRAME TITLE TEXT-app.
SELECT-OPTIONS so_obj FOR balhdr-object.
SELECT-OPTIONS so_sub FOR balhdr-subobject.
SELECT-OPTIONS so_exnum FOR balhdr-extnumber.
PARAMETERS p_cntxt TYPE baltabname
  MATCHCODE OBJECT dd_struc.
SELECTION-SCREEN END OF BLOCK app.

* time restriction
SELECTION-SCREEN BEGIN OF BLOCK tim WITH FRAME TITLE TEXT-001.
PARAMETERS pa_max TYPE bal_s_lfil-max_nr_logs." visible length 10.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) TEXT-003 FOR FIELD pa_df.
PARAMETERS pa_df TYPE baldate.
PARAMETERS pa_tf TYPE baltime.
*      PARAMETERS pa_ut TYPE flag AS CHECKBOX.                                  "prepared for user time display
*      SELECTION-SCREEN COMMENT (12) text-016 FOR FIELD pa_ut.                  "prepared for user time display
*      PARAMETERS pa_tz TYPE ttzz-tzone DEFAULT sy-zonlo.                       "prepared for user time display
*      SELECTION-SCREEN COMMENT (10) text-017 FOR FIELD pa_tz.                   "prepared for user time display
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) TEXT-004 FOR FIELD pa_dt.
PARAMETERS pa_dt TYPE baldate.
PARAMETERS pa_tt TYPE baltime.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK tim.

* keys
SELECTION-SCREEN BEGIN OF BLOCK key WITH FRAME TITLE TEXT-key.
SELECT-OPTIONS so_num FOR balhdr-lognumber.
SELECT-OPTIONS so_hdl FOR balhdr-log_handle VISIBLE LENGTH 22.
SELECTION-SCREEN END OF BLOCK key.

* originator data
SELECTION-SCREEN BEGIN OF BLOCK ori WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS so_user FOR balhdr-aluser.
SELECT-OPTIONS so_tcod FOR balhdr-altcode.
SELECT-OPTIONS so_prg FOR balhdr-alprog.
SELECTION-SCREEN END OF BLOCK ori.

* class restriction
SELECTION-SCREEN BEGIN OF BLOCK cls WITH FRAME TITLE TEXT-021.
SELECT-OPTIONS so_cls FOR balhdr-probclass.
SELECT-OPTIONS so_mode FOR balhdr-almode.
SELECTION-SCREEN END OF BLOCK cls.

* lock handling (logs in process)
SELECTION-SCREEN BEGIN OF BLOCK lck WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pa_lh3 TYPE c RADIOBUTTON GROUP lh DEFAULT 'X'.
SELECTION-SCREEN COMMENT (40) TEXT-lh3 FOR FIELD pa_lh3.
PARAMETERS pa_lh0 TYPE c RADIOBUTTON GROUP lh.
SELECTION-SCREEN COMMENT (70) TEXT-lh0 FOR FIELD pa_lh0.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pa_lh1 TYPE c RADIOBUTTON GROUP lh.
SELECTION-SCREEN COMMENT (40) TEXT-lh1 FOR FIELD pa_lh1.
PARAMETERS pa_lh2 TYPE c RADIOBUTTON GROUP lh.
SELECTION-SCREEN COMMENT (40) TEXT-lh2 FOR FIELD pa_lh2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK lck.

* display modes
SELECTION-SCREEN BEGIN OF BLOCK dsp WITH FRAME TITLE TEXT-006.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pa_alv TYPE c RADIOBUTTON GROUP ds DEFAULT 'X' USER-COMMAND ab.
SELECTION-SCREEN COMMENT (40) TEXT-007 FOR FIELD pa_alv.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pa_list TYPE c RADIOBUTTON GROUP ds.
SELECTION-SCREEN COMMENT (12) TEXT-008 FOR FIELD pa_list.
PARAMETERS pa_lt TYPE flag AS CHECKBOX.
SELECTION-SCREEN COMMENT (15) TEXT-018 FOR FIELD pa_lt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK dsp.

* display messages
SELECTION-SCREEN BEGIN OF BLOCK msg WITH FRAME TITLE TEXT-019.
SELECT-OPTIONS so_mid FOR t100-arbgb.                     "class
SELECT-OPTIONS so_mnu FOR t100-msgnr.                     "msg ID
SELECT-OPTIONS so_ter FOR balhdr-alstate.              "msg type (this is only to get type C(1))
SELECTION-SCREEN END OF BLOCK msg.

INITIALIZATION.
  PERFORM get_selopt_default.

  pa_df = sy-datum.
  pa_dt = sy-datum.
  pa_tt = '235959'.

AT SELECTION-SCREEN.
* manipulate the log number: fill the leading zeroes
  LOOP AT so_num.
    CASE so_num-option.
      WHEN 'CP' OR 'NP'.
        " do nothing
      WHEN 'BT' OR 'NB'.
        PERFORM fill_zeroes CHANGING so_num-low.
        PERFORM fill_zeroes CHANGING so_num-high.
      WHEN OTHERS.
        PERFORM fill_zeroes CHANGING so_num-low.
    ENDCASE.
    MODIFY so_num.
  ENDLOOP.

* handle initial time values
  IF pa_tf = `      `.
    pa_tf = '000000'.
  ENDIF.
  IF pa_tt = `      `.
    pa_tt = '000000'.
  ENDIF.

* F4 help for message error type
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ter-low.
  PERFORM get_f4_value_for_msgty USING 'low'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ter-high.
  PERFORM get_f4_value_for_msgty USING 'high'.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-name = 'PA_LT'.
      IF pa_alv = 'X'.
        screen-input = 0.
      ELSE.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF pa_alv = 'X' AND pa_lt = 'X'.
      pa_lt = ''.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
  PERFORM execute.

FORM execute.
  DATA lt_handle TYPE bal_t_logh.
  DATA lt_locked TYPE balhdr_t.

  PERFORM check_input.

  PERFORM set_selopt_default.

  PERFORM load_logs
    CHANGING lt_handle
             lt_locked.

  PERFORM display
    USING lt_handle
          lt_locked.

ENDFORM.

FORM check_input.
  IF pa_df > pa_dt.
    MESSAGE s260(bl).
    STOP.
  ENDIF.
  IF pa_df = pa_dt AND
     pa_tf > pa_tt.
    MESSAGE s261(bl).
    STOP.
  ENDIF.

*  IF pa_ut = 'X'.                          "prepared for user time display
*    PERFORM convert_time_date.
*  ENDIF.


ENDFORM.

FORM load_logs
  CHANGING et_handle TYPE bal_t_logh
           et_locked TYPE balhdr_t.

  DATA ls_filter TYPE bal_s_lfil.
  DATA lt_field TYPE bal_t_fld.
  DATA lt_header TYPE balhdr_t.
  DATA l_lock_handling TYPE i.
  DATA lt_handle TYPE bal_t_logh.
  DATA lf_alv TYPE c.
  DATA lf_nomsg TYPE c.
  DATA l_msgtext TYPE c LENGTH 100.
  DATA lt_message_handle     TYPE bal_t_msgh.
  DATA ls_message_filter     TYPE bal_s_mfil.

  FIELD-SYMBOLS <ls_header> TYPE balhdr.


  CLEAR et_handle.
  CLEAR et_locked.

  ls_filter-lognumber = so_num[].
  ls_filter-log_handle = so_hdl[].
  ls_filter-object = so_obj[].
  ls_filter-subobject = so_sub[].
  ls_filter-extnumber = so_exnum[].
  ls_filter-date_time-date_from = pa_df.
  ls_filter-date_time-time_from = pa_tf.
  ls_filter-date_time-date_to = pa_dt.
  ls_filter-date_time-time_to = pa_tt.
  ls_filter-aluser = so_user[].
  ls_filter-altcode = so_tcod[].
  ls_filter-probclass = so_cls[].
  ls_filter-almode = so_mode[].
  ls_filter-alprog = so_prg[].
  ls_filter-max_nr_logs = pa_max.

  APPEND 'LOG_HANDLE' TO lt_field.

  PERFORM get_display_mode
    CHANGING lf_alv.

  IF lf_alv IS INITIAL.
*   In text mode, display the selection options as well
    PERFORM init_page
      IN PROGRAM saplsbal_display
      USING -1 sy-batch.

    PERFORM display_selopt
      USING ls_filter.
  ENDIF.

  PERFORM progress_indicator
    USING 0 TEXT-009.

  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
*     I_CLIENT           = SY-MANDT
      i_s_log_filter     = ls_filter
      i_t_sel_field      = lt_field
    IMPORTING
      e_t_log_header     = lt_header
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2
      OTHERS             = 99.
  CASE sy-subrc.
    WHEN 0. " OK
    WHEN 1.
      IF lf_alv IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno INTO l_msgtext.
        WRITE / l_msgtext.
      ENDIF.
      STOP.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.

  CLEAR lt_handle.
  LOOP AT lt_header ASSIGNING <ls_header>.
    INSERT <ls_header>-log_handle INTO TABLE lt_handle.
  ENDLOOP.

* derive desired lock handling
  IF pa_lh0 IS NOT INITIAL.
    l_lock_handling = 0.
  ELSEIF pa_lh1 IS NOT INITIAL OR pa_lh3 IS NOT INITIAL.
    l_lock_handling = 1.
  ELSE.
    l_lock_handling = 2.
  ENDIF.

  PERFORM progress_indicator
    USING 33 TEXT-010.

  IF lf_alv IS INITIAL AND
     pa_lh3 IS NOT INITIAL.
*   Text mode showing also locked headers:
*   load handled in the display function
    et_handle = lt_handle.
  ELSE.
*   In text mode, always load also messages.
*   In ALV mode, load messages if locked logs are to be shown,
*   otherwise load them step-by step on click on log headers
    lf_nomsg = lf_alv.
    IF pa_lh0 IS NOT INITIAL.
      CLEAR lf_nomsg.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_handle         = lt_handle
        i_lock_handling        = l_lock_handling
        i_do_not_load_messages = lf_nomsg
      IMPORTING
        e_t_log_handle         = et_handle " really loaded logs
        e_t_locked             = et_locked
      EXCEPTIONS
        OTHERS                 = 0.

*   filter messages
    IF so_mid IS NOT INITIAL OR so_mnu IS NOT INITIAL OR so_ter IS NOT INITIAL.
      ls_message_filter-msgid = so_mid[].
      ls_message_filter-msgno = so_mnu[].
      ls_message_filter-msgty = so_ter[].

      PERFORM progress_indicator
        USING 53 TEXT-019.

      CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
        EXPORTING
          i_s_msg_filter = ls_message_filter
*         i_t_msg_context_filter = lt_context_filter
        IMPORTING
          e_t_msg_handle = lt_message_handle
        EXCEPTIONS
          msg_not_found  = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
*   Implement suitable error handling here
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM lt_message_handle COMPARING log_handle.

      et_handle[] = lt_message_handle[].
    ENDIF.
  ENDIF.

  IF pa_lh3 IS INITIAL.
    CLEAR et_locked. " do not show
  ENDIF.

ENDFORM.

FORM display
  USING it_handle TYPE bal_t_logh
        it_locked TYPE balhdr_t.

  DATA lf_alv TYPE c.
  DATA ls_profile TYPE bal_s_prof.
  DATA l_msgtxt TYPE c LENGTH 100.
  DATA ls_message_filter   TYPE bal_s_mfil.

  PERFORM progress_indicator
    USING 66 TEXT-011.

  PERFORM get_display_mode
    CHANGING lf_alv.

  IF it_locked IS NOT INITIAL AND
     lf_alv IS NOT INITIAL.
*   display headers of locked logs via ALV popup
    PERFORM display_locked_alv(saplsbal_display)
      USING it_locked.
  ENDIF.

* Now display the rest logs
  IF it_handle IS NOT INITIAL.
    IF lf_alv IS NOT INITIAL.
*     display via ALV, messages are shown on doubleclick only
      CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
        IMPORTING
          e_s_display_profile = ls_profile
        EXCEPTIONS
          OTHERS              = 0.

      CLEAR ls_profile-lev2_fcat.
      CLEAR ls_profile-lev2_sort.
      ls_profile-tree_nomsg = 'X'.

      IF p_cntxt IS NOT INITIAL.
        PERFORM merge_context_fcat USING p_cntxt CHANGING ls_profile-mess_fcat[].
      ENDIF.

      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_t_log_handle      = it_handle
          i_s_display_profile = ls_profile
*        exceptions
*         no_data_available   = 3
*         others              = 99
        .
    ELSE.
*     display as text
*     Filter messages
      IF so_mid IS NOT INITIAL OR so_mnu IS NOT INITIAL OR so_ter IS NOT INITIAL.
        ls_message_filter-msgid = so_mid[].
        ls_message_filter-msgno = so_mnu[].
        ls_message_filter-msgty = so_ter[].
      ENDIF.

      CALL FUNCTION 'BAL_DSP_LOG_TEXTFORM'
        EXPORTING
          it_log_handle     = it_handle
*     locked logs are displayed in the FM
*     if the function loads them itself
          if_load           = pa_lh3
          if_lt             = pa_lt
          ls_message_filter = ls_message_filter
*       EXCEPTIONS
*         LOG_NOT_FOUND     = 1
*         OTHERS            = 2
        .
    ENDIF.
  ELSEIF it_locked IS INITIAL.
    IF lf_alv IS NOT INITIAL.
      MESSAGE i208(bl).
    ELSE.
      MESSAGE i208(bl) INTO l_msgtxt.
      WRITE / l_msgtxt.
    ENDIF.
  ENDIF.

  PERFORM progress_indicator
    USING 100 space.

ENDFORM.

FORM display_selopt
  USING is_filter TYPE bal_s_lfil.

  DATA lt_dfies TYPE STANDARD TABLE OF dfies.
  DATA l_fieldinfo TYPE dfies.
  DATA l_text TYPE val_text.
  DATA l_bt_text TYPE string.
  DATA lo_elemdescr TYPE REF TO cl_abap_elemdescr.
  DATA l_tabix TYPE i.

  FIELD-SYMBOLS <ls_fieldinfo> TYPE dfies.
  FIELD-SYMBOLS <lt_range> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <ls_range> TYPE data.
  FIELD-SYMBOLS <l_so> TYPE c.
  FIELD-SYMBOLS <l_value> TYPE c.

  DEFINE write_lock_handling.
    IF pa_&1 IS NOT INITIAL.
      WRITE TEXT-&1.
    ENDIF.
  END-OF-DEFINITION.

  WRITE / 'Selektionsoptionen'(014) COLOR COL_NORMAL.

  IF is_filter IS INITIAL.
    WRITE / 'Keine Einschränkungen'(013).
    ULINE.
    RETURN.
  ENDIF.

* process ranges
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = 'BAL_S_LFIL'
      all_types = 'X'
    TABLES
      dfies_tab = lt_dfies.

  LOOP AT lt_dfies ASSIGNING <ls_fieldinfo>.
    CASE <ls_fieldinfo>-fieldname.
      WHEN 'DATE_TIME' OR
           'MAX_NR_LOGS'.
        CONTINUE.
    ENDCASE.

    ASSIGN COMPONENT <ls_fieldinfo>-fieldname
      OF STRUCTURE is_filter TO <lt_range>.
    CHECK sy-subrc IS INITIAL.
    CHECK <lt_range> IS NOT INITIAL.

    CLEAR lo_elemdescr.

    LOOP AT <lt_range> ASSIGNING <ls_range>.
      l_tabix = sy-tabix.
      ASSIGN COMPONENT 'LOW'
        OF STRUCTURE <ls_range> TO <l_value>.
      IF lo_elemdescr IS INITIAL.
        lo_elemdescr ?= cl_abap_typedescr=>describe_by_data( <l_value> ).
        l_fieldinfo = lo_elemdescr->get_ddic_field( sy-langu ).
        WRITE /(*) l_fieldinfo-scrtext_m.
        IF lines( <lt_range> ) > 1.
          WRITE / '-'.
        ENDIF.
      ENDIF.

      IF l_tabix > 1.
        WRITE / '-'.
      ENDIF.

      ASSIGN COMPONENT 'SIGN'
        OF STRUCTURE <ls_range> TO <l_so>.
      IF <l_so> = 'E'.
        WRITE 'ausschließlich'(012).
      ENDIF.

      ASSIGN COMPONENT 'OPTION'
        OF STRUCTURE <ls_range> TO <l_so>.

      PERFORM get_valtext
        USING 'DDOPTION' <l_so>
        CHANGING l_text.


      CASE <l_so>.
        WHEN 'BT' OR 'NB'.
          l_bt_text = l_text.
          REPLACE '...' IN l_bt_text WITH <l_value>.
          ASSIGN COMPONENT 'HIGH'
            OF STRUCTURE <ls_range> TO <l_value>.
          REPLACE '...' IN l_bt_text WITH <l_value>.
          WRITE l_bt_text.
        WHEN OTHERS.
          WRITE (*) l_text.
          WRITE (*) <l_value>.
      ENDCASE.
    ENDLOOP.
  ENDLOOP.

* message filter  (messages not included in is_filter)
  DATA: wa_range_id TYPE LINE OF bal_r_msid.
  DATA: wa_range_no TYPE LINE OF bal_r_msno.
  DATA: wa_range_ty TYPE LINE OF bal_r_msty.

  IF so_mid[] IS NOT INITIAL.
    WRITE / 'Message class'.
    LOOP AT so_mid[] INTO wa_range_id.
      IF wa_range_id-sign = 'E'.
        WRITE 'ausschließlich'(012).
      ENDIF.

      PERFORM get_valtext
       USING 'DDOPTION' wa_range_id-option
       CHANGING l_text.

      CASE wa_range_id-option.
        WHEN 'BT' OR 'NB'.
          l_bt_text = l_text.
          REPLACE '...' IN l_bt_text WITH wa_range_id-low.
          REPLACE '...' IN l_bt_text WITH wa_range_id-high.
          WRITE l_bt_text.
        WHEN OTHERS.
          l_bt_text = wa_range_id-low.
          WRITE (*) l_text.
          WRITE (*) l_bt_text.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  IF so_mnu[] IS NOT INITIAL.
    CLEAR l_text.
    CLEAR l_bt_text.
    WRITE / 'Message number'.
    LOOP AT so_mnu[] INTO wa_range_no.
      IF wa_range_no-sign = 'E'.
        WRITE 'ausschließlich'(012).
      ENDIF.

      PERFORM get_valtext
       USING 'DDOPTION' wa_range_id-option
       CHANGING l_text.

      CASE wa_range_no-option.
        WHEN 'BT' OR 'NB'.
          l_bt_text = l_text.
          REPLACE '...' IN l_bt_text WITH wa_range_no-low.
          REPLACE '...' IN l_bt_text WITH wa_range_no-high.
          WRITE l_bt_text.
        WHEN OTHERS.
          l_bt_text = wa_range_no-low.
          WRITE (*) l_text.
          WRITE (*) l_bt_text.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  IF so_ter[] IS NOT INITIAL.
    CLEAR l_text.
    CLEAR l_bt_text.
    WRITE / 'Message type'.
    LOOP AT so_ter[] INTO wa_range_ty.
      IF wa_range_ty-sign = 'E'.
        WRITE 'ausschließlich'(012).
      ENDIF.

      PERFORM get_valtext
       USING 'DDOPTION' wa_range_ty-option
       CHANGING l_text.

      CASE wa_range_ty-option.
        WHEN 'BT' OR 'NB'.
          l_bt_text = l_text.
          REPLACE '...' IN l_bt_text WITH wa_range_ty-low.
          REPLACE '...' IN l_bt_text WITH wa_range_ty-high.
          WRITE l_bt_text.
        WHEN OTHERS.
          l_bt_text = wa_range_ty-low.
          WRITE (*) l_text.
          WRITE (*) l_bt_text.
      ENDCASE.
    ENDLOOP.
  ENDIF.

* date and time interval
  IF is_filter-date_time IS NOT INITIAL.
    PERFORM get_valtext
      USING 'DDOPTION' 'BT'
            CHANGING l_text.

    l_bt_text = l_text.
    CLEAR l_text.
    WRITE is_filter-date_time-date_from TO l_text(10).
    WRITE is_filter-date_time-time_from TO l_text+11(8).
    REPLACE '...' IN l_bt_text WITH l_text.
    WRITE is_filter-date_time-date_to TO l_text(10).
    WRITE is_filter-date_time-time_to TO l_text+11(8).
    REPLACE '...' IN l_bt_text WITH l_text.

    WRITE: /(*) 'Anlegedatum'(015), l_bt_text.
  ENDIF.

* locked logs
  IF pa_lh0 IS NOT INITIAL OR
     pa_lh1 IS NOT INITIAL OR
     pa_lh2 IS NOT INITIAL OR
     pa_lh3 IS NOT INITIAL.
    WRITE: /(*) TEXT-005 NO-GAP, ':'.
    write_lock_handling: lh0, lh1, lh2, lh3.
  ENDIF.

* max. nr. logs
  IF is_filter-max_nr_logs > 0.
    WRITE: /(*) TEXT-max NO-GAP, ':', is_filter-max_nr_logs.
  ENDIF.

  ULINE.
ENDFORM.

FORM get_valtext
  USING i_domname TYPE domname
        i_value TYPE data
  CHANGING e_text TYPE val_text.

  DATA l_value TYPE domvalue_l.
  DATA l_wa TYPE dd07v.

  l_value = i_value.

  CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
    EXPORTING
      domname  = i_domname
      value    = l_value
      langu    = sy-langu
*     BYPASS_BUFFER       = ' '
    IMPORTING
      dd07v_wa = l_wa
*     RC       =
    .

  e_text = l_wa-ddtext.

ENDFORM.

FORM set_selopt_default.

  DEFINE set_parameter.
    DESCRIBE TABLE &1[] LINES sy-tfill.
    CASE sy-tfill.
      WHEN 0.
        CLEAR &1-low.
        SET PARAMETER ID &2 FIELD &1-low.
      WHEN 1.
        READ TABLE &1 INDEX 1.
        IF &1-sign = 'I' AND
           &1-option = 'EQ'.
          SET PARAMETER ID &2 FIELD &1-low.
        ENDIF.
    ENDCASE.
  END-OF-DEFINITION.

  set_parameter:
    so_obj 'BALOBJ',
    so_sub 'BALSUBOBJ',
    so_exnum 'BALEXT'.

ENDFORM.

FORM get_selopt_default.

  DEFINE get_parameter.
    GET PARAMETER ID &2 FIELD &1-low.
    IF &1-low IS NOT INITIAL AND
       &1-low <> '*'.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      APPEND &1.
    ENDIF.
  END-OF-DEFINITION.

  get_parameter:
    so_obj 'BALOBJ',
    so_sub 'BALSUBOBJ',
    so_exnum 'BALEXT'.

ENDFORM.

FORM fill_zeroes
  CHANGING c_lognum TYPE balognr.

  DATA l_num TYPE i.
  STATICS l_zero TYPE balognr.

  IF l_zero IS INITIAL.
    CLEAR l_zero WITH '0'.
  ENDIF.

  l_num = strlen( l_zero ) - strlen( c_lognum ).

  CHECK l_num > 0.

  CONCATENATE l_zero(l_num) c_lognum
    INTO c_lognum.

ENDFORM.

FORM progress_indicator
  USING i_pct TYPE i
        i_text TYPE clike.

  CHECK sy-batch IS INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = i_pct
      text       = i_text.

ENDFORM.

FORM get_display_mode
  CHANGING ef_alv TYPE char1.

  CLEAR ef_alv.

  IF sy-batch IS INITIAL AND
     pa_alv IS NOT INITIAL.
*   display via ALV
    ef_alv = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  convert_time_date
*&---------------------------------------------------------------------*
*       text: "prepared for user time display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM convert_time_date .
*
*DATA: tstamp_f TYPE timestamp,
*      tstamp_t TYPE timestamp,
*      lv_sys_tzone TYPE ttzz-tzone.
*
*SELECT SINGLE tzonesys FROM ttzcu INTO lv_sys_tzone.
*
*
*
*
**convert date and time FROM
*CONVERT DATE pa_df TIME pa_tf
*          INTO TIME STAMP tstamp_f
*          TIME ZONE lv_sys_tzone.
*
*
*CONVERT TIME STAMP tstamp_f
*    TIME ZONE 'UTC' INTO TIME pa_tf DATE pa_df.
*
*
**CONVERT TIME STAMP tstamp_f INTO DATE pa_df TIME pa_tf.
*
**convert date and time TO
*CONVERT DATE pa_dt TIME pa_tt
*          INTO TIME STAMP tstamp_t
*          TIME ZONE lv_sys_tzone.
*
*CONVERT TIME STAMP tstamp_f
*    TIME ZONE 'UTC' INTO TIME pa_tt DATE pa_dt.
*
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_F4_VALUE_FOR_MSGTY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_f4_value_for_msgty
   USING low_or_high TYPE c.

  TYPES:
    BEGIN OF ls_field,
      domvalue_l TYPE dd07v-domvalue_l,
      ddtext     TYPE dd07v-ddtext,
    END OF ls_field.

  DATA: it_sel_m_ty TYPE STANDARD TABLE OF ddshretval,
        wa_sel_m_ty LIKE LINE OF it_sel_m_ty.


  DATA: domname LIKE dd01v-domname.
  DATA: record_tab   TYPE STANDARD TABLE OF seahlpres,
        dd07v_tab    TYPE STANDARD TABLE OF dd07v WITH HEADER LINE,
        wa_dd07v_tab LIKE LINE OF dd07v_tab,
        lt_field     TYPE STANDARD TABLE OF ls_field WITH HEADER LINE.

  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name       = 'BALMSGTY'
      langu      = sy-langu
      texts_only = 'X'
    TABLES
      dd07v_tab  = dd07v_tab
    EXCEPTIONS
      OTHERS     = 2.

*  MOVE-CORRESPONDING dd07v_tab[] TO  lt_field[].
  LOOP AT dd07v_tab INTO wa_dd07v_tab.
    lt_field-domvalue_l = wa_dd07v_tab-domvalue_l.
    lt_field-ddtext = wa_dd07v_tab-ddtext.
    APPEND lt_field.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = 'BALMSGTY'
      retfield        = 'DOMVALUE_L'
*     PVALKEY         = ' '
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
*     DYNPROFIELD     = ''
*     STEPL           = 0
      window_title    = TEXT-020
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = 'X'  "allows you select multiple entries from the popup
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     MARK_TAB        =
* IMPORTING
*     USER_RESET      = ld_ret
    TABLES
      value_tab       = lt_field
*     FIELD_TAB       = lt_field
      return_tab      = it_sel_m_ty
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  LOOP AT it_sel_m_ty INTO wa_sel_m_ty.

    IF low_or_high EQ 'low'.
      so_ter-low    = wa_sel_m_ty-fieldval.
      so_ter-sign   = 'I'.
      so_ter-option = 'EQ'.
*      APPEND so_ter.
    ELSEIF low_or_high EQ 'high'.
      so_ter-high    = wa_sel_m_ty-fieldval.
      so_ter-sign   = 'I'.
      so_ter-option = 'EQ'.
*      APPEND so_ter.
    ENDIF.
*      CLEAR so_ter.
  ENDLOOP.

ENDFORM.

FORM merge_context_fcat
    USING i_ddic TYPE baltabname
    CHANGING t_fcat TYPE bal_t_fcat.

  DATA(lt_objects) = cl_abap_typedescr=>describe_by_name( i_ddic )->get_ddic_object( ).
  CHECK lt_objects[] IS NOT INITIAL.

  APPEND LINES OF CORRESPONDING bal_t_fcat(
    lt_objects[]
    MAPPING ref_table = tabname
            ref_field = fieldname
  ) TO t_fcat[].

ENDFORM.
