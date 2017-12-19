*&---------------------------------------------------------------------*
*& Report ZSFBH
*&---------------------------------------------------------------------*
*&
*& SAPscript forms breakdown helper
*&
*& Version 2017.2-01 (23.04.2017)
*&
*& written by Michael Keller
*&
*& This tool helps analysing SAPscript forms. Have a look at my blog
*& on SAP Community Network for more details.
*&
*& Disclaimer: I do not take any responsibility and I am not liable for
*& any damage caused through use of this tool.
*&
*&---------------------------------------------------------------------*

REPORT zsfbh MESSAGE-ID sy.

TYPE-POOLS: icon,
            vrm.

INCLUDE <cl_alv_control>.

TABLES sscrfields.

CONSTANTS gc_vinfo TYPE text30 VALUE '2017.2-01 (23.04.2017)'.

CONSTANTS: gc_burl1(23) TYPE c VALUE 'https://blogs.sap.com',
           gc_burl2(32) TYPE c VALUE '/2016/09/19/',
           gc_burl3(32) TYPE c VALUE 'sapscript-forms-breakdown-tool/'.

CONSTANTS: gc_yes TYPE xfeld VALUE 'X',
           gc_no  TYPE xfeld VALUE space.

CONSTANTS: gc_enabled  TYPE char1 VALUE '1',
           gc_disabled TYPE char1 VALUE '0'.

CONSTANTS: gc_rc_error TYPE sysubrc VALUE 4.

CLASS lcl_event_handler DEFINITION DEFERRED.

TYPES: BEGIN OF operand,
         operand TYPE tdline,
       END OF operand.

TYPES: operands TYPE TABLE OF operand.

TYPES: BEGIN OF operator,
         operator TYPE tdline,
       END OF operator.

TYPES: operators TYPE TABLE OF operator.

DATA: gs_header TYPE thead,
      gt_lines  TYPE TABLE OF tline,
      gt_fcat   TYPE lvc_t_fcat,
      gr_grid   TYPE REF TO cl_gui_alv_grid,
      gr_evhdl  TYPE REF TO lcl_event_handler.

DATA: gv_nobr_space TYPE c LENGTH 1.

DATA: BEGIN OF gt_data OCCURS 100,
        mark     TYPE xfeld,
        row      TYPE tdrow,
        window   TYPE tdwindow,
        format   TYPE tdformat,
        original TYPE tdline,
        colexpic TYPE icon_d,
        colexpfn TYPE uffilter,
        adapted  TYPE tdline,
        symbol1  TYPE tdline,
        symbol2  TYPE tdline,
        symbol3  TYPE tdline,
        symbol4  TYPE tdline,
        symbol5  TYPE tdline,
        level    TYPE i,
        mccabe1  TYPE numc5,
        mccabe2  TYPE numc5,
        color    TYPE lvc_t_scol,
        style    TYPE lvc_t_styl.
DATA: END OF gt_data.

DATA: BEGIN OF gt_mark OCCURS 5,
        tabix TYPE sytabix.
DATA: END OF gt_mark.

DATA: BEGIN OF gs_stats,
        locphy TYPE numc5, " number of physical lines
        locpro TYPE numc5, " number of program lines
        locprp TYPE numc3,
        loccom TYPE numc5, " number of commented lines
        loccop TYPE numc3,
        locbl  TYPE numc5, " number of blank lines
        locblp TYPE numc3,
        numwin TYPE numc3, " number of windows
        numtel TYPE numc3, " number of text elements
        mccab1 TYPE numc5, " cyclomatic complexity (single precision)
        mccab2 TYPE numc5, " cyclomatic complexity (double precision)
        hsopt  TYPE operators, " Halstead operators
        hsopd  TYPE operands,  " Halstead operands
        hsln1  TYPE numc5, " Halstead n1
        hsln2  TYPE numc5, " Halstead n2
        hsbn1  TYPE numc5, " Halstead N1
        hsbn2  TYPE numc5, " Halstead N2
        hsprv  TYPE numc5, " Halstead program vocabulary
        hsprl  TYPE numc5, " Halstead program length
        hsvol  TYPE numc5, " Halstead volume
        hsdif  TYPE numc5, " Halstead difficulty
        hseff  TYPE numc5, " Halstead effort
        hstti  TYPE numc5, " Halstead time to implement
      END OF gs_stats.

* commands used in SAPscript
DATA: BEGIN OF gt_sscmd OCCURS 40,
        command TYPE tdline.
DATA: END OF gt_sscmd.

* form selection
SELECTION-SCREEN: BEGIN OF BLOCK bl_form WITH FRAME TITLE gv_tx005,
                  BEGIN OF LINE,
                  COMMENT 1(20) gv_tx000,
                  POSITION 25.
PARAMETERS:       pa_mandt LIKE gs_header-mandt
                           DEFAULT sy-mandt
                           MATCHCODE OBJECT salv_bs_mandt.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(20) gv_tx001,
                  POSITION 25.
PARAMETERS:       pa_fname TYPE tdobname
                           MEMORY ID txf
                           VISIBLE LENGTH 15.
SELECTION-SCREEN: PUSHBUTTON 44(20) pb_fshow USER-COMMAND form_show
                                             VISIBLE LENGTH 4,
                  PUSHBUTTON 50(20) pb_fedit USER-COMMAND form_edit
                                             VISIBLE LENGTH 4,
                  END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(20) gv_tx002,
                  POSITION 25.
PARAMETERS:       pa_flang TYPE tdspras
                           MEMORY ID txl
                           DEFAULT sy-langu
                           MATCHCODE OBJECT h_t002.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(20) gv_tx003,
                  POSITION 25.
PARAMETERS:       pa_wname TYPE txline.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(20) gv_tx004,
                  POSITION 25.
PARAMETERS:       pa_tname TYPE txline.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK bl_form.

* color selection
SELECTION-SCREEN: BEGIN OF BLOCK bl_colors WITH FRAME TITLE gv_tx006,
                  BEGIN OF LINE,
                  COMMENT 1(10) gv_tx007,
                  POSITION 11.
PARAMETERS:       pa_form1 TYPE tdformat DEFAULT '/W'.
SELECTION-SCREEN: COMMENT 16(5) gv_tx008,
                  POSITION 22.
PARAMETERS:       pa_clrp1 TYPE text12
                           DEFAULT '500'
                           AS LISTBOX
                           VISIBLE LENGTH 12
                           USER-COMMAND clrp1.
SELECTION-SCREEN: COMMENT 36(10) gv_tx019,
                  POSITION 47.
PARAMETERS:       pa_colr1 TYPE numc3 DEFAULT '500'.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(10) gv_tx009,
                  POSITION 11.
PARAMETERS:       pa_form2 TYPE tdformat DEFAULT '/E'.
SELECTION-SCREEN: COMMENT 16(5) gv_tx010,
                  POSITION 22.
PARAMETERS:       pa_clrp2 TYPE text12
                           DEFAULT '100'
                           AS LISTBOX
                           VISIBLE LENGTH 12
                           USER-COMMAND clrp2.
SELECTION-SCREEN: COMMENT 36(10) gv_tx020,
                  POSITION 47.
PARAMETERS:       pa_colr2 TYPE numc3 DEFAULT '100'.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(10) gv_tx011,
                  POSITION 11.
PARAMETERS:       pa_form3 TYPE tdformat DEFAULT '/:'.
SELECTION-SCREEN: COMMENT 16(5) gv_tx012,
                  POSITION 22.
PARAMETERS:       pa_clrp3 TYPE text12
                           DEFAULT '200'
                           AS LISTBOX
                           VISIBLE LENGTH 12
                           USER-COMMAND clrp3.
SELECTION-SCREEN: COMMENT 36(10) gv_tx021,
                  POSITION 47.
PARAMETERS:       pa_colr3 TYPE numc3 DEFAULT '200'.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(10) gv_tx013,
                  POSITION 11.
PARAMETERS:       pa_form4 TYPE tdformat DEFAULT '/*'.
SELECTION-SCREEN: COMMENT 16(5) gv_tx014,
                  POSITION 22.
PARAMETERS:       pa_clrp4 TYPE text12
                           DEFAULT '210'
                           AS LISTBOX
                           VISIBLE LENGTH 12
                           USER-COMMAND clrp4.
SELECTION-SCREEN: COMMENT 36(10) gv_tx022,
                  POSITION 47.
PARAMETERS:       pa_colr4 TYPE numc3 DEFAULT '210'.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK bl_colors.

* ALV-layout
SELECTION-SCREEN: BEGIN OF BLOCK bl_vari WITH FRAME TITLE gv_tx015,
                  BEGIN OF LINE,
                  COMMENT 1(13) gv_tx016.
PARAMETERS:       pa_varia TYPE slis_vari.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE,
                  COMMENT 1(13) gv_tx023.
PARAMETERS:       pa_idtcn TYPE n LENGTH 1 DEFAULT '3'.
SELECTION-SCREEN: COMMENT 17(20) gv_tx024,
                  END OF LINE,
                  END OF BLOCK bl_vari.

* update
SELECTION-SCREEN: BEGIN OF BLOCK bl_update WITH FRAME TITLE gv_tx017,
                  BEGIN OF LINE,
                  COMMENT 1(36) gv_tx018,
                  PUSHBUTTON 37(50) pb_updck USER-COMMAND updcheck
                                             VISIBLE LENGTH 22.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK bl_update.

SELECTION-SCREEN: FUNCTION KEY 1,
                  FUNCTION KEY 2,
                  FUNCTION KEY 3,
                  FUNCTION KEY 4,
                  FUNCTION KEY 5.

DEFINE bdc-add.

  CLEAR ls_bdcdt.

  ls_bdcdt-dynbegin = &1.

  IF &1 EQ 'X'.
    ls_bdcdt-program = &2.
    ls_bdcdt-dynpro = &3.
  ELSE.
    ls_bdcdt-fnam = &2.
    ls_bdcdt-fval = &3.
  ENDIF.

  APPEND ls_bdcdt TO lt_bdcdt.

END-OF-DEFINITION.

DEFINE sscmd_add.

  APPEND &1 TO gt_sscmd.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS handle_after_user_command
                  FOR EVENT after_user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm
                  e_saved
                  e_not_processed.

    CLASS-METHODS handle_context_menu_request
                  FOR EVENT context_menu_request OF cl_gui_alv_grid
      IMPORTING e_object
                  sender.

    CLASS-METHODS handle_onf4
                  FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS handle_user_command
                  FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_after_user_command.

    DATA ls_data LIKE LINE OF gt_data.

    IF e_ucomm = '&DELETE_FILTER'.
*     reset collapse/expand icons
      ls_data-colexpic = icon_collapse.

      MODIFY gt_data FROM ls_data
                     TRANSPORTING colexpic
                     WHERE colexpic = icon_expand.

      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD handle_context_menu_request.

    CALL METHOD e_object->add_separator.

    CALL METHOD e_object->add_function
      EXPORTING
        fcode = '&COPY'
        text  = 'Copy to clipboard'
        icon  = icon_system_copy.
*       ftype             =
*       disabled          =
*       hidden            =
*       checked           =
*       accelerator       =
*       insert_at_the_top = SPACE

    CALL METHOD e_object->add_function
      EXPORTING
        fcode = '&EDIT_SAME_WINDOW'
        text  = 'Edit line in same window'
        icon  = icon_change.
*       ftype             =
*       disabled          =
*       hidden            =
*       checked           =
*       accelerator       =
*       insert_at_the_top = SPACE

    CALL METHOD e_object->add_function
      EXPORTING
        fcode = '&EDIT_NEW_WINDOW'
        text  = 'Edit line in new window'
        icon  = icon_change.
*       ftype             =
*       disabled          =
*       hidden            =
*       checked           =
*       accelerator       =
*       insert_at_the_top = SPACE

    CALL METHOD e_object->add_function
      EXPORTING
        fcode = '&SHOW_STATS'
        text  = 'Show stats'
        icon  = icon_display.
*       ftype             =
*       disabled          =
*       hidden            =
*       checked           =
*       accelerator       =
*       insert_at_the_top = SPACE

  ENDMETHOD.                    "handle_context_menu_req

  METHOD handle_onf4.
  ENDMETHOD.                    "handle_onf4

  METHOD handle_user_command.

    DATA: ls_rowid TYPE lvc_s_row,
          ls_colid TYPE lvc_s_col,
          ls_data  LIKE LINE OF gt_data.

    CALL METHOD gr_grid->get_current_cell
      IMPORTING
*       e_row     =
*       e_value   =
*       e_col     =
        es_row_id = ls_rowid
        es_col_id = ls_colid.
*       es_row_no =

    IF ls_rowid IS NOT INITIAL.
      READ TABLE gt_data INTO ls_data INDEX ls_rowid-index.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.

    CASE e_ucomm.
      WHEN '&COPY'.
        PERFORM alv_copy_to_clipboard.

      WHEN '&EDIT_SAME_WINDOW'.
        PERFORM form_edit USING '1' ls_data.

      WHEN '&EDIT_NEW_WINDOW'.
        PERFORM form_edit USING '2' ls_data.

      WHEN '&SHOW_STATS'.
        PERFORM form_stats_show.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_fname.
  PERFORM form_f4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_wname.
  PERFORM form_window_f4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_tname.
  PERFORM form_text_element_f4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_varia.
  PERFORM alv_variant_f4.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.

START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM main.

  DATA lv_subrc TYPE sysubrc.

  CLEAR gs_stats.

  PERFORM prechecks CHANGING lv_subrc.
  IF lv_subrc <> 0.
    RETURN.
  ENDIF.

  PERFORM form_read USING    space
                    CHANGING lv_subrc.

  IF lv_subrc <> 0.
    RETURN.
  ENDIF.

  PERFORM alv_data_prepare CHANGING lv_subrc.
  IF lv_subrc <> 0.
    RETURN.
  ENDIF.

  PERFORM alv_fcat_prepare CHANGING lv_subrc.
  IF lv_subrc <> 0.
    RETURN.
  ENDIF.

  PERFORM alv_show CHANGING lv_subrc.
  IF lv_subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.                    "main
*&---------------------------------------------------------------------*
*&      Form  ALV_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_fcat_prepare CHANGING cv_subrc TYPE sysubrc.

  DATA ls_fcat TYPE lvc_s_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = 1.
  ls_fcat-fieldname = 'ROW'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Line'.
  ls_fcat-scrtext_m = 'Line'.
  ls_fcat-scrtext_l = 'Line'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'WINDOW'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Window'.
  ls_fcat-scrtext_m = 'Window'.
  ls_fcat-scrtext_l = 'Window'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'FORMAT'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Format'.
  ls_fcat-scrtext_m = 'Format'.
  ls_fcat-scrtext_l = 'Format'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'ORIGINAL'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Original'.
  ls_fcat-scrtext_m = 'Original'.
  ls_fcat-scrtext_l = 'Original'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'COLEXPIC'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-icon      = gc_yes.
  ls_fcat-scrtext_s = 'C./E. Icon'.
  ls_fcat-scrtext_m = 'Col./Exp. Icon'.
  ls_fcat-scrtext_l = 'Collapse/Expand Icon'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'COLEXPFN'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-no_out    = gc_yes.
  ls_fcat-scrtext_s = 'C./E. Id'.
  ls_fcat-scrtext_m = 'Col./Exp. Id'.
  ls_fcat-scrtext_l = 'Collapse/Expand Id'.
  ls_fcat-ref_field = 'FILTER_ID'.
  ls_fcat-ref_table = 'DF55L'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'ADAPTED'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Adapted'.
  ls_fcat-scrtext_m = 'Adapted'.
  ls_fcat-scrtext_l = 'Adapted'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'SYMBOL1'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Symbol 1'.
  ls_fcat-scrtext_m = 'Symbol 1'.
  ls_fcat-scrtext_l = 'Symbol 1'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'SYMBOL2'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Symbol 2'.
  ls_fcat-scrtext_m = 'Symbol 2'.
  ls_fcat-scrtext_l = 'Symbol 2'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'SYMBOL3'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Symbol 3'.
  ls_fcat-scrtext_m = 'Symbol 3'.
  ls_fcat-scrtext_l = 'Symbol 3'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'SYMBOL4'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Symbol 4'.
  ls_fcat-scrtext_m = 'Symbol 4'.
  ls_fcat-scrtext_l = 'Symbol 4'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'SYMBOL5'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Symbol 5'.
  ls_fcat-scrtext_m = 'Symbol 5'.
  ls_fcat-scrtext_l = 'Symbol 5'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'LEVEL'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'Level'.
  ls_fcat-scrtext_m = 'Level'.
  ls_fcat-scrtext_l = 'Level'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'MCCABE1'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'McCabe'.
  ls_fcat-scrtext_m = 'McCabe (SP)'.
  ls_fcat-scrtext_l = 'McCabe (single precision)'.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-col_pos = ls_fcat-col_pos + 1.
  ls_fcat-fieldname = 'MCCABE2'.
  ls_fcat-tabname   = 'GT_DATA'.
  ls_fcat-scrtext_s = 'McCabe'.
  ls_fcat-scrtext_m = 'McCabe (DP)'.
  ls_fcat-scrtext_l = 'McCabe (double precision)'.
  APPEND ls_fcat TO gt_fcat.

ENDFORM.                    " ALV_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ALV_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM alv_data_prepare CHANGING cv_subrc TYPE sysubrc.

  DATA: ls_data    LIKE LINE OF gt_data,
        lv_count   TYPE i,
        lt_parts   TYPE TABLE OF tdline,
        ls_parts   TYPE tdline,
        lv_row     TYPE i,
        lt_lines   TYPE TABLE OF tline,
        ls_lines   TYPE tline,
        lt_symbols TYPE TABLE OF itcst,
        lv_window  TYPE tdwindow,
        lv_when    TYPE xfeld,
        lv_level   TYPE i,
        lv_begin   TYPE sytabix,
        lv_copy    TYPE xfeld,
        lv_tabix   TYPE sytabix.

  FIELD-SYMBOLS: <fs_lines>   TYPE tline,
                 <fs_symbols> TYPE itcst.


  IF pa_wname IS NOT INITIAL.
*   reduce to relevant window data
    READ TABLE gt_lines TRANSPORTING NO FIELDS
                        WITH KEY tdformat = '/W'
                                 tdline   = pa_wname.
    IF sy-subrc <> 0.
      cv_subrc = sy-subrc.
      RETURN.
    ENDIF.

    lv_begin = sy-tabix.

    LOOP AT gt_lines INTO ls_lines FROM lv_begin.
      IF ls_lines-tdformat = '/W' AND ls_lines-tdline <> pa_wname.
        EXIT.
      ENDIF.

      IF pa_tname IS INITIAL.
        APPEND ls_lines TO lt_lines.
      ELSE.
        IF ls_lines-tdformat = '/E' AND ls_lines-tdline = pa_tname.
          lv_copy = 'X'.
        ENDIF.
        IF ls_lines-tdformat = '/E' AND ls_lines-tdline <> pa_tname.
          CLEAR lv_copy.
        ENDIF.
        IF lv_copy = 'X'.
          APPEND ls_lines TO lt_lines.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_lines IS NOT INITIAL.
      CLEAR gt_lines.
      gt_lines = lt_lines.
    ENDIF.
  ENDIF.

  LOOP AT gt_lines ASSIGNING <fs_lines>.
    CLEAR: ls_data,
           lt_lines.

    lv_tabix = sy-tabix.

    ls_data-format   = <fs_lines>-tdformat.
    ls_data-original = <fs_lines>-tdline.
    ls_data-adapted  = <fs_lines>-tdline.
    ls_data-window   = lv_window.

    PERFORM alv_color_set CHANGING ls_data.

    IF <fs_lines>-tdformat = '/W'.
      CLEAR: lv_row,
             ls_data-window.
      lv_window = <fs_lines>-tdline.
    ENDIF.

    IF <fs_lines>-tdformat = '/:'.
      SPLIT <fs_lines>-tdline AT space INTO TABLE lt_parts.
      IF sy-subrc = 0.
        DELETE lt_parts WHERE table_line = space.
      ENDIF.
      READ TABLE lt_parts INTO ls_parts INDEX 1.
      IF sy-subrc <> 0.
      ENDIF.

      APPEND <fs_lines>-tdline TO lt_lines.

      CALL FUNCTION 'TEXT_SYMBOL_COLLECT'
        TABLES
          lines   = lt_lines
          symbols = lt_symbols.

      LOOP AT lt_symbols ASSIGNING <fs_symbols>.
        CASE sy-tabix.
          WHEN '1'.
            ls_data-symbol1 = <fs_symbols>-name.
          WHEN '2'.
            ls_data-symbol2 = <fs_symbols>-name.
          WHEN '3'.
            ls_data-symbol3 = <fs_symbols>-name.
          WHEN '4'.
            ls_data-symbol4 = <fs_symbols>-name.
          WHEN '5'.
            ls_data-symbol5 = <fs_symbols>-name.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF <fs_lines>-tdformat = '/:'.
      IF ls_parts CS 'ENDIF' OR ls_parts CS 'ELSE'.
        lv_count = lv_count - pa_idtcn.
      ENDIF.
      IF ls_parts CS 'ENDCASE'.
        lv_count = lv_count - ( pa_idtcn * 2 ).
      ENDIF.
      IF ls_parts CS 'WHEN'.
        lv_count = lv_count - pa_idtcn.
      ENDIF.
    ENDIF.

    IF lv_count < 0.
      lv_count = 0.
    ENDIF.

    IF lv_count <> 0.
      DO lv_count TIMES.
        CONCATENATE gv_nobr_space ls_data-adapted INTO ls_data-adapted.
      ENDDO.
      ls_data-level = lv_count / pa_idtcn.
    ENDIF.

    IF <fs_lines>-tdformat = '/:'.
      IF ls_parts = 'IF'.
        lv_count = lv_count + pa_idtcn.
        ls_data-colexpic = icon_collapse.
      ENDIF.
      IF ls_parts = 'ELSE' OR ls_parts = 'ELSE.' OR ls_parts = 'ELSEIF'.
        lv_count = lv_count + pa_idtcn. "3.
      ENDIF.
      IF ls_parts = 'CASE'.
        lv_count = lv_count + ( pa_idtcn * 2 ).
        ls_data-colexpic = icon_collapse.
      ENDIF.
      IF ls_parts = 'WHEN'.
        lv_count = lv_count + pa_idtcn.
      ENDIF.
    ENDIF.

    PERFORM form_stats_collect
            USING    lt_parts
                     ls_parts
                     lv_tabix
            CHANGING ls_data.

    ls_data-row = lv_row.
    APPEND ls_data TO gt_data.
    lv_row = lv_row + 1.
  ENDLOOP.

  PERFORM form_stats_calculate.

  DELETE gt_data WHERE window = '%%DOCU'.

ENDFORM.                    " ALV_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM alv_show CHANGING cv_subrc TYPE sysubrc.

  DATA: ls_layout TYPE lvc_s_layo,
        lt_events TYPE slis_t_event,
        lt_evexit TYPE slis_t_event_exit,
        ls_evexit TYPE slis_event_exit,
        lt_excl   TYPE slis_t_extab,
        ls_vari   TYPE disvariant,
        lv_cbpfs  TYPE slis_formname.

  FIELD-SYMBOLS <fs_events> TYPE slis_alv_event.

  ls_layout-box_fname  = 'MARK'.
  ls_layout-ctab_fname = 'COLOR'.
  ls_layout-stylefname = 'STYLE'.
  ls_layout-col_opt = 'X'.

  ls_vari-report   = sy-repid.
  ls_vari-handle   = 'SFBH'.
  ls_vari-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 4
    IMPORTING
      et_events       = lt_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_events ASSIGNING <fs_events>.
    CASE <fs_events>-name.
      WHEN 'CALLER_EXIT'.
        <fs_events>-form = 'ALV_CALLER_EXIT'.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  ls_evexit-ucomm = '&INFO'.
  ls_evexit-after = 'X'.
  APPEND ls_evexit TO lt_evexit.

  PERFORM alv_exclude_fcodes CHANGING lt_excl.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
      i_bypassing_buffer       = 'X'
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = lv_cbpfs
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = ls_layout
      it_fieldcat_lvc          = gt_fcat
      it_excluding             = lt_excl
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = ls_vari
      it_events                = lt_events
      it_event_exit            = lt_evexit
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    cv_subrc = sy-subrc.
    RETURN.
  ENDIF.

ENDFORM.                    " ALV_SHOW
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.

  DATA: ls_dyntxt TYPE smp_dyntxt,
        lt_excl   TYPE TABLE OF rsexfcode WITH HEADER LINE,
        ls_excl   TYPE rsexfcode.

  sy-title = 'SAPscript forms breakdown helper'.

  gv_tx000 = 'Client'.
  gv_tx001 = 'Form'.
  gv_tx002 = 'Language'.
  gv_tx003 = 'Window'.
  gv_tx004 = 'Text element'.
  gv_tx005 = 'Form selection'.
  gv_tx006 = 'Color selection'.
  gv_tx007 = 'Format 1'.
  gv_tx008 = 'Color'.
  gv_tx009 = 'Format 2'.
  gv_tx010 = 'Color'.
  gv_tx011 = 'Format 3'.
  gv_tx012 = 'Color'.
  gv_tx013 = 'Format 4'.
  gv_tx014 = 'Color'.
  gv_tx015 = 'Layout'.
  gv_tx016 = 'ALV Layout'.
  gv_tx017 = 'Update check'.
  gv_tx019 = 'Color code'.
  gv_tx020 = 'Color code'.
  gv_tx021 = 'Color code'.
  gv_tx022 = 'Color code'.
  gv_tx023 = 'Indentation'.
  gv_tx024 = 'times blank (max. 4)'.

  CONCATENATE 'Your version:' gc_vinfo INTO gv_tx018 SEPARATED BY space.

* no-break space is needed for indentation (ALT+0160)
  gv_nobr_space = cl_abap_conv_in_ce=>uccpi( 160 ).

  CLEAR ls_dyntxt.
  ls_dyntxt-icon_id   = icon_information.
  ls_dyntxt-quickinfo = 'Show documentation'.
  ls_dyntxt-path      = 'T'.
  sscrfields-functxt_01 = ls_dyntxt.

  CLEAR ls_dyntxt.
  ls_dyntxt-text      = 'Form Painter'.
  ls_dyntxt-icon_id   = icon_protocol.
  ls_dyntxt-icon_text = 'Form Painter'.
  ls_dyntxt-quickinfo = 'Call SE71'.
  ls_dyntxt-path      = 'T'.
  sscrfields-functxt_02 = ls_dyntxt.

  CLEAR ls_dyntxt.
  ls_dyntxt-text      = 'Style'.
  ls_dyntxt-icon_id   = icon_align.
  ls_dyntxt-icon_text = 'Styles'.
  ls_dyntxt-quickinfo = 'Call SE72'.
  ls_dyntxt-path      = 'T'.
  sscrfields-functxt_03 = ls_dyntxt.

  CLEAR ls_dyntxt.
  ls_dyntxt-text      = 'Graphics'.
  ls_dyntxt-icon_id   = icon_tif.
  ls_dyntxt-icon_text = 'Graphics'.
  ls_dyntxt-quickinfo = 'Call SE78'.
  ls_dyntxt-path      = 'T'.
  sscrfields-functxt_04 = ls_dyntxt.

  CLEAR ls_dyntxt.
  ls_dyntxt-text      = 'Standard Text'.
  ls_dyntxt-icon_id   = icon_change_text.
  ls_dyntxt-icon_text = 'Standard Text'.
  ls_dyntxt-quickinfo = 'Call SO10'.
  ls_dyntxt-path      = 'T'.
  sscrfields-functxt_05 = ls_dyntxt.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_check
      text                  = 'Check for new version'
      info                  = 'Show blog on SCN'
*     ADD_STDINF            = 'X'
    IMPORTING
      result                = pb_updck
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_display
      text                  = ' '
      info                  = 'Show form'
*     ADD_STDINF            = 'X'
    IMPORTING
      result                = pb_fshow
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_change
      text                  = ' '
      info                  = 'Edit form'
*     ADD_STDINF            = 'X'
    IMPORTING
      result                = pb_fedit
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
  ENDIF.

  PERFORM color_preset.

  PERFORM sscmds_fill.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM at_selection_screen.

  IF sy-dynnr <> '1000'.
    RETURN.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM show_version_info.

    WHEN 'FC02'.
      CALL TRANSACTION 'SE71'.

    WHEN 'FC03'.
      CALL TRANSACTION 'SE72'.

    WHEN 'FC04'.
      CALL TRANSACTION 'SE78'.

    WHEN 'FC05'.
      CALL TRANSACTION 'SO10'.

    WHEN 'UPDCHECK'.
      PERFORM update_check.

    WHEN 'FORM_SHOW'.
      PERFORM form_show_simple.

    WHEN 'FORM_EDIT'.
      PERFORM form_edit_simple.

    WHEN 'CLRP1'. " color preset 1
      pa_colr1 = pa_clrp1.

    WHEN 'CLRP2'. " color preset 2
      pa_colr2 = pa_clrp2.

    WHEN 'CLRP3'. " color preset 3
      pa_colr3 = pa_clrp3.

    WHEN 'CLRP4'. " color preset 4
      pa_colr4 = pa_clrp4.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM at_selection_screen_output.

  LOOP AT SCREEN.
    IF screen-name = 'PA_COLR1'.
      IF pa_clrp1 = 'IND'.
        screen-input = gc_enabled.
      ELSE.
        screen-input = gc_disabled.
      ENDIF.
    ENDIF.

    IF screen-name = 'PA_COLR2'.
      IF pa_clrp2 = 'IND'.
        screen-input = gc_enabled.
      ELSE.
        screen-input = gc_disabled.
      ENDIF.
    ENDIF.

    IF screen-name = 'PA_COLR3'.
      IF pa_clrp3 = 'IND'.
        screen-input = gc_enabled.
      ELSE.
        screen-input = gc_disabled.
      ENDIF.
    ENDIF.

    IF screen-name = 'PA_COLR4'.
      IF pa_clrp4 = 'IND'.
        screen-input = gc_enabled.
      ELSE.
        screen-input = gc_disabled.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  COLOR_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_DATA  text
*----------------------------------------------------------------------*
FORM alv_color_set CHANGING cs_data LIKE LINE OF gt_data.

  DATA: ls_scol  TYPE lvc_s_scol,
        ls_color TYPE lvc_s_colo.

  CLEAR ls_scol.

  IF cs_data-format = pa_form1 AND pa_colr1 IS NOT INITIAL.
    ls_color-col = pa_colr1+0(1).
    ls_color-int = pa_colr1+1(1).
    ls_color-inv = pa_colr1+2(1).
    ls_scol-color = ls_color.
  ENDIF.

  IF cs_data-format = pa_form2.
    ls_color-col = pa_colr2+0(1).
    ls_color-int = pa_colr2+1(1).
    ls_color-inv = pa_colr2+2(1).
    ls_scol-color = ls_color.
  ENDIF.

  IF cs_data-format = pa_form3.
    ls_color-col = pa_colr3+0(1).
    ls_color-int = pa_colr3+1(1).
    ls_color-inv = pa_colr3+2(1).
    ls_scol-color = ls_color.
  ENDIF.

  IF cs_data-format = pa_form4.
    ls_color-col = pa_colr4+0(1).
    ls_color-int = pa_colr4+1(1).
    ls_color-inv = pa_colr4+2(1).
    ls_scol-color = ls_color.
  ENDIF.

  IF ls_scol IS NOT INITIAL.
    APPEND ls_scol TO cs_data-color.
  ENDIF.

ENDFORM.                    " COLOR_SET
*&---------------------------------------------------------------------*
*&      Form  alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_UCOMM     text
*      -->US_SELFIELD  text
*----------------------------------------------------------------------*
FORM alv_user_command USING uv_ucomm    TYPE syucomm
                            us_selfield TYPE slis_selfield.

  DATA ls_stable TYPE lvc_s_stbl.

  FIELD-SYMBOLS <fs_data> LIKE LINE OF gt_data.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = gr_grid.
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =

  IF gr_grid IS NOT BOUND.
    RETURN.
  ENDIF.

  IF us_selfield-tabindex IS NOT INITIAL.
    READ TABLE gt_data ASSIGNING <fs_data> INDEX us_selfield-tabindex.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.

  CALL METHOD gr_grid->check_changed_data.
*   IMPORTING
*     e_valid   =
*   CHANGING
*     c_refresh = 'X'

  CASE uv_ucomm.
    WHEN '&IC1'.
      PERFORM alv_handle_double_click
              CHANGING us_selfield
                       <fs_data>.

    WHEN '&INFO'.
      PERFORM show_version_info.

    WHEN OTHERS.
  ENDCASE.

* refresh output
  IF us_selfield-refresh = gc_yes.
    CLEAR us_selfield-refresh.
    ls_stable-col = 'X'.
    ls_stable-row = 'X'.

    CALL METHOD gr_grid->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = space
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error while refreshing ALV.' TYPE 'I'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  alv_caller_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_DATA    text
*----------------------------------------------------------------------*
FORM alv_caller_exit USING us_data TYPE slis_data_caller_exit.

  DATA: lt_f4        TYPE lvc_t_f4,
        ls_f4        TYPE lvc_s_f4,
        lr_functions TYPE REF TO cl_salv_functions,
        lv_title     TYPE syst_title.

  CONCATENATE gs_header-tdform
              '/'
              gs_header-tdspras
              '/'
              gs_header-mandt INTO lv_title.

  CONCATENATE 'SAPscript forms breakdown helper on'
              lv_title
              INTO sy-title SEPARATED BY space.

*  sy-title = 'SAPscript forms breakdown helper on '.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = gr_grid.
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =

  IF gr_grid IS NOT BOUND.
    RETURN.
  ENDIF.

  CALL METHOD gr_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

  CALL METHOD gr_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  IF sy-subrc <> 0. " without error handling
  ENDIF.

  SET HANDLER lcl_event_handler=>handle_after_user_command
              FOR gr_grid.

  SET HANDLER lcl_event_handler=>handle_context_menu_request
              FOR gr_grid.

  SET HANDLER lcl_event_handler=>handle_onf4
              FOR gr_grid.

  SET HANDLER lcl_event_handler=>handle_user_command
              FOR gr_grid.

ENDFORM.                    "alv_caller_exit
*&---------------------------------------------------------------------*
*&      Form  ALV_HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_US_SELFIELD  text
*      <--P_<FS_DATA>  text
*----------------------------------------------------------------------*
FORM alv_handle_double_click CHANGING cs_sfield TYPE slis_selfield
                                      cs_data   LIKE LINE OF gt_data.

  IF cs_sfield-fieldname = 'ADAPTED' AND cs_data-format = '/:'.
    IF cs_data-original CS 'IF'   OR cs_data-original CS 'ELSEIF' OR
       cs_data-original CS 'ELSE' OR cs_data-original CS 'ENDIF'  OR
       cs_data-original CS 'CASE' OR cs_data-original CS 'WHEN'   OR
       cs_data-original CS 'ENDCASE'.
      PERFORM alv_accentuation
              CHANGING cs_sfield
                       cs_data.
    ELSEIF cs_data-original CS 'INCLUDE'.
      PERFORM form_include_show
              CHANGING cs_sfield
                       cs_data.
    ELSEIF cs_data-original CS 'PERFORM' AND
           cs_data-original NS 'ENDPERFORM'.
      PERFORM form_subroutine_show
              CHANGING cs_sfield
                       cs_data.
    ENDIF.
  ELSEIF cs_sfield-fieldname = 'COLEXPIC' AND
         cs_data-colexpic = icon_collapse.
    PERFORM alv_collapse
            CHANGING cs_sfield
                     cs_data.
  ELSEIF cs_sfield-fieldname = 'COLEXPIC' AND
         cs_data-colexpic = icon_expand.
    PERFORM alv_expand
            CHANGING cs_sfield
                     cs_data.
  ENDIF.

ENDFORM.                    " ALV_HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  MARK_BLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_SFIELD  text
*      <--P_CS_DATA  text
*----------------------------------------------------------------------*
FORM alv_accentuation CHANGING cs_sfield TYPE slis_selfield
                               cs_data   LIKE LINE OF gt_data.

  DATA: ls_style TYPE lvc_s_styl,
        lv_tabix TYPE sytabix,
        lv_level TYPE i,
        lv_case  TYPE xfeld,
        ls_mark  LIKE LINE OF gt_mark.

  FIELD-SYMBOLS: <fs_data> LIKE LINE OF gt_data,
                 <fs_mark> LIKE LINE OF gt_mark.

* use current table index as starting point
  IF cs_data-original CS 'IF' OR
     ( cs_data-original CS 'CASE' AND cs_data-original NS 'ENDCASE' ).
    lv_tabix = cs_sfield-tabindex.
    lv_level = cs_data-level.
    IF cs_data-original CS 'CASE'.
      lv_case = gc_yes.
    ENDIF.
  ENDIF.

* search for starting point backwards
  IF cs_data-original CS 'ELSEIF' OR
     cs_data-original CS 'ELSE'   OR
     cs_data-original CS 'ENDIF'  OR
     cs_data-original CS 'WHEN'   OR
     cs_data-original CS 'ENDCASE'.

    lv_tabix = cs_sfield-tabindex.
    lv_level = cs_data-level.

    IF cs_data-original CS 'WHEN' OR cs_data-original CS 'ENDCASE'.
      lv_case = gc_yes.
    ENDIF.

    DO.
      READ TABLE gt_data ASSIGNING <fs_data> INDEX lv_tabix.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF lv_case = gc_no AND <fs_data>-level <> lv_level.
        lv_tabix = lv_tabix - 1.
        CONTINUE.
      ENDIF.

      IF lv_case = gc_yes AND <fs_data>-level < lv_level - 1.
        lv_tabix = lv_tabix - 1.
        CONTINUE.
      ENDIF.

      IF cs_data-original CS 'ELSEIF' OR
         cs_data-original CS 'ELSE'   OR
         cs_data-original CS 'ENDIF'.
        IF <fs_data>-original CS 'IF' AND
           <fs_data>-original NS 'ENDIF' AND
           <fs_data>-original NS 'ELSEIF'.
          lv_level = <fs_data>-level.
          EXIT.
        ENDIF.
      ENDIF.

      IF cs_data-original CS 'WHEN' OR cs_data-original CS 'ENDCASE'.
        IF <fs_data>-original CS 'CASE' AND
           <fs_data>-original NS 'ENDCASE'.
          lv_level = <fs_data>-level.
          lv_case = gc_yes.
          EXIT.
        ENDIF.
      ENDIF.

      lv_tabix = lv_tabix - 1.
    ENDDO.
  ENDIF.

  IF lv_tabix IS INITIAL.
    MESSAGE 'Block not detected.' TYPE 'I'.
    RETURN.
  ENDIF.

* clear old marks
  IF gt_mark[] IS NOT INITIAL.
    LOOP AT gt_mark ASSIGNING <fs_mark>.
      READ TABLE gt_data ASSIGNING <fs_data> INDEX <fs_mark>-tabix.
      IF sy-subrc = 0.
        CLEAR <fs_data>-style.
      ENDIF.
    ENDLOOP.
  ENDIF.

* mark key words
  LOOP AT gt_data ASSIGNING <fs_data> FROM lv_tabix.
    ls_mark-tabix = sy-tabix.

    IF lv_case = gc_no AND <fs_data>-level <> lv_level.
      CONTINUE.
    ENDIF.

    IF lv_case = gc_yes AND <fs_data>-level > lv_level + 1.
      CONTINUE.
    ENDIF.

    IF <fs_data>-original CS 'IF'     OR
       <fs_data>-original CS 'ELSEIF' OR
       <fs_data>-original CS 'ELSE'   OR
       <fs_data>-original CS 'ENDIF'  OR
       <fs_data>-original CS 'CASE'   OR
       <fs_data>-original CS 'WHEN'   OR
       <fs_data>-original CS 'ENDCASE'.

      CLEAR: ls_style,
             <fs_data>-style.

      ls_style-fieldname = 'ADAPTED'.
      ls_style-style     = alv_style_font_bold.
      APPEND ls_style TO <fs_data>-style.
      APPEND ls_mark TO gt_mark.
      IF cs_sfield-refresh = gc_no.
        cs_sfield-refresh = gc_yes.
      ENDIF.
    ENDIF.

    IF <fs_data>-original CS 'ENDIF' OR
       <fs_data>-original CS 'ENDCASE'.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_VERSION_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_version_info.

  DATA: lv_text TYPE text80.

  CONCATENATE 'Version' gc_vinfo INTO lv_text SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = 'Version information'
      txt1  = 'SAPscript forms breakdown helper'
      txt2  = lv_text
      txt3  = 'written by Michael Keller'
      txt4  = 'use it, improve it, share it ...'.

ENDFORM.                    " SHOW_VERSION_INFO
*&---------------------------------------------------------------------*
*&      Form  ALV_EXCLUDE_FCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCL  text
*----------------------------------------------------------------------*
FORM alv_exclude_fcodes CHANGING ct_excl TYPE slis_t_extab.

  DATA ls_excl TYPE slis_extab.

  ls_excl-fcode = '&ABC'.
  APPEND ls_excl TO ct_excl.

  ls_excl-fcode = '&GRAPH'.
  APPEND ls_excl TO ct_excl.

  ls_excl-fcode = '&AQW'.
  APPEND ls_excl TO ct_excl.

  ls_excl-fcode = '%SL'.
  APPEND ls_excl TO ct_excl.

  ls_excl-fcode = '&VEXCEL'.
  APPEND ls_excl TO ct_excl.

ENDFORM.                    " ALV_EXCLUDE_FCODES

*&---------------------------------------------------------------------*
*&      Form  alv_variant_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_variant_f4.

  DATA ls_varia TYPE disvariant.

  ls_varia-report   = sy-repid.
  ls_varia-handle   = 'SFBH'.
  ls_varia-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_varia
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
      i_save        = 'A'
*     I_DISPLAY_VIA_GRID  = ' '
    IMPORTING
*     E_EXIT        =
      es_variant    = ls_varia
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    pa_varia = ls_varia-variant.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  FORMULAR_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_f4.

  DATA: lv_fname  TYPE tdform,
        lv_flang  TYPE spras,
        lt_dynpf  TYPE TABLE OF dynpread,
        ls_dynpf  TYPE dynpread,
        lv_answer TYPE char1.

* user decides which f4 help
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'F4-Help selection'
*     DIAGNOSE_OBJECT       = ' '
      text_question         = 'Please choose your F4-Help.'
      text_button_1         = 'Properties'
      icon_button_1         = '@3D@'
      text_button_2         = 'Tree'
      icon_button_2         = '@JG@'
*     DEFAULT_BUTTON        = '1'
*     DISPLAY_CANCEL_BUTTON = 'X'
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
      iv_quickinfo_button_1 = 'Select by properties'
      iv_quickinfo_button_2 = 'Select by tree'
    IMPORTING
      answer                = lv_answer
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error showing F4-Help selection.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF lv_answer = 'A'.
    MESSAGE 'F4-Help selection aborted.' TYPE 'S'.
    RETURN.
  ENDIF.

  IF lv_answer = '1'. " by properties
    SUBMIT rstxfcat VIA SELECTION-SCREEN AND RETURN.
    GET PARAMETER ID 'TXF' FIELD lv_fname.
    GET PARAMETER ID 'TXL' FIELD lv_flang.
  ELSEIF lv_answer = '2'. " by tree
    CALL FUNCTION 'DISPLAY_FORM_TREE_F4'
      EXPORTING
        p_tree_name     = 'SAP_ALL'
        p_display_mode  = 'D'
*       I_FORM_NAME     =
      IMPORTING
        p_form_name     = lv_fname
        p_form_language = lv_flang
      EXCEPTIONS
        cancelled       = 1
        parameter_error = 2
        not_found       = 3
        OTHERS          = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.

  ls_dynpf-fieldname  = 'PA_FLANG'.
  ls_dynpf-stepl      = sy-stepl.
  ls_dynpf-fieldvalue = lv_flang.
  APPEND ls_dynpf TO lt_dynpf.

  ls_dynpf-fieldname  = 'PA_FNAME'.
  ls_dynpf-stepl      = sy-stepl.
  ls_dynpf-fieldvalue = lv_fname.
  APPEND ls_dynpf TO lt_dynpf.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpf
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.                    " FORMULAR_F4
*&---------------------------------------------------------------------*
*&      Form  WINDOW_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_window_f4.

  TYPES: BEGIN OF values,
           wname TYPE tdwindow,
         END OF values.

  DATA: lt_values TYPE TABLE OF values,
        ls_values TYPE values,
        lt_return TYPE TABLE OF ddshretval,
        lv_subrc  TYPE sysubrc.

  FIELD-SYMBOLS <fs_lines> TYPE tline.

  CLEAR gt_lines.

  PERFORM form_read
          USING    gc_yes
          CHANGING lv_subrc.

  IF lv_subrc <> 0 OR gt_lines IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_lines ASSIGNING <fs_lines> WHERE tdformat = '/W'
                                        AND   tdline <> '%%DOCU'.
    ls_values = <fs_lines>-tdline.
    APPEND ls_values TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'WNAME'
*     PVALKEY         = ' '
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'PA_WNAME'
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
*   IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = lt_values
*     FIELD_TAB       =
      return_tab      = lt_return
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.                    " WINDOW_F4
*&---------------------------------------------------------------------*
*&      Form  FORMULAR_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CV_SUBRC  text
*      <--P_LT_LINES  text
*----------------------------------------------------------------------*
FORM form_read USING    uv_selsc TYPE xfeld
               CHANGING cv_subrc TYPE sysubrc.

  DATA: lt_fields TYPE TABLE OF dynpread,
        ls_fields TYPE dynpread.

  IF uv_selsc = gc_yes.
*   get current values from selection screen
    ls_fields-fieldname = 'PA_MANDT'.
    APPEND ls_fields TO lt_fields.

    ls_fields-fieldname = 'PA_FNAME'.
    APPEND ls_fields TO lt_fields.

    ls_fields-fieldname = 'PA_FLANG'.
    APPEND ls_fields TO lt_fields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = '1000'
*       TRANSLATE_TO_UPPER   = ' '
*       REQUEST              = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX = ' '
*       START_SEARCH_IN_CURRENT_SCREEN = ' '
*       START_SEARCH_IN_MAIN_SCREEN    = ' '
*       START_SEARCH_IN_STACKED_SCREEN = ' '
*       START_SEARCH_ON_SCR_STACKPOS   = ' '
*       SEARCH_OWN_SUBSCREENS_FIRST    = ' '
*       SEARCHPATH_OF_SUBSCREEN_AREAS  = ' '
      TABLES
        dynpfields           = lt_fields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.

    IF sy-subrc <> 0.
      cv_subrc = sy-subrc.
      RETURN.
    ENDIF.

    LOOP AT lt_fields INTO ls_fields.
      IF ls_fields-fieldname = 'PA_MANDT'.
        pa_mandt = ls_fields-fieldvalue.
      ELSEIF ls_fields-fieldname = 'PA_FNAME'.
        pa_fname = ls_fields-fieldvalue.
        TRANSLATE pa_fname TO UPPER CASE.
      ELSEIF ls_fields-fieldname = 'PA_FLANG'.
        pa_flang = ls_fields-fieldvalue.
        TRANSLATE pa_flang TO UPPER CASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = pa_mandt
      id                      = 'TXT'
      language                = pa_flang
      name                    = pa_fname
      object                  = 'FORM'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
    IMPORTING
      header                  = gs_header
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = gt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CASE sy-subrc.
    WHEN 0.

    WHEN 4.
      cv_subrc = sy-subrc.
      MESSAGE 'Form not found.' TYPE 'I'.
      RETURN.

    WHEN OTHERS.
      cv_subrc = sy-subrc.
      MESSAGE 'Error while reading form.' TYPE 'I'.
      RETURN.
  ENDCASE.

ENDFORM.                    " FORMULAR_READ
*&---------------------------------------------------------------------*
*&      Form  FORMULAR_ELEMENT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_text_element_f4.

  TYPES: BEGIN OF values,
           wname TYPE tdwindow,
           tname TYPE tdline,
         END OF values.

  DATA: lt_values TYPE TABLE OF values,
        ls_values TYPE values,
        lt_return TYPE TABLE OF ddshretval,
        lv_subrc  TYPE sysubrc,
        lv_tabix  TYPE sytabix,
        lt_dynpf  TYPE TABLE OF dynpread,
        ls_dynpf  TYPE dynpread,
        lt_fields TYPE TABLE OF dfies.

  FIELD-SYMBOLS <fs_lines> TYPE tline.

  CLEAR gt_lines.

  PERFORM form_read
          USING    gc_yes
          CHANGING lv_subrc.

  IF lv_subrc <> 0 OR gt_lines IS INITIAL.
    RETURN.
  ENDIF.

  ls_dynpf-fieldname = 'PA_WNAME'.
  APPEND ls_dynpf TO lt_dynpf.

* get current values from selection screen
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = '1000'
*     TRANSLATE_TO_UPPER   = ' '
*     REQUEST              = ' '
*     PERFORM_CONVERSION_EXITS       = ' '
*     PERFORM_INPUT_CONVERSION       = ' '
*     DETERMINE_LOOP_INDEX = ' '
*     START_SEARCH_IN_CURRENT_SCREEN = ' '
*     START_SEARCH_IN_MAIN_SCREEN    = ' '
*     START_SEARCH_IN_STACKED_SCREEN = ' '
*     START_SEARCH_ON_SCR_STACKPOS   = ' '
*     SEARCH_OWN_SUBSCREENS_FIRST    = ' '
*     SEARCHPATH_OF_SUBSCREEN_AREAS  = ' '
    TABLES
      dynpfields           = lt_dynpf
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lt_dynpf INTO ls_dynpf
                       WITH KEY fieldname = 'PA_WNAME'.

  IF sy-subrc = 0 AND ls_dynpf-fieldvalue IS NOT INITIAL.
    TRANSLATE ls_dynpf-fieldvalue TO UPPER CASE.
    READ TABLE gt_lines TRANSPORTING NO FIELDS
                        WITH KEY tdformat = '/W'
                                 tdline   = ls_dynpf-fieldvalue.

    IF sy-subrc = 0.
      lv_tabix = sy-tabix + 1.
    ELSE.
      MESSAGE 'Window not found.' TYPE 'I'.
      RETURN.
    ENDIF.

*   text elements of a specific window
    ls_values-wname = ls_dynpf-fieldvalue.
    LOOP AT gt_lines ASSIGNING <fs_lines> FROM lv_tabix
                                          WHERE tdformat = '/W'
                                          OR    tdformat = '/E'.
      IF <fs_lines>-tdformat = '/E'.
        ls_values-tname = <fs_lines>-tdline.
        APPEND ls_values TO lt_values.
      ELSEIF <fs_lines>-tdformat = '/W'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
*   all text elements of form
    LOOP AT gt_lines ASSIGNING <fs_lines> WHERE tdformat = '/W'
                                          OR    tdformat = '/E'.

      IF <fs_lines>-tdformat = '/W'.
        ls_values-wname = <fs_lines>-tdline.
      ENDIF.

      IF <fs_lines>-tdformat = '/E'.
        ls_values-tname = <fs_lines>-tdline.
        APPEND ls_values TO lt_values.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA: lt_dselc TYPE TABLE OF dselc,
        ls_dselc TYPE dselc.

  ls_dselc-fldname = 'F0001'.
  ls_dselc-dyfldname = 'PA_WNAME'.
  APPEND ls_dselc TO lt_dselc.

  ls_dselc-fldname = 'F0002'.
  ls_dselc-dyfldname = 'PA_TNAME'.
  APPEND ls_dselc TO lt_dselc.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'TNAME'
*     PVALKEY         = ' '
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'PA_TNAME'
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
*   IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = lt_values
      field_tab       = lt_fields
      return_tab      = lt_return
      dynpfld_mapping = lt_dselc
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.                    " FORMULAR_ELEMENT_F4
*&---------------------------------------------------------------------*
*&      Form  ALV_COPY_TO_CLIPBOARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_copy_to_clipboard.

  DATA: lt_data  TYPE TABLE OF tdline,
        ls_data  TYPE tdline,
        lv_rc    TYPE i,
        lv_lines TYPE i,
        lv_msg   TYPE text50.

  FIELD-SYMBOLS <fs_data> LIKE LINE OF gt_data.

  LOOP AT gt_data ASSIGNING <fs_data> WHERE mark = gc_yes.
    CLEAR ls_data.
    ls_data = <fs_data>-adapted.
    APPEND ls_data TO lt_data.
  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE 'Please mark one or more lines.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF lt_data IS INITIAL.
    MESSAGE 'No lines were found to copy.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>clipboard_export
*   EXPORTING
*     no_auth_check        = SPACE
    IMPORTING
      data                 = lt_data
    CHANGING
      rc                   = lv_rc
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      no_authority         = 4
      OTHERS               = 5.

  IF sy-subrc <> 0 OR lv_rc <> 0.
    MESSAGE 'An error occured.' TYPE 'I'.
  ENDIF.

  lv_lines = lines( lt_data ).
  WRITE lv_lines TO lv_msg LEFT-JUSTIFIED.

  IF lv_lines = 1.
    CONCATENATE lv_msg 'line was copied to clipboard.'
                INTO lv_msg SEPARATED BY space.
  ELSEIF lv_lines > 1.
    CONCATENATE lv_msg 'lines were copied to clipboard.'
                INTO lv_msg SEPARATED BY space.
  ENDIF.

  MESSAGE lv_msg TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_COLLAPSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_SFIELD  text
*      <--P_CS_DATA  text
*----------------------------------------------------------------------*
FORM alv_collapse CHANGING cs_sfield TYPE slis_selfield
                           cs_data   LIKE LINE OF gt_data.

  DATA: lt_filter TYPE lvc_t_filt,
        ls_filter TYPE lvc_s_filt,
        lv_rint   TYPE qfranint,
        lv_rnumc  TYPE numc4.

  FIELD-SYMBOLS <fs_data> LIKE LINE OF gt_data.

  IF gr_grid IS INITIAL.
    MESSAGE 'Error while collapsing rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL METHOD gr_grid->get_filter_criteria
    IMPORTING
      et_filter = lt_filter.

* create individual value for filter
  CALL FUNCTION 'QF05_RANDOM_INTEGER'
    EXPORTING
      ran_int_max   = 1024
      ran_int_min   = 1
    IMPORTING
      ran_int       = lv_rint
    EXCEPTIONS
      invalid_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error while collapsing rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  lv_rnumc = lv_rint.
  CONCATENATE sy-uzeit '-' lv_rnumc INTO ls_filter-low.

  LOOP AT gt_data ASSIGNING <fs_data> FROM cs_sfield-tabindex.
    IF sy-tabix = cs_sfield-tabindex. " not to filter
      CONCATENATE 'B-' ls_filter-low INTO <fs_data>-colexpfn.
    ELSE. " filter
      IF <fs_data>-colexpfn IS NOT INITIAL.
*       delete older filter if exists
        READ TABLE lt_filter TRANSPORTING NO FIELDS
                             WITH KEY low = <fs_data>-colexpfn.
        IF sy-subrc = 0.
          DELETE lt_filter INDEX sy-tabix.
        ENDIF.
        IF <fs_data>-colexpic = icon_expand.
          <fs_data>-colexpic = icon_collapse.
        ENDIF.
      ENDIF.

*     set new value for filter
      CONCATENATE 'F-' ls_filter-low INTO <fs_data>-colexpfn.

      IF <fs_data>-format = '/:' AND <fs_data>-level = cs_data-level.
        IF cs_data-original CS 'IF' AND
           <fs_data>-original CS 'ENDIF'.
          EXIT.
        ENDIF.
        IF cs_data-original CS 'CASE' AND
           <fs_data>-original CS 'ENDCASE'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE 'Error while collapsing rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  ls_filter-tabname   = 'GT_DATA'.
  ls_filter-fieldname = 'COLEXPFN'.
  ls_filter-ref_field = 'COLEXPFN'.
  ls_filter-sign      = 'E'.
  ls_filter-option    = 'EQ'.
  CONCATENATE 'F-' ls_filter-low INTO ls_filter-low.
  ls_filter-order = lines( lt_filter ) + 1.
  APPEND ls_filter TO lt_filter.

  CALL METHOD gr_grid->set_filter_criteria
    EXPORTING
      it_filter                 = lt_filter
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error while collapsing rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  cs_sfield-refresh = gc_yes.
  cs_data-colexpic = icon_expand.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_EXPAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_SFIELD  text
*      <--P_CS_DATA  text
*----------------------------------------------------------------------*
FORM alv_expand CHANGING cs_sfield TYPE slis_selfield
                         cs_data   LIKE LINE OF gt_data.

  DATA: lt_filter TYPE lvc_t_filt,
        ls_filter TYPE lvc_s_filt,
        ls_data   LIKE LINE OF gt_data.

  IF cs_data-colexpfn IS INITIAL.
    MESSAGE 'Error while expanding rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  ls_filter-low = cs_data-colexpfn.
  ls_filter-low+0(1) = 'F'.
  CLEAR cs_data-colexpfn.

  CALL METHOD gr_grid->get_filter_criteria
    IMPORTING
      et_filter = lt_filter.

  DELETE lt_filter WHERE low = ls_filter-low.
  IF sy-subrc <> 0.
    MESSAGE 'Error while expanding rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL METHOD gr_grid->set_filter_criteria
    EXPORTING
      it_filter                 = lt_filter
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error while expanding rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  MODIFY gt_data FROM ls_data
                 TRANSPORTING colexpfn
                 WHERE colexpfn = ls_filter-low.

  IF sy-subrc <> 0.
    MESSAGE 'Error while expanding rows.' TYPE 'I'.
    RETURN.
  ENDIF.

  cs_sfield-refresh = gc_yes.
  cs_data-colexpic = icon_collapse.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_check.

  DATA lv_url TYPE string.

* build url to open blog on scn via internet browser
  CONCATENATE gc_burl1 gc_burl2 gc_burl3 INTO lv_url.

  CALL METHOD cl_gui_frontend_services=>execute
    EXPORTING
      document               = lv_url
*     application            =
*     parameter              =
*     default_directory      =
*     maximized              =
*     minimized              =
*     synchronous            =
*     operation              = 'OPEN'
    EXCEPTIONS
      cntl_error             = 1
      error_no_gui           = 2
      bad_parameter          = 3
      file_not_found         = 4
      path_not_found         = 5
      file_extension_unknown = 6
      error_execute_failed   = 7
      synchronous_failed     = 8
      not_supported_by_gui   = 9
      OTHERS                 = 10.

  IF sy-subrc <> 0.
    MESSAGE 'Failed to show blog via internet browser.' TYPE 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COLOR_PRESET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM color_preset.

  DATA: ls_values TYPE vrm_value,
        lt_values TYPE TABLE OF vrm_value,
        lt_id     TYPE TABLE OF vrm_id,
        ls_id     TYPE vrm_id.

  ls_values-key = '100'.
  ls_values-text = 'grey blue'.
  APPEND ls_values TO lt_values.

  ls_values-key = '200'.
  ls_values-text = 'light grey'.
  APPEND ls_values TO lt_values.

  ls_values-key = '210'.
  ls_values-text = 'grey'.
  APPEND ls_values TO lt_values.

  ls_values-key = '300'.
  ls_values-text = 'light yellow'.
  APPEND ls_values TO lt_values.

  ls_values-key = '310'.
  ls_values-text = 'yellow'.
  APPEND ls_values TO lt_values.

  ls_values-key = '410'.
  ls_values-text = 'blue'.
  APPEND ls_values TO lt_values.

  ls_values-key = '500'.
  ls_values-text = 'light green'.
  APPEND ls_values TO lt_values.

  ls_values-key = '510'.
  ls_values-text = 'green'.
  APPEND ls_values TO lt_values.

  ls_values-key = '600'.
  ls_values-text = 'light red'.
  APPEND ls_values TO lt_values.

  ls_values-key = '610'.
  ls_values-text = 'red'.
  APPEND ls_values TO lt_values.

  ls_values-key = '700'.
  ls_values-text = 'light orange'.
  APPEND ls_values TO lt_values.

  ls_values-key = '710'.
  ls_values-text = 'orange'.
  APPEND ls_values TO lt_values.

  ls_values-key = 'IND'.
  ls_values-text = 'individual'.
  APPEND ls_values TO lt_values.

  ls_id = 'PA_CLRP1'.
  APPEND ls_id TO lt_id.

  ls_id = 'PA_CLRP2'.
  APPEND ls_id TO lt_id.

  ls_id = 'PA_CLRP3'.
  APPEND ls_id TO lt_id.

  ls_id = 'PA_CLRP4'.
  APPEND ls_id TO lt_id.

  LOOP AT lt_id INTO ls_id.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = ls_id
        values          = lt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0768   text
*----------------------------------------------------------------------*
FORM form_edit USING uv_type TYPE char1
                     us_data LIKE LINE OF gt_data.

  DATA: lv_subrc TYPE sysubrc,
        lt_bdcdt TYPE bdcdata OCCURS 20,
        ls_bdcdt TYPE bdcdata,
        lt_msgs  TYPE bdcmsgcoll OCCURS 10,
        lv_wname TYPE tdwindow,
        lv_row   TYPE tdrow,
        lv_line  TYPE txlinenr,
        lt_fcont TYPE dyfatc_tab,
        ls_fcont TYPE rpy_dyfatc,
        lv_gedit TYPE tdlineed,
        lv_fpain TYPE tdfpainter.

* check if batch input will work (depends on SAP Basis release
* and SE71 functions)
  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = 'SAPMSSCF'
      dynnr                = '1102'
*     SUPPRESS_EXIST_CHECKS       = ' '
*     SUPPRESS_CORR_CHECKS = ' '
*   IMPORTING
*     HEADER               =
    TABLES
*     CONTAINERS           =
      fields_to_containers = lt_fcont
*     FLOW_LOGIC           =
*     PARAMS               =
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE 'Checks on dynpro failed.' TYPE 'I'.
    RETURN.
  ENDIF.

  READ TABLE lt_fcont INTO ls_fcont
                      WITH KEY cont_type = 'RADIOGROUP'
                               type      = 'RADIO'
                               name      = 'RSSCF-TDWINOB'.

  IF sy-subrc <> 0 OR ls_fcont-invisible IS NOT INITIAL.
    MESSAGE 'Function is currently not supported.' TYPE 'I'.
    RETURN.
  ENDIF.

* check if SAPscript editor is active and form painter is not
  SELECT SINGLE gra_fpaint gra_editor
         FROM rseumod
         INTO ( lv_fpain, lv_gedit )
         WHERE uname = sy-uname.

  IF sy-subrc <> 0.
    MESSAGE 'Prechecks failed.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF lv_gedit = gc_no.
    MESSAGE 'Please use SAPscript editor as standard editor.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF lv_fpain = gc_yes.
    MESSAGE 'Please do not use form painter.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF us_data-window IS INITIAL.
    IF us_data-format = '/W'.
      lv_wname = us_data-original.
    ENDIF.
  ELSE.
    lv_wname = us_data-window.
  ENDIF.

  IF lv_wname IS INITIAL.
    MESSAGE 'Please select a line with a window.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF us_data-row IS NOT INITIAL.
    lv_line = us_data-row.
  ENDIF.

  bdc-add gc_yes 'SAPMSSCF' '1102'.
  bdc-add space  'BDC_CURSOR' 'RSSCF-TDFORM'.
  bdc-add space  'BDC_OKCODE' '=EDIT'.
  bdc-add space  'RSSCF-TDFORM' pa_fname.
  bdc-add space  'RSSCF-TDSPRAS' pa_flang.
  bdc-add space  'RSSCF-TDWINOB' gc_yes.

  bdc-add gc_yes 'SAPMSSCF' '1140'.
  bdc-add space  'BDC_CURSOR' 'ITCTW-TDWINDOW'.
  bdc-add space  'BDC_OKCODE' '/00'.
  bdc-add space  'ITCTW-TDWINDOW' lv_wname.

  bdc-add gc_yes 'SAPMSSCF' '1140'.
  bdc-add space  'BDC_CURSOR' 'ITCTW-TDWINDOW'.
  bdc-add space  'BDC_OKCODE' '=WIED'.

  bdc-add gc_yes 'SAPLSTXX' '1100'.
  bdc-add space  'BDC_CURSOR' 'RSTXT-TXLINE(00)'.
  bdc-add space  'BDC_OKCODE' '=EDPO'.

  bdc-add gc_yes 'SAPLSTXX' '1155'.
  bdc-add space  'BDC_CURSOR' 'RSTXT-TXLINENR'.

  IF lv_line IS NOT INITIAL.
    bdc-add space  'RSTXT-TXLINENR' lv_line.
  ELSE.
    bdc-add space  'RSTXT-TXLINENR' '1'.
  ENDIF.

  IF uv_type = '1'.
*   in same window
    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      EXPORTING
        tcode                   = 'SE71'
*       SKIP_SCREEN             = ' '
        mode_val                = 'E'
*       UPDATE_VAL              = 'A'
      IMPORTING
        subrc                   = lv_subrc
      TABLES
        using_tab               = lt_bdcdt
*       SPAGPA_TAB              =
        mess_tab                = lt_msgs
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        OTHERS                  = 3.
  ELSEIF uv_type = '2'.
*   open new window
    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'ZSFBH'
      EXPORTING
        tcode                   = 'SE71'
*       SKIP_SCREEN             = ' '
        mode_val                = 'E'
*       UPDATE_VAL              = 'A'
*      IMPORTING
*       subrc                   =
      TABLES
        using_tab               = lt_bdcdt
*       SPAGPA_TAB              =
        mess_tab                = lt_msgs
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        OTHERS                  = 3.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE 'An error occured while starting SE71.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF uv_type = '2'.
    MESSAGE 'Transaction SE71 started in new window.' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_EDIT_EASY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_edit_simple.

  DATA: lt_bdcdt TYPE bdcdata OCCURS 20,
        ls_bdcdt TYPE bdcdata.

  IF pa_fname IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE tdname
         FROM stxh CLIENT SPECIFIED
         INTO pa_fname
         WHERE mandt  = pa_mandt
         AND tdobject = 'FORM'
         AND tdname   = pa_fname
         AND tdid     = 'TXT'
         AND tdspras  = pa_flang.

  IF sy-subrc <> 0.
    MESSAGE 'Form does not exist.' TYPE 'I'.
    RETURN.
  ENDIF.

  bdc-add gc_yes 'SAPMSSCF' '1102'.
  bdc-add space  'BDC_CURSOR' 'RSSCF-TDFORM'.
  bdc-add space  'BDC_OKCODE' '=EDIT'.
  bdc-add space  'RSSCF-TDFORM' pa_fname.
  bdc-add space  'RSSCF-TDSPRAS' pa_flang.
  bdc-add space  'RSSCF-TDHEADEROB' gc_yes.

  CALL TRANSACTION 'SE71' USING lt_bdcdt MODE 'E'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_SHOW_SIMPLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_show_simple.

  DATA: lt_bdcdt TYPE bdcdata OCCURS 20,
        ls_bdcdt TYPE bdcdata.

  IF pa_fname IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE tdname
         FROM stxh CLIENT SPECIFIED
         INTO pa_fname
         WHERE mandt  = pa_mandt
         AND tdobject = 'FORM'
         AND tdname   = pa_fname
         AND tdid     = 'TXT'
         AND tdspras  = pa_flang.

  IF sy-subrc <> 0.
    MESSAGE 'Form does not exist.' TYPE 'I'.
    RETURN.
  ENDIF.

  bdc-add gc_yes 'SAPMSSCF' '1102'.
  bdc-add space  'BDC_CURSOR' 'RSSCF-TDFORM'.
  bdc-add space  'BDC_OKCODE' '=SHOW'.
  bdc-add space  'RSSCF-TDFORM' pa_fname.
  bdc-add space  'RSSCF-TDSPRAS' pa_flang.
  bdc-add space  'RSSCF-TDHEADEROB' gc_yes.

  CALL TRANSACTION 'SE71' USING lt_bdcdt MODE 'E'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRECHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM prechecks CHANGING cv_subrc TYPE sysubrc.

  IF pa_idtcn < 1 OR pa_idtcn > 4.
    MESSAGE 'Please use 1 to 4 blanks for indentation only.' TYPE 'I'.
    cv_subrc = gc_rc_error.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_INCLUDE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_SFIELD  text
*      <--P_CS_DATA  text
*----------------------------------------------------------------------*
FORM form_include_show CHANGING cs_sfield TYPE slis_selfield
                                cs_data   LIKE LINE OF gt_data.

  DATA: lt_parts TYPE TABLE OF tdline,
        ls_parts TYPE tdline,
        lv_tabix TYPE sytabix,
        lv_name  TYPE tdobname,
        lv_id    TYPE tdid,
        lv_spras TYPE tdspras,
        lt_bdcdt TYPE bdcdata OCCURS 20,
        ls_bdcdt TYPE bdcdata.

  TRANSLATE cs_data-original TO UPPER CASE.

  SPLIT cs_data-original AT space INTO TABLE lt_parts.
  IF sy-subrc <> 0.
    MESSAGE 'INCLUDE not parsed.' TYPE 'I'.
    RETURN.
  ENDIF.

* find name of include
  READ TABLE lt_parts TRANSPORTING NO FIELDS
                      WITH KEY table_line = 'INCLUDE'.

  IF sy-subrc <> 0.
    MESSAGE 'INCLUDE not parsed.' TYPE 'I'.
    RETURN.
  ELSE.
    lv_tabix = sy-tabix + 1.
    READ TABLE lt_parts INTO ls_parts INDEX lv_tabix.
    IF sy-subrc <> 0.
      MESSAGE 'INCLUDE not parsed.' TYPE 'I'.
      RETURN.
    ELSE.
*     check if dynamic include
      IF ls_parts CA '&'.
        MESSAGE 'INCLUDE is dynamic.' TYPE 'I'.
        RETURN.
      ENDIF.

*     check if name consists of different parts
      IF ls_parts CA ''''.
        DO.
          CONCATENATE lv_name ls_parts INTO lv_name
                                       SEPARATED BY space.

          lv_tabix = lv_tabix + 1.
          READ TABLE lt_parts INTO ls_parts INDEX lv_tabix.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          IF ls_parts CA ''''.
            CONCATENATE lv_name ls_parts INTO lv_name
                                         SEPARATED BY space.
            EXIT.
          ENDIF.
        ENDDO.
      ELSE.
        lv_name = ls_parts.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH space.
      SHIFT lv_name LEFT DELETING LEADING space.
    ENDIF.
  ENDIF.

* find id
  READ TABLE lt_parts TRANSPORTING NO FIELDS
                      WITH KEY table_line = 'ID'.

  IF sy-subrc <> 0.
    lv_id = 'ST'.
  ELSE.
    lv_tabix = sy-tabix + 1.
    READ TABLE lt_parts INTO ls_parts INDEX lv_tabix.
    IF sy-subrc <> 0.
      lv_id = gs_header-tdid.
    ELSE.
      REPLACE ALL OCCURRENCES OF '''' IN ls_parts WITH space.
      SHIFT ls_parts LEFT DELETING LEADING space.
      lv_id = ls_parts.
    ENDIF.
  ENDIF.

* find language
  READ TABLE lt_parts TRANSPORTING NO FIELDS
                      WITH KEY table_line = 'LANGUAGE'.

  IF sy-subrc <> 0.
    lv_spras = gs_header-tdspras.
  ELSE.
    lv_tabix = sy-tabix + 1.
    READ TABLE lt_parts INTO ls_parts INDEX lv_tabix.
    IF sy-subrc <> 0.
    ELSE.
      REPLACE ALL OCCURRENCES OF '''' IN ls_parts WITH space.
      SHIFT ls_parts LEFT DELETING LEADING space.
      lv_spras = ls_parts.
    ENDIF.
  ENDIF.

  bdc-add gc_yes 'SAPMSSCE' '1100'.
  bdc-add space  'BDC_CURSOR' 'RSSCE-TDNAME'.
  bdc-add space  'BDC_OKCODE' ''.
  bdc-add space  'RSSCE-TDNAME' lv_name.
  bdc-add space  'RSSCE-TDID' lv_id.
  bdc-add space  'RSSCE-TDSPRAS' lv_spras.

  CALL TRANSACTION 'SO10' USING lt_bdcdt MODE 'E'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COMPLEXITY_MEASURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DATA  text
*----------------------------------------------------------------------*
FORM form_stats_collect USING    ut_parts TYPE tsfotabl
                                 us_parts TYPE tdline
                                 uv_tabix TYPE sytabix
                        CHANGING cs_data  LIKE LINE OF gt_data.

  DATA: lt_parts TYPE TABLE OF tdline,
        ls_parts TYPE tdline,
        lv_lines TYPE i,
        lv_tabix TYPE i.

* number of physical lines
  gs_stats-locphy = gs_stats-locphy + 1.

  IF cs_data-format <> '/*' AND cs_data-original IS INITIAL.
*   number of blank lines
    gs_stats-locbl = gs_stats-locbl + 1.
  ENDIF.

  IF cs_data-format = '/*'.
*   number of commented lines
    gs_stats-loccom = gs_stats-loccom + 1.
  ELSE.
    IF cs_data-original IS NOT INITIAL.
*     number of program lines
      gs_stats-locpro = gs_stats-locpro + 1.
    ENDIF.
  ENDIF.

  IF cs_data-format = '/W'.
*   number of windows
    gs_stats-numwin = gs_stats-numwin + 1.
  ENDIF.

  IF cs_data-format = '/E'.
*   number of text elements
    gs_stats-numtel = gs_stats-numtel + 1.
  ENDIF.

* collect data for McCabe metric (cyclomatic complexity)
  IF cs_data-format = '/:'.
    IF gs_stats-mccab1 IS INITIAL.
      gs_stats-mccab1 = 1.
      gs_stats-mccab2 = 1.
      cs_data-mccabe1 = gs_stats-mccab1.
      cs_data-mccabe2 = gs_stats-mccab2.
      SHIFT cs_data-mccabe1 LEFT DELETING LEADING '0'.
      SHIFT cs_data-mccabe2 LEFT DELETING LEADING '0'.
    ENDIF.

    IF us_parts = 'IF' OR us_parts = 'ELSEIF' OR us_parts = 'CASE'.
      gs_stats-mccab1 = gs_stats-mccab1 + 1.
      cs_data-mccabe1 = gs_stats-mccab1.
      SHIFT cs_data-mccabe1 LEFT DELETING LEADING '0'.
    ENDIF.

    IF us_parts = 'IF' OR us_parts = 'ELSEIF'.
      gs_stats-mccab2 = gs_stats-mccab2 + 1.
      LOOP AT ut_parts TRANSPORTING NO FIELDS
                       WHERE table_line = 'AND'
                       OR    table_line = 'OR'.
*       count every condition
        gs_stats-mccab2 = gs_stats-mccab2 + 1.
      ENDLOOP.
      cs_data-mccabe2 = gs_stats-mccab2.
      SHIFT cs_data-mccabe2 LEFT DELETING LEADING '0'.
    ENDIF.
  ENDIF.

* collect data for Halstead metric
  SPLIT cs_data-original AT space INTO TABLE lt_parts.
  lv_lines = lines( lt_parts ).
  LOOP AT lt_parts INTO ls_parts.
    lv_tabix = sy-tabix.
    TRANSLATE ls_parts TO UPPER CASE.

    IF cs_data-format <> '/:'.
*     should be operand
      APPEND ls_parts TO gs_stats-hsopd.
      CONTINUE.
    ENDIF.

    IF lv_tabix = lv_lines.
      REPLACE ALL OCCURRENCES OF '.' IN ls_parts WITH space.
    ENDIF.

    READ TABLE gt_sscmd TRANSPORTING NO FIELDS
                        WITH KEY command = ls_parts.

    IF sy-subrc = 0.
*     should be operator
      APPEND ls_parts TO gs_stats-hsopt.
      CONTINUE.
    ELSE.
      IF ls_parts IS NOT INITIAL.
*       should be operand
        APPEND ls_parts TO gs_stats-hsopd.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_SUBROUTINE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_SFIELD  text
*      <--P_CS_DATA  text
*----------------------------------------------------------------------*
FORM form_subroutine_show CHANGING cs_sfield TYPE slis_selfield
                                   cs_data   LIKE LINE OF gt_data.

  DATA: lt_parts TYPE TABLE OF tdline,
        ls_parts TYPE tdline,
        ls_vhead TYPE scwb_version_header,
        lt_scode TYPE bcwbd_abaptext_lines,
        lv_subr  TYPE abaptxt255,
        lv_line  TYPE n LENGTH 6.

* reduce SAPscript command
  SPLIT cs_data-original AT space INTO TABLE lt_parts.
  DELETE lt_parts WHERE table_line = 'PERFORM' OR
                        table_line = 'IN' OR
                        table_line = 'PROGRAM'.

  IF sy-subrc <> 0.
    MESSAGE 'Subroutine could not be shown.' TYPE 'I'.
    RETURN.
  ENDIF.

  READ TABLE lt_parts INTO lv_subr INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE 'Subroutine could not be shown.' TYPE 'I'.
    RETURN.
  ENDIF.

  READ TABLE lt_parts INTO ls_vhead-objname INDEX 2.
  IF sy-subrc <> 0.
    MESSAGE 'Subroutine could not be shown.' TYPE 'I'.
    RETURN.
  ENDIF.

  ls_vhead-pgmid   = 'LIMU'.
  ls_vhead-objtype = 'REPS'.

  CALL FUNCTION 'SCWB_GET_ABAP_CODE_OF_OBJECT'
    EXPORTING
      is_version_header  = ls_vhead
*     IV_CASE            = ' '
*     IV_CONDENSE_REQUIRED       = ' '
*     IV_IGNORE_COMMENTS = ' '
    IMPORTING
*     ES_RAW_INCLUDE     =
*     ET_CODE_RAW        =
      et_code_normalized = lt_scode
    EXCEPTIONS
      rfc_error          = 1
      not_found          = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Subroutine could not be shown.' TYPE 'I'.
    RETURN.
  ENDIF.

  CLEAR lv_line.

  FIND FIRST OCCURRENCE OF lv_subr
       IN TABLE lt_scode
       IGNORING CASE
       MATCH LINE lv_line.

  IF sy-subrc <> 0.
    MESSAGE 'Subroutine could not be shown.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'EDITOR_PROGRAM'
    EXPORTING
*     APPID       = '  '
      display     = gc_yes
*     FBNAME      = ' '
      line        = lv_line
*     MESSAGE     = ' '
*     OFFSET      = '00'
      program     = ls_vhead-objname
      topline     = lv_line
*     VARIED      = ' '
*     TRDIR_INF   = ' '
*     STATUS      = ' '
*     ORIGINAL_POS_FLAG       = 'X'
*   IMPORTING
*     DYNPRO      =
*     EVENT       =
*     FCODE       =
*     MODULE      =
*     SUBRC       =
    EXCEPTIONS
      application = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Subroutine could not be shown.' TYPE 'I'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_STATS_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_stats_show.

  DATA: lt_msgs TYPE bapiret2_t,
        ls_msgs TYPE bapiret2,
        lv_perc TYPE char15,
        lv_text TYPE tdline.

* general information
  MESSAGE s499 WITH 'General information' INTO ls_msgs-message.
  sy-vline = gc_yes.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Number of window elements:'
                    gs_stats-numwin
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Number of text elements:'
                    gs_stats-numtel
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

* source lines of code
  MESSAGE s499 WITH 'Source lines of code' INTO ls_msgs-message.
  sy-vline = gc_yes.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Number of physical lines:'
                    gs_stats-locphy
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  CONCATENATE '(~' gs_stats-locprp '%)' INTO lv_perc.
  MESSAGE s499 WITH 'Number of program lines:'
                    gs_stats-locpro
                    lv_perc
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  CLEAR lv_perc.
  CONCATENATE '(~' gs_stats-loccop '%)' INTO lv_perc.
  MESSAGE s499 WITH 'Number of commented lines:'
                    gs_stats-loccom
                    lv_perc
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  CLEAR lv_perc.
  CONCATENATE '(~' gs_stats-locblp '%)' INTO lv_perc.
  MESSAGE s499 WITH 'Number of blanks lines:'
                    gs_stats-locbl
                    lv_perc
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

* McCabe metric
  MESSAGE s499 WITH 'McCabe metric' INTO ls_msgs-message.
  sy-vline = gc_yes.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'McCabe (single precision):'
                    gs_stats-mccab1
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'McCabe (double precision):'
                    gs_stats-mccab2
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

* Halstead metric
  MESSAGE s499 WITH 'Halstead metric' INTO ls_msgs-message.
  sy-vline = gc_yes.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Number of distinct operators:'
                    gs_stats-hsln1
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Number of distinct operands:'
                    gs_stats-hsln2
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Total number of operators:'
                    gs_stats-hsbn1
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Total number of operands:'
                    gs_stats-hsbn2
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Halstead program vocabulary:'
                    gs_stats-hsprv
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Halstead program length:'
                    gs_stats-hsprl
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Halstead volume:'
                    gs_stats-hsvol
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Halstead difficulty:'
                    gs_stats-hsdif
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Halstead effort to implement:'
                    gs_stats-hseff
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

  MESSAGE s499 WITH 'Halstead time to implement:'
                    gs_stats-hstti
               INTO ls_msgs-message.
  PERFORM messages_collect CHANGING lt_msgs.

* operator list
  MESSAGE s499 WITH 'Operator list' INTO ls_msgs-message.
  sy-vline = gc_yes.
  PERFORM messages_collect CHANGING lt_msgs.


  FIELD-SYMBOLS: <fs_hsopt> TYPE operator.

  DATA: lv_count TYPE numc5,
        lt_hsopt LIKE gs_stats-hsopt.

  lt_hsopt = gs_stats-hsopt.
  SORT lt_hsopt.
  DELETE ADJACENT DUPLICATES FROM lt_hsopt.

  LOOP AT lt_hsopt ASSIGNING <fs_hsopt>.
    CLEAR: lv_count,
           lv_text.

    LOOP AT gs_stats-hsopt TRANSPORTING NO FIELDS
                           WHERE table_line = <fs_hsopt>-operator.
      lv_count = lv_count + 1.
    ENDLOOP.

    CONCATENATE '"' <fs_hsopt>-operator '":' INTO lv_text.
    SHIFT lv_count LEFT DELETING LEADING '0'.

    MESSAGE s499 WITH 'Number of statement'
                      lv_text
                      lv_count
                 INTO ls_msgs-message.
    PERFORM messages_collect CHANGING lt_msgs.
  ENDLOOP.

* output in window
  PERFORM messages_show USING lt_msgs.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_COLLECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_MSGS  text
*----------------------------------------------------------------------*
FORM messages_collect CHANGING ct_msgs TYPE bapiret2_t.

  DATA ls_msgs TYPE bapiret2.

  ls_msgs-id         = sy-msgid.
  ls_msgs-type       = sy-msgty.
  ls_msgs-number     = sy-msgno.
  ls_msgs-message_v1 = sy-msgv1.
  ls_msgs-message_v2 = sy-msgv2.
  ls_msgs-message_v3 = sy-msgv3.
  ls_msgs-message_v4 = sy-msgv4.

* mark line as headline for later use
  IF sy-vline = gc_yes.
    CLEAR sy-vline.
    ls_msgs-parameter = 'HL'.
  ENDIF.

  APPEND ls_msgs TO ct_msgs.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MSGS  text
*----------------------------------------------------------------------*
FORM messages_show USING ut_msgs TYPE bapiret2_t.

  DATA: ls_msgs  TYPE bapiret2,
        lv_zeile TYPE numc3.

  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXPORTING
*     COLLECT_AND_SEND     = ' '
*     RESET                = 'X'
*     LINE_FROM            = ' '
*     LINE_TO              = ' '
*     I_STORE_DUPLICATES   = 'X'
      i_no_duplicate_count = 0
*     I_IDENTIFICATION     =
*     CHECK_ON_COMMIT      = 'X'
*     I_ALLOW_FOREIGN_RESET       = 'X'
*     I_RESET_LINE         = 'X'
*   IMPORTING
*     E_IDENTIFICATION     =
    EXCEPTIONS
      log_not_active       = 1
      wrong_identification = 2
      OTHERS               = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  lv_zeile = '001'.

  LOOP AT ut_msgs INTO ls_msgs.
    IF ls_msgs-parameter = 'HL'.
      CALL FUNCTION 'MESSAGE_SET_DEFAULTLINE'
        EXPORTING
          arbgb      = ls_msgs-id
          msgty      = ls_msgs-type
          msgv1      = ls_msgs-message_v1
          msgv2      = ls_msgs-message_v2
          msgv3      = ls_msgs-message_v3
          msgv4      = ls_msgs-message_v4
          txtnr      = ls_msgs-number
          zeile      = lv_zeile
*         MODIFY     =
        EXCEPTIONS
          not_active = 1
          OTHERS     = 2.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ELSE.
      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = 'SY'
*         EXCEPTION_IF_NOT_ACTIVE       = 'X'
          msgty                  = 'S'
          msgv1                  = ls_msgs-message_v1
          msgv2                  = ls_msgs-message_v2
          msgv3                  = ls_msgs-message_v3
          msgv4                  = ls_msgs-message_v4
          txtnr                  = '499'
*         zeile                  =
*       IMPORTING
*         ACT_SEVERITY           =
*         MAX_SEVERITY           =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDIF.

    lv_zeile = lv_zeile + 1.
  ENDLOOP.

  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
*     CORRECTIONS_OPTION = ' '
*     CORRECTIONS_FUNC_TEXT       = ' '
*     MSG_SELECT_FUNC    = ' '
*     MSG_SELECT_FUNC_TEXT        = ' '
*     LINE_FROM          = ' '
*     LINE_TO            = ' '
      object             = 'Stats'
*     SEND_IF_ONE        = ' '
*     BATCH_LIST_TYPE    = 'J'
      show_linno         = gc_no
*     SHOW_LINNO_TEXT    = ' '
*     SHOW_LINNO_TEXT_LEN         = '3'
      i_use_grid         = gc_yes
*     I_AMODAL_WINDOW    = ' '
*   IMPORTING
*     CORRECTIONS_WANTED =
*     MSG_SELECTED       =
*     E_EXIT_COMMAND     =
    EXCEPTIONS
      inconsistent_range = 1
      no_messages        = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_STATS_CALCULATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_stats_calculate.

  DATA: lt_hsopd LIKE gs_stats-hsopd,
        lt_hsopt LIKE gs_stats-hsopt.

* number of program lines as percentage value
  gs_stats-locprp = ( gs_stats-locpro / gs_stats-locphy ) * 100.
  SHIFT gs_stats-locprp LEFT DELETING LEADING '0'.

* number of commented lines as percentage value
  gs_stats-loccop = ( gs_stats-loccom / gs_stats-locphy  ) * 100.
  SHIFT gs_stats-loccop LEFT DELETING LEADING '0'.

* number of blank lines as percentage value
  gs_stats-locblp = ( gs_stats-locbl / gs_stats-locphy ) * 100.
  SHIFT gs_stats-locblp LEFT DELETING LEADING '0'.

* number of distinct operators
  lt_hsopt = gs_stats-hsopt.
  SORT lt_hsopt.
  DELETE ADJACENT DUPLICATES FROM lt_hsopt.
  gs_stats-hsln1 = lines( lt_hsopt ).

* number of distinct operands
  lt_hsopd = gs_stats-hsopd.
  SORT lt_hsopd.
  DELETE ADJACENT DUPLICATES FROM lt_hsopd.
  gs_stats-hsln2 = lines( lt_hsopd ).

* total number of operators
  gs_stats-hsbn1 = lines( gs_stats-hsopt ).

* total number of operands
  gs_stats-hsbn2 = lines( gs_stats-hsopd ).

  gs_stats-hsprv = gs_stats-hsln1 + gs_stats-hsln2.
  gs_stats-hsprl = gs_stats-hsbn1 + gs_stats-hsbn2.
  gs_stats-hsvol = gs_stats-hsprl * log( gs_stats-hsprv ).
  gs_stats-hsdif = ( gs_stats-hsln1 / 2 ) *
                   ( gs_stats-hsbn2 / gs_stats-hsln2 ).
  gs_stats-hseff = gs_stats-hsdif * gs_stats-hsvol.
  gs_stats-hstti = gs_stats-hseff / 18.

  SHIFT gs_stats-locphy LEFT DELETING LEADING '0'.
  SHIFT gs_stats-locpro LEFT DELETING LEADING '0'.
  SHIFT gs_stats-loccom LEFT DELETING LEADING '0'.
  SHIFT gs_stats-locbl  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-numwin LEFT DELETING LEADING '0'.
  SHIFT gs_stats-numtel LEFT DELETING LEADING '0'.
  SHIFT gs_stats-mccab1 LEFT DELETING LEADING '0'.
  SHIFT gs_stats-mccab2 LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsln1  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsln2  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsbn1  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsbn2  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsprv  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsprl  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsvol  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hsdif  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hseff  LEFT DELETING LEADING '0'.
  SHIFT gs_stats-hstti  LEFT DELETING LEADING '0'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SSCR_EXPR_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sscmds_fill.

* build a global internal table with all known SAPscript commands

  sscmd_add 'NEW-PAGE'.

  sscmd_add 'PROTECT'.
  sscmd_add 'ENDPROTECT'.

  sscmd_add 'NEW-WINDOW'.

  sscmd_add 'DEFINE'.

  sscmd_add 'SET'.
  sscmd_add 'DATE'.
  sscmd_add 'TIME'.
  sscmd_add 'MASK'.
  sscmd_add 'COUNTRY'.
  sscmd_add 'SIGN'.
  sscmd_add 'LEFT'.
  sscmd_add 'RIGHT'.

  sscmd_add 'RESET'.

  sscmd_add 'INCLUDE'.
  sscmd_add 'OBJECT'.
  sscmd_add 'ID'.
  sscmd_add 'LANGUAGE'.
  sscmd_add 'PARAGRAPH'.
  sscmd_add 'NEW-PARAGRAPH'.

  sscmd_add 'STYLE'.
  sscmd_add 'DOMINANT'.

  sscmd_add 'ADDRESS'.
  sscmd_add 'DELIVERY'.
  sscmd_add 'TYPE'.
  sscmd_add 'PRIORITY'.
  sscmd_add 'LINES'.
  sscmd_add 'TITLE'.
  sscmd_add 'NAME'.
  sscmd_add 'PERSON'.
  sscmd_add 'PERSONNUMBER'.
  sscmd_add 'DEPARTMENT'.
  sscmd_add 'STREET'.
  sscmd_add 'HOUSE'.
  sscmd_add 'LOCATION'.
  sscmd_add 'POBOX'.
  sscmd_add 'CODE'.
  sscmd_add 'CITY'.
  sscmd_add 'POSTCODE'.
  sscmd_add 'NO_UPPERCASE_FOR_CITY'.
  sscmd_add 'REGION'.
  sscmd_add 'COUNTRY'.
  sscmd_add 'COUNTRY_IN_REC_LANG'.
  sscmd_add 'LANG_FOR_COUNTRY'.
  sscmd_add 'FROMCOUNTRY'.
  sscmd_add 'ADDRESSNUMBER'.
  sscmd_add 'ENDADDRESS'.

  sscmd_add 'TOP'.
  sscmd_add 'ENDTOP'.

  sscmd_add 'BOTTOM'.
  sscmd_add 'ENDBOTTOM'.

  sscmd_add 'IF'.
  sscmd_add 'ELSEIF'.
  sscmd_add 'ELSE'.
  sscmd_add 'ENDIF'.
  sscmd_add 'NOT'.
  sscmd_add 'AND'.
  sscmd_add 'OR'.
  sscmd_add 'EQ'.
  sscmd_add 'LT'.
  sscmd_add 'GT'.
  sscmd_add 'LE'.
  sscmd_add 'GE'.
  sscmd_add 'NE'.
  sscmd_add '='.
  sscmd_add '<'.
  sscmd_add '>'.
  sscmd_add '<='.
  sscmd_add '>='.
  sscmd_add '<>'.

  sscmd_add 'CASE'.
  sscmd_add 'WHEN'.
  sscmd_add 'OTHERS'.
  sscmd_add 'ENDCASE'.

  sscmd_add 'PERFORM'.
  sscmd_add 'IN'.
  sscmd_add 'PROGRAM'.
  sscmd_add 'USING'.
  sscmd_add 'CHANGING'.
  sscmd_add 'ENDPERFORM'.

  sscmd_add 'PRINT-CONTROL'.

  sscmd_add 'BOX'.
  sscmd_add 'XPOS'.
  sscmd_add 'YPOS'.
  sscmd_add 'WIDTH'.
  sscmd_add 'HEIGHT'.
  sscmd_add 'FRAME'.
  sscmd_add 'INTENSITY'.
  sscmd_add 'POSITION'.
  sscmd_add 'XORIGIN'.
  sscmd_add 'YORIGIN'.
  sscmd_add 'WINDOW'.
  sscmd_add 'PAGE'.
  sscmd_add 'SIZE'.

  sscmd_add 'HEX'.
  sscmd_add 'TYPE'.
  sscmd_add 'ENDHEX'.

  sscmd_add 'SUMMING'.
  sscmd_add 'INTO'.

  sscmd_add 'PARAGRAPH'.
  sscmd_add 'TEXT'.
  sscmd_add 'STRING'.
  sscmd_add 'PAGE'.

ENDFORM.