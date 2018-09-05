*&---------------------------------------------------------------------*
*& Report SAPHTML_EVENTS_DEMO                                          *
*&---------------------------------------------------------------------*
REPORT  zsaphtml_events_demo.

DATA: html_control TYPE REF TO cl_gui_html_viewer,
      my_container TYPE REF TO cl_gui_custom_container,
      prog_repid LIKE sy-repid,                             "#EC NEEDED
      edurl(2048),
      edframe(255),
      edaction(256),
      edgetdata(2048),
      edpostdataline(1024),
      ok_code LIKE sy-ucomm,
      myevent_tab TYPE cntl_simple_events,
      myevent TYPE cntl_simple_event,
      postdata_tab TYPE cnht_post_data_tab,
      edquery_table TYPE cnht_query_table.                  "#EC NEEDED


*****************************************************
*              CLASS cl_myevent_handler             *
*****************************************************
CLASS cl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_sapevent
               FOR EVENT sapevent OF cl_gui_html_viewer
                 IMPORTING action frame getdata postdata query_table.

ENDCLASS.

SET SCREEN 100.

DATA: evt_receiver TYPE REF TO cl_myevent_handler.

* CLASS CL_GUI_CFW DEFINITION LOAD.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'TESTHTM1'.
  SET TITLEBAR '001'.

  IF html_control IS INITIAL.
    prog_repid = sy-repid.

    CREATE OBJECT my_container
        EXPORTING
            container_name = 'HTML_CONTROL'.

    CREATE OBJECT html_control
         EXPORTING
              parent    = my_container.
    IF sy-subrc NE 0.
*
    ENDIF.

* register event
    myevent-eventid = html_control->m_id_sapevent.
    myevent-appl_event = 'x'.
    APPEND myevent TO myevent_tab.
    CALL METHOD html_control->set_registered_events
        EXPORTING
           events = myevent_tab.

    CREATE OBJECT evt_receiver.

    SET HANDLER evt_receiver->on_sapevent
                FOR html_control.

    PERFORM load_graphics.
    PERFORM load_home_page.
  ENDIF.
ENDMODULE.                             " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'BACK'.                       "Beenden
      IF NOT html_control IS INITIAL.
        CALL METHOD html_control->free.
        FREE html_control.
      ENDIF.
      LEAVE TO SCREEN 0.

    WHEN 'HHOM'.                       " show the home page
      PERFORM load_home_page.

    WHEN 'HBAK'.
      CALL METHOD html_control->go_back.

    WHEN 'HFWD'.
      CALL METHOD html_control->go_forward.

    WHEN 'HRFR'.
      CALL METHOD html_control->do_refresh.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                             " USER_COMMAND_0100  INPUT


*&---------------------------------------------------------------------*
*&      Form  LOAD_HOME_PAGE
*&---------------------------------------------------------------------*
FORM load_home_page.
  DATA: doc_url(80).

  CALL METHOD html_control->load_html_document
       EXPORTING
            document_id  = 'ZSAPHTML_EVENTS_DEMO_HOME'
       IMPORTING
            assigned_url = doc_url
       EXCEPTIONS
            OTHERS       = 1.

  IF sy-subrc EQ 0.
    CALL METHOD html_control->show_url
         EXPORTING
              url = doc_url.
  ENDIF.
ENDFORM.                               " LOAD_HOME_PAGE


*&---------------------------------------------------------------------*
*&      Form  LOAD_FRAME_SET
*&---------------------------------------------------------------------*
FORM load_frame_set.
  DATA: doc_url(80).

  CALL METHOD html_control->load_html_document
       EXPORTING
            document_id  = 'SAPHTML_EVENTS_DEMO_FRAME1'
            document_url = 'HTMLFrame1.htm'
       EXCEPTIONS
            OTHERS       = 1.

  CALL METHOD html_control->load_html_document
       EXPORTING
            document_id  = 'ZSAPHTML_EVENTS_DEMO_HOME'
            document_url = 'HTMLFrame2.htm'
       EXCEPTIONS
            OTHERS       = 1.

  CALL METHOD html_control->load_html_document
       EXPORTING
            document_id  = 'SAPHTML_EVENTS_DEMO_FRAMESET'
       IMPORTING
            assigned_url = doc_url
       EXCEPTIONS
            OTHERS       = 1.

  IF sy-subrc EQ 0.
    CALL METHOD html_control->show_url
         EXPORTING
              url = doc_url.
  ENDIF.
ENDFORM.                               " LOAD_FRAME_SET


*&---------------------------------------------------------------------*
*&      Form  LOAD_GRAPHICS
*&---------------------------------------------------------------------*
FORM load_graphics.
  CALL METHOD html_control->load_mime_object
     EXPORTING
          object_id  = 'HTMLCNTL_TESTHTM2_SAPLOGO'
          object_url = 'SAPLOGO.GIF'
     EXCEPTIONS
          OTHERS     = 1.
  CALL METHOD html_control->load_mime_object
       EXPORTING
            object_id  = 'HTMLCNTL_TESTHTM2_SAP_AG'
            object_url = 'SAP_AG.GIF'
       EXCEPTIONS
            OTHERS     = 1.
  CALL METHOD html_control->load_mime_object
       EXPORTING
            object_id  = 'HTMLCNTL_TESTHTM2_BACKGROUND'
            object_url = 'HOME_BACKGROUND.GIF'
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                               " LOAD_GRAPHICS

****************************************************
*    cl_myevent_handler implementation             *
****************************************************
CLASS cl_myevent_handler IMPLEMENTATION.

  METHOD on_sapevent.

    CLEAR edaction.
    CLEAR edframe.
    CLEAR edgetdata.
    CLEAR edpostdataline.

    edaction       = action.
    edframe        = frame.
    edgetdata      = getdata.
    postdata_tab   = postdata.
    IF NOT postdata_tab IS INITIAL.
      READ TABLE postdata_tab INDEX 1 INTO edpostdataline.
    ENDIF.
    edquery_table  = query_table.

    CASE action.
      WHEN 'SHOW_FRAMESET'.
        PERFORM load_frame_set.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
