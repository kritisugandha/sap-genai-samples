************************************************************************************************************************
* Copyright 2024 Google LLC                                                                                            *
* ABAP SDK for Google Cloud is made available as "Software" under the agreement governing your use of                  *
* Google Cloud Platform including the Service Specific Terms available at                                              *
*                                                                                                                      *
* https://cloud.google.com/terms/service-terms                                                                         *
*                                                                                                                      *
* Without limiting the generality of the above terms, you may not modify or distribute ABAP SDK for Google Cloud       *
* without express written permission from Google.                                                                      *
************************************************************************************************************************

CLASS lcl_main IMPLEMENTATION.

  METHOD authorization_check.

    AUTHORITY-CHECK OBJECT 'ZGOOG_SDK'
      ID 'ACTVT' FIELD '16'.
    IF sy-subrc <> 0.
      MESSAGE e003(/goog/sdk_msg) WITH /goog/cl_googauth_v1=>c_ret_code_461.

    ENDIF.

  ENDMETHOD.

  METHOD set_pf_status.

    SET PF-STATUS 'GUI_STATUS'.
    SET TITLEBAR 'GUI_TITLE'.

  ENDMETHOD.

  METHOD create_containers.

    IF gr_cont_sysins IS NOT BOUND.
      CREATE OBJECT gr_cont_sysins
        EXPORTING
          container_name = 'C_SYSTEM_INST'
          repid          = sy-repid
          dynnr          = sy-dynnr.

    ENDIF.

    IF gr_cont_prompt IS NOT BOUND.
      CREATE OBJECT gr_cont_prompt
        EXPORTING
          container_name = 'C_PROMPT'
          repid          = sy-repid
          dynnr          = sy-dynnr.

    ENDIF.

    IF gr_cont_response IS NOT BOUND.
      CREATE OBJECT gr_cont_response
        EXPORTING
          container_name = 'C_RESPONSE'
          repid          = sy-repid
          dynnr          = sy-dynnr.

    ENDIF.

  ENDMETHOD.

  METHOD create_text_editors.

    IF gr_text_sysins IS NOT BOUND.
      CREATE OBJECT gr_text_sysins
        EXPORTING
          wordwrap_mode = 1               " 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
          parent        = gr_cont_sysins. " Parent Container

    ENDIF.

    IF gr_text_prompt IS NOT BOUND.
      CREATE OBJECT gr_text_prompt
        EXPORTING
          wordwrap_mode = 1               " 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
          parent        = gr_cont_prompt. " Parent Container

    ENDIF.

    IF gr_text_response IS NOT BOUND.
      CREATE OBJECT gr_text_response
        EXPORTING
          wordwrap_mode = 1                 " 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
          parent        = gr_cont_response. " Parent Container
      gr_text_response->set_readonly_mode( 1 ).

    ENDIF.

    IF p_rb1 = abap_true.
      LOOP AT SCREEN.
        IF screen-group1 = 'MM'.
          screen-active    = 0.

        ENDIF.
        MODIFY SCREEN.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read_text_editor.

    CASE sy-ucomm.
      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        LEAVE TO SCREEN 0.

      WHEN 'EXECUTE'.
        IF p_model_key IS INITIAL.
          MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          CALL METHOD execute.

        ENDIF.

      WHEN 'BROWSE'.
        CALL METHOD browse_files.

      WHEN 'CLEAR_GCS'.
        p_mime_gcs = 'application/pdf'.
        CLEAR p_gcs.

      WHEN 'CLEAR_FILE'.
        p_mime_file = 'application/pdf'.
        CLEAR:
              p_no_files,
              gt_file_table.

      WHEN 'CLEAR'.
        gr_text_sysins->delete_text( ).
        gr_text_prompt->delete_text( ).
        gr_text_response->delete_text( ).

        p_mime_gcs = 'application/pdf'.
        p_mime_file = 'application/pdf'.

        CLEAR:
              p_model_key,
              p_no_files,
              p_gcs,
              gt_file_table.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD execute.

    DATA:
      lv_system_instructions TYPE string,
      lv_prompt              TYPE string,
      lt_file_data           TYPE gtt_file_data,
      lt_text_response       TYPE soli_tab,
      lo_generative_model    TYPE REF TO /goog/cl_generative_model,
      lv_response            TYPE string,
      lv_error               TYPE abap_bool,
      lo_cx_sdk              TYPE REF TO /goog/cx_sdk,
      lo_out                 TYPE REF TO if_demo_output,
      lv_msg                 TYPE string,
      lo_response            TYPE REF TO /goog/cl_model_response.

    FIELD-SYMBOLS:
      <ls_text_sysins> TYPE soli,
      <ls_text_prompt> TYPE soli,
      <ls_file_data>   TYPE gty_file_data.

    CLEAR lt_file_data.

    gr_text_response->delete_text( ).

    gr_text_sysins->get_text_as_stream( IMPORTING  text                   = gt_text_sysins
                                        EXCEPTIONS error_cntl_call_method = 1
                                                   OTHERS                 = 3 ).

    gr_text_prompt->get_text_as_stream( IMPORTING  text                   = gt_text_prompt
                                        EXCEPTIONS error_cntl_call_method = 1
                                                   OTHERS                 = 3 ).

    LOOP AT gt_text_sysins ASSIGNING <ls_text_sysins>.
      IF lv_system_instructions IS INITIAL.
        lv_system_instructions = <ls_text_sysins>.
      ELSE.
        CONCATENATE lv_system_instructions <ls_text_sysins> INTO lv_system_instructions
          SEPARATED BY space.

      ENDIF.

    ENDLOOP.

    LOOP AT gt_text_prompt ASSIGNING <ls_text_prompt>.
      IF lv_prompt IS INITIAL.
        lv_prompt = <ls_text_prompt>.
      ELSE.
        CONCATENATE lv_prompt <ls_text_prompt> INTO lv_prompt
          SEPARATED BY space.

      ENDIF.

    ENDLOOP.

    CALL METHOD get_file_data
      IMPORTING
        ev_error     = lv_error
        et_file_data = lt_file_data.
    IF lv_error = abap_true.
      RETURN.

    ENDIF.

    TRY.
        CREATE OBJECT lo_generative_model
          EXPORTING
            iv_model_key = p_model_key.

        IF p_rb2 = 'X'.
          LOOP AT lt_file_data ASSIGNING <ls_file_data>.
            lo_generative_model->set_inline_data( iv_mime_type = <ls_file_data>-mime_type
                                                  iv_data      = <ls_file_data>-filedata ).

          ENDLOOP.

          IF p_gcs IS NOT INITIAL.
            IF p_gcs CS 'gs://'.
              lo_generative_model->set_file_data( iv_mime_type = p_mime_gcs
                                                  iv_file_uri  = p_gcs ).
            ELSE.
              lo_generative_model->set_files_from_gcs( iv_storage_bucket_name = p_gcs ).

            ENDIF.

          ENDIF.

        ENDIF.

        lo_response = lo_generative_model->set_system_instructions( lv_system_instructions
                                        )->generate_content( lv_prompt ).
        lv_response = lo_response->get_text( ).
        IF lv_response IS NOT INITIAL.
          CALL METHOD convert_string_to_table
            EXPORTING
              iv_response      = lv_response
            IMPORTING
              et_text_response = lt_text_response.

          gr_text_response->set_text_as_stream( lt_text_response ).

          CLEAR:
                lv_response,
                lt_text_response.
        ELSE.
          lv_msg = lo_response->get_finish_reason( ).
          CONCATENATE TEXT-006 lv_msg INTO lv_msg
            SEPARATED BY space.
          lo_out = cl_demo_output=>new( ).
          lo_out->write_text( lv_msg ).
          CONCATENATE TEXT-007 TEXT-008 INTO lv_msg
            SEPARATED BY space.
          lo_out->write_text( lv_msg ).
          lo_out->display(  ).
          CLEAR lv_msg.

        ENDIF.
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        cl_demo_output=>display( lo_cx_sdk->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD convert_string_to_table.

    DATA lv_len_content   TYPE i.
    DATA lv_len_src       TYPE i.
    DATA lv_len_wa        TYPE i.
    DATA lv_offset        TYPE i.
    DATA ls_text_response TYPE soli.

    DESCRIBE FIELD ls_text_response LENGTH lv_len_content IN CHARACTER MODE.
    lv_offset = 0.
    lv_len_src = strlen( iv_response ).
    WHILE lv_offset < lv_len_src.
      lv_len_wa = lv_len_src - lv_offset.
      IF lv_len_wa > lv_len_content.
        APPEND iv_response+lv_offset(lv_len_content) TO et_text_response.
        lv_offset = lv_offset + lv_len_content.
      ELSE.
        APPEND iv_response+lv_offset(lv_len_wa) TO et_text_response.
        lv_offset = lv_offset + lv_len_wa.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD get_file_data.

    DATA ls_file_data   TYPE gty_file_data.
    DATA lv_file_length TYPE i.
    DATA lt_bin_data    TYPE STANDARD TABLE OF char1024.
    DATA lv_xfile       TYPE xstring.
    DATA lv_output      TYPE string.
    DATA lv_filename    TYPE string.

    FIELD-SYMBOLS:
                   <ls_file_table>  TYPE gty_file_table.

    LOOP AT gt_file_table ASSIGNING <ls_file_table>.
      lv_filename = <ls_file_table>-filename.
      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = lv_filename
          filetype                = 'BIN'
        IMPORTING
          filelength              = lv_file_length
        CHANGING
          data_tab                = lt_bin_data
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19.
      IF sy-subrc <> 0.
        MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
        ev_error = abap_true.
        RETURN.

      ENDIF.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_file_length
        IMPORTING
          buffer       = lv_xfile
        TABLES
          binary_tab   = lt_bin_data.

      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lv_xfile
        IMPORTING
          output = lv_output.

      ls_file_data-filename  = lv_filename.
      IF <ls_file_table>-mime_type <> p_mime_file.
        <ls_file_table>-mime_type = p_mime_file.

      ENDIF.
      ls_file_data-mime_type = <ls_file_table>-mime_type.
      ls_file_data-filedata  = lv_output.

      APPEND ls_file_data TO et_file_data.
      CLEAR ls_file_data.

    ENDLOOP.

  ENDMETHOD.

  METHOD browse_files.

    DATA:
      lv_rc           TYPE i,
      lv_lines        TYPE string,
      lt_filetable    TYPE filetable,
      ls_file_table   TYPE gty_file_table,
      lv_window_title TYPE string.

    FIELD-SYMBOLS:
                   <ls_filetable>  TYPE file_table.

    CLEAR lt_filetable.
    lv_window_title = TEXT-004.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = lv_window_title
        multiselection          = abap_true
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
*       Implement suitable error handling here
    ENDIF.

    LOOP AT lt_filetable ASSIGNING <ls_filetable>.
      ls_file_table-filename  = <ls_filetable>-filename.
      ls_file_table-mime_type = p_mime_file.

      APPEND ls_file_table TO gt_file_table.

    ENDLOOP.

    lv_lines = lines( gt_file_table ).
    IF lv_lines = 1.
      CONCATENATE lv_lines TEXT-002 INTO p_no_files
        SEPARATED BY space.
    ELSE.
      CONCATENATE lv_lines TEXT-003 INTO p_no_files
        SEPARATED BY space.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Module STATUS_1000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_container OUTPUT.

  lcl_main=>create_containers( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_TEXT_EDITOR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_text_editor OUTPUT.

  lcl_main=>create_text_editors( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  READ_EDITOR_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_text_editor INPUT.

  lcl_main=>read_text_editor( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_PF_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_pf_status OUTPUT.

  IF gv_first_run = abap_true.
    lcl_main=>authorization_check( ).
    gv_first_run = abap_false.

  ENDIF.

  lcl_main=>set_pf_status( ).

ENDMODULE.
