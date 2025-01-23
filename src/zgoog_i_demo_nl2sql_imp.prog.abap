**********************************************************************
*  Copyright 2024 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************

CLASS lcl_main IMPLEMENTATION.
  METHOD set_pf_status.

    SET PF-STATUS 'GUI_STATUS'.
    SET TITLEBAR 'GUI_TITLE'.

  ENDMETHOD.

  METHOD create_containers.

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

      WHEN 'CLEAR'.
        gr_text_prompt->delete_text( ).
        gr_text_response->delete_text( ).

        CLEAR:
              p_model_key.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD execute.

    DATA:
      lv_prompt              TYPE string,
      lt_text_response       TYPE soli_tab,
      lo_nl2sql    TYPE REF TO ZGOOG_CL_NL2SQL_CDS_VIEW,
      lv_response            TYPE string,
      lv_error               TYPE abap_bool,
      lo_cx_sdk              TYPE REF TO /goog/cx_sdk,
      lo_out                 TYPE REF TO if_demo_output,
      lv_msg                 TYPE string.

    FIELD-SYMBOLS:
      <ls_text_sysins> TYPE soli,
      <ls_text_prompt> TYPE soli.

    gr_text_response->delete_text( ).

    gr_text_prompt->get_text_as_stream( IMPORTING  text                   = gt_text_prompt
                                        EXCEPTIONS error_cntl_call_method = 1
                                                   OTHERS                 = 3 ).

    LOOP AT gt_text_prompt ASSIGNING <ls_text_prompt>.
      IF lv_prompt IS INITIAL.
        lv_prompt = <ls_text_prompt>.
      ELSE.
        CONCATENATE lv_prompt <ls_text_prompt> INTO lv_prompt
          SEPARATED BY space.

      ENDIF.

    ENDLOOP.

    TRY.
        CREATE OBJECT lo_nl2sql
          EXPORTING
            iv_model_key = p_model_key.


        lv_response = lo_nl2sql->GENERATE_SQL(
          IV_PROMPT = lv_prompt
          IV_CDS_VIEW = p_cds_view ).

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

  lcl_main=>set_pf_status( ).

ENDMODULE.
