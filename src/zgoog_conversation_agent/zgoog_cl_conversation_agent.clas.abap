**********************************************************************
*  Copyright 2025 Google LLC                                         *
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

CLASS zgoog_cl_conversation_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_t_conversation_hist TYPE STANDARD TABLE OF zgoog_conv_hist WITH DEFAULT KEY .

    DATA gv_session_id TYPE guid .

    METHODS constructor
      IMPORTING
        !iv_model_key  TYPE /goog/model_key
        !iv_session_id TYPE guid OPTIONAL
        !iv_log_obj    TYPE balobj_d OPTIONAL
        !iv_log_subobj TYPE balsubobj OPTIONAL
      RAISING
        /goog/cx_sdk .
    METHODS send_message_content
      IMPORTING
        !iv_prompt_text              TYPE string
        !it_raw_file_data            TYPE /goog/cl_generative_model=>tt_file_inline_data OPTIONAL
        !it_gcs_file_uris            TYPE /goog/cl_generative_model=>tt_file_uri OPTIONAL
      RETURNING
        VALUE(ro_conversation_agent) TYPE REF TO zgoog_cl_conversation_agent
      RAISING
        /goog/cx_sdk .
    METHODS get_text_response
      RETURNING
        VALUE(ev_response) TYPE string .
    METHODS get_conversation_history
      RETURNING
        VALUE(et_conversation_history) TYPE /goog/cl_aiplatform_v1=>ty_t_695 .
    METHODS get_conversation_session_id
      RETURNING
        VALUE(rv_session_id) TYPE guid
      RAISING
        /goog/cx_sdk .
    METHODS clear_conversation_history
      IMPORTING
        !iv_session_id TYPE guid
      RAISING
        /goog/cx_sdk .
  PROTECTED SECTION.

    DATA go_generative_model TYPE REF TO /goog/cl_generative_model .
    DATA gt_conversation_hist TYPE ty_t_conversation_hist .
    DATA gt_message_content TYPE /goog/cl_generative_model=>tt_content_history .
    DATA gv_text_response TYPE string .
    DATA gs_content_response TYPE /goog/cl_aiplatform_v1=>ty_727 .
    DATA gt_conversation_history TYPE /goog/cl_aiplatform_v1=>ty_t_695 .
    DATA gs_current_conversation TYPE /goog/cl_aiplatform_v1=>ty_695 .

    METHODS update_conversation_history
      RAISING
        /goog/cx_sdk .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZGOOG_CL_CONVERSATION_AGENT IMPLEMENTATION.


  METHOD clear_conversation_history.

    DELETE FROM zgoog_conv_hist WHERE session_id = iv_session_id.
    IF sy-subrc <> 0.
      CALL METHOD /goog/cl_vertex_ai_sdk_utility=>raise_error
        EXPORTING
          iv_ret_code = 462
          iv_err_text = |Purge not successful for the Session ID, SY_SUBRC - | && sy-subrc.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA:
      lo_system_uuid TYPE REF TO if_system_uuid,
      lo_uuid_error  TYPE REF TO cx_uuid_error.

    CREATE OBJECT go_generative_model
      EXPORTING
        iv_model_key  = iv_model_key
        iv_log_obj    = iv_log_obj
        iv_log_subobj = iv_log_subobj.

    IF iv_session_id IS SUPPLIED.
      gv_session_id = iv_session_id.

    ELSE.
      lo_system_uuid = cl_uuid_factory=>create_system_uuid( ).

      TRY.
          gv_session_id = lo_system_uuid->create_uuid_x16( ).
        CATCH cx_uuid_error INTO lo_uuid_error.
          CALL METHOD /goog/cl_vertex_ai_sdk_utility=>raise_error
            EXPORTING
              iv_ret_code = 462
              iv_err_text = lo_uuid_error->get_text( ).

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_conversation_history.

    DATA:
      lv_content_xstring TYPE xstring,
      lv_content_string  TYPE string,
      lt_bin_data        TYPE STANDARD TABLE OF char1024,
      lv_file_length     TYPE i.

    FIELD-SYMBOLS:
                   <ls_conversation_hist>  TYPE zgoog_conv_hist.

    SELECT *
      FROM zgoog_conv_hist
      INTO TABLE gt_conversation_hist
      WHERE session_id = gv_session_id.
    IF sy-subrc = 0.
      SORT gt_conversation_hist BY start_time DESCENDING.

    ENDIF.

    READ TABLE gt_conversation_hist ASSIGNING <ls_conversation_hist> INDEX 1.
    IF sy-subrc = 0.
      lv_content_xstring = cl_http_utility=>decode_x_base64( <ls_conversation_hist>-content ).

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_content_xstring
        IMPORTING
          output_length = lv_file_length
        TABLES
          binary_tab    = lt_bin_data.

      CALL FUNCTION 'SCMS_BINARY_TO_STRING'
        EXPORTING
          input_length = lv_file_length
        IMPORTING
          text_buffer  = lv_content_string
        TABLES
          binary_tab   = lt_bin_data.

      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json          = lv_content_string
                                                      iv_pretty_name   = /ui2/cl_json=>pretty_mode-extended
                                            IMPORTING es_data          = et_conversation_history ).

    ENDIF.

  ENDMETHOD.


  METHOD get_conversation_session_id.

    IF gv_session_id IS NOT INITIAL.
      rv_session_id = gv_session_id.
    ELSE.
      CALL METHOD /goog/cl_vertex_ai_sdk_utility=>raise_error
        EXPORTING
          iv_ret_code = 461
          iv_err_text = 'Conversation not started or Session ID not set'.

    ENDIF.

  ENDMETHOD.


  METHOD get_text_response.

    ev_response = gv_text_response.

  ENDMETHOD.


  METHOD send_message_content.

    DATA:
      ls_content_history TYPE /goog/cl_generative_model=>ty_content_history,
      lt_content_history TYPE /goog/cl_generative_model=>tt_content_history,
      ls_inline_data     TYPE /goog/cl_generative_model=>ty_file_inline_data,
      ls_file_uri        TYPE /goog/cl_generative_model=>ty_file_uri,
      lo_model_response  TYPE REF TO /goog/cl_model_response,
      ls_current_part    TYPE /goog/cl_aiplatform_v1=>ty_740.

    FIELD-SYMBOLS:
      <ls_conversation_history> TYPE /goog/cl_aiplatform_v1=>ty_695,
      <ls_part>                 TYPE /goog/cl_aiplatform_v1=>ty_740,
      <ls_raw_file_data>        TYPE /goog/cl_generative_model=>ty_file_inline_data,
      <ls_gcs_file_uri>         TYPE /goog/cl_generative_model=>ty_file_uri.

    gt_conversation_history = get_conversation_history( ).

    gs_current_conversation-role = 'user'.

    IF gt_conversation_history IS NOT INITIAL.
      LOOP AT gt_conversation_history ASSIGNING <ls_conversation_history>.
        ls_content_history-role = <ls_conversation_history>-role.

        IF <ls_conversation_history>-parts IS NOT INITIAL.
          LOOP AT <ls_conversation_history>-parts ASSIGNING <ls_part>.
            IF <ls_part>-text IS NOT INITIAL.
              ls_content_history-text = <ls_part>-text.

            ENDIF.

            IF <ls_part>-inline_data IS NOT INITIAL.
              ls_inline_data-mime_type = <ls_part>-inline_data-mime_type.
              ls_inline_data-file_data = <ls_part>-inline_data-data.

              IF <ls_part>-video_metadata IS NOT INITIAL.
                ls_inline_data-video_start_offset = <ls_part>-video_metadata-start_offset.
                ls_inline_data-video_end_offset = <ls_part>-video_metadata-end_offset.

              ENDIF.

              APPEND ls_inline_data TO ls_content_history-file_inline_data.
              CLEAR ls_inline_data.

            ENDIF.

            IF <ls_part>-file_data IS NOT INITIAL.
              ls_file_uri-mime_type = <ls_part>-file_data-mime_type.
              ls_file_uri-file_uri = <ls_part>-file_data-file_uri.

              IF <ls_part>-video_metadata IS NOT INITIAL.
                ls_file_uri-video_start_offset = <ls_part>-video_metadata-start_offset.
                ls_file_uri-video_end_offset = <ls_part>-video_metadata-end_offset.

              ENDIF.

              APPEND ls_file_uri TO ls_content_history-file_uris.
              CLEAR ls_file_uri.

            ENDIF.

            IF <ls_part>-function_call IS NOT INITIAL.
              ls_content_history-function_call = <ls_part>-function_call.

            ENDIF.

            IF <ls_part>-function_response IS NOT INITIAL.
              ls_content_history-function_response = <ls_part>-function_response.

            ENDIF.

            APPEND ls_content_history TO lt_content_history.
            CLEAR:
              ls_content_history-text,
              ls_content_history-file_inline_data,
              ls_content_history-file_uris,
              ls_content_history-function_call,
              ls_content_history-function_response.

          ENDLOOP.

        ENDIF.

        CLEAR ls_content_history.

      ENDLOOP.

    ENDIF.

    IF lt_content_history IS NOT INITIAL.
      go_generative_model->add_content_history( lt_content_history ).

    ENDIF.

    IF it_raw_file_data IS SUPPLIED.
      LOOP AT it_raw_file_data ASSIGNING <ls_raw_file_data>.
        go_generative_model->set_inline_data( iv_mime_type          = <ls_raw_file_data>-mime_type
                                              iv_data               = <ls_raw_file_data>-file_data
                                              iv_video_start_offset = <ls_raw_file_data>-video_start_offset
                                              iv_video_end_offset   = <ls_raw_file_data>-video_end_offset ).

        ls_current_part-inline_data-mime_type = <ls_raw_file_data>-mime_type.
        ls_current_part-inline_data-data = <ls_raw_file_data>-file_data.
        ls_current_part-video_metadata-start_offset = <ls_raw_file_data>-video_start_offset.
        ls_current_part-video_metadata-end_offset = <ls_raw_file_data>-video_end_offset.

        APPEND ls_current_part TO gs_current_conversation-parts.
        CLEAR ls_current_part.

      ENDLOOP.

    ENDIF.

    IF it_gcs_file_uris IS SUPPLIED.
      LOOP AT it_gcs_file_uris ASSIGNING <ls_gcs_file_uri>.
        go_generative_model->set_file_data( iv_mime_type          = <ls_gcs_file_uri>-mime_type
                                            iv_file_uri           = <ls_gcs_file_uri>-file_uri
                                            iv_video_start_offset = <ls_gcs_file_uri>-video_start_offset
                                            iv_video_end_offset   = <ls_gcs_file_uri>-video_end_offset ).

        ls_current_part-file_data-mime_type = <ls_gcs_file_uri>-mime_type.
        ls_current_part-file_data-file_uri = <ls_gcs_file_uri>-file_uri.
        ls_current_part-video_metadata-start_offset = <ls_gcs_file_uri>-video_start_offset.
        ls_current_part-video_metadata-end_offset = <ls_gcs_file_uri>-video_end_offset.

        APPEND ls_current_part TO gs_current_conversation-parts.
        CLEAR ls_current_part.

      ENDLOOP.

    ENDIF.

    lo_model_response = go_generative_model->generate_content( iv_prompt_text ).
    gv_text_response = lo_model_response->get_text( ).

    ls_current_part-text = iv_prompt_text.
    APPEND ls_current_part TO gs_current_conversation-parts.
    CLEAR ls_current_part.

    gs_content_response = lo_model_response->get_response( ).

    update_conversation_history( ).

    ro_conversation_agent = me.

  ENDMETHOD.


  METHOD update_conversation_history.

    DATA:
      ls_chat_hist       TYPE zgoog_conv_hist,
      lv_timestamp       TYPE timestamp,
      lv_content_xstring TYPE xstring.


    FIELD-SYMBOLS:
                   <ls_candidate>  TYPE /goog/cl_aiplatform_v1=>ty_690.

    IF gs_current_conversation IS NOT INITIAL.
      APPEND gs_current_conversation TO gt_conversation_history.

    ENDIF.

    IF gs_content_response IS NOT INITIAL.
      LOOP AT gs_content_response-candidates ASSIGNING <ls_candidate>.
        APPEND <ls_candidate>-content TO gt_conversation_history.

      ENDLOOP.

    ENDIF.

    ls_chat_hist-session_id = gv_session_id.
    GET TIME STAMP FIELD lv_timestamp.
    ls_chat_hist-start_time = lv_timestamp.
    ls_chat_hist-user_name = sy-uname.
    ls_chat_hist-content = /goog/cl_json_util=>serialize_json( is_data = gt_conversation_history ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = ls_chat_hist-content
      IMPORTING
        buffer = lv_content_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      CALL METHOD /goog/cl_vertex_ai_sdk_utility=>raise_error
        EXPORTING
          iv_ret_code = 462
          iv_err_text = |Error while encoding conversation to xstring, SY_SUBRC - | && sy-subrc.

    ENDIF.

    ls_chat_hist-content = cl_http_utility=>encode_x_base64( lv_content_xstring ).

    MODIFY zgoog_conv_hist FROM ls_chat_hist.
    IF sy-subrc <> 0.
      CALL METHOD /goog/cl_vertex_ai_sdk_utility=>raise_error
        EXPORTING
          iv_ret_code = 462
          iv_err_text = |Conversation History update not successful for the Session ID, SY_SUBRC - | && sy-subrc.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
