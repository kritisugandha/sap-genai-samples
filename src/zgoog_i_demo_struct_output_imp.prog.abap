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

    IF p_rb1 = abap_true.
      LOOP AT SCREEN.
        IF screen-group1 = 'MM'.
          screen-active    = 0.

        ENDIF.
        MODIFY SCREEN.

      ENDLOOP.

      gr_cont_prompt->set_visible( visible = cl_gui_control=>visible_true ).

    ELSE.

      LOOP AT SCREEN.
        IF screen-group1 = 'TX'.
          screen-active    = 0.

        ENDIF.
        MODIFY SCREEN.

      ENDLOOP.

      gr_cont_prompt->set_visible( visible = cl_gui_control=>visible_false ).

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
      lv_prompt           TYPE string,
      lt_file_data        TYPE gtt_file_data,
      lt_text_response    TYPE soli_tab,
      lo_generative_model TYPE REF TO /goog/cl_generative_model,
      lv_response         TYPE string,
      lv_error            TYPE abap_bool,
      lo_cx_sdk           TYPE REF TO /goog/cx_sdk,
      lo_out              TYPE REF TO if_demo_output,
      lv_msg              TYPE string,
      lo_response         TYPE REF TO /goog/cl_model_response.

    FIELD-SYMBOLS:
      <ls_text_sysins> TYPE soli,
      <ls_text_prompt> TYPE soli,
      <ls_file_data>   TYPE gty_file_data.

    CLEAR lt_file_data.

    DATA(lv_json_schema) =
      get_json_schema(
         iv_v2_model  = p_v2_model
         iv_v2_entity = p_v2_entity
         iv_struct = p_struct ).

    IF lv_json_schema IS INITIAL.
      MESSAGE 'JSON Schema not determined. Provide a valid OData V2 or V4 service and entity' TYPE 'I'.
      RETURN.
    ENDIF.

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
          "Multi-media input
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

          IF lt_file_data IS INITIAL AND
             p_gcs IS INITIAL.

            MESSAGE 'Provide a file input' TYPE 'I'.
            RETURN.
          ENDIF.

          " Multi-modal input
          lv_prompt = 'Convert the file input into JSON schema: ' && space && lv_json_schema.

        ELSE.

          IF lv_prompt IS INITIAL.
            MESSAGE 'Provide a text input' TYPE 'I'.
            RETURN.
          ENDIF.

          " Text Input
          lv_prompt = 'Convert the following User Input into JSON schema: ' && lv_json_schema && ' User Input: ' &&  lv_prompt.

        ENDIF.

        lo_response = lo_generative_model->set_response_mime_type( 'application/json' )->generate_content( lv_prompt ).
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

  METHOD get_json_schema_v2.

    DATA(lo_metadata_provider) = /iwbep/cl_mgw_med_provider=>get_med_provider_wo_cache( ).

    TRY.
        DATA(lo_model) = lo_metadata_provider->get_model(
           EXPORTING
             iv_technical_name           = CONV #( iv_v2_model )
             iv_version                  = 0001
             ).

        DATA(lo_entity) = lo_model->get_entity_type( CONV #( iv_v2_entity ) ).

        DATA(lt_properties) = lo_entity->get_properties( ).

        DATA: lt_properties_comp TYPE abap_component_tab.

        DATA(ls_schema_object) = VALUE t_schema_object( type = 'OBJECT' ).

        LOOP AT lt_properties REFERENCE INTO DATA(ls_properties).

          DATA(lo_property_inner_field) =
            cl_abap_structdescr=>get(
              VALUE #(
                ( name = 'TYPE' type = cl_abap_elemdescr=>get_string( ) ) ) ).

          APPEND VALUE #( name = ls_properties->name
                          type = lo_property_inner_field )
                          TO lt_properties_comp.
        ENDLOOP.

        DATA(lo_properties_struc) = cl_abap_structdescr=>get( lt_properties_comp ).
        CREATE DATA ls_schema_object-properties TYPE HANDLE lo_properties_struc.
        ASSIGN ls_schema_object-properties->* TO FIELD-SYMBOL(<ls_properties>).

        LOOP AT lt_properties REFERENCE INTO DATA(ls_properties_upd).
          DATA(lv_index) = sy-tabix.

          ASSIGN COMPONENT lv_index OF STRUCTURE <ls_properties> TO FIELD-SYMBOL(<ls_comp_outer>).
          ASSIGN COMPONENT 1 OF STRUCTURE <ls_comp_outer> TO FIELD-SYMBOL(<lv_comp_inner>).

          DATA(lv_edm_type) = ls_properties_upd->property->/iwbep/if_mgw_odata_re_prop~get_core_type( ).

          <lv_comp_inner> = edm_to_swagger( CONV #( lv_edm_type+4 ) ).
        ENDLOOP.

        rv_schema = /ui2/cl_json=>serialize(
                          data = ls_schema_object
                          ts_as_iso8601 = abap_true
                          compress      = abap_true
                          pretty_name   = /ui2/cl_json=>pretty_mode-low_case ).

      CATCH /iwbep/cx_mgw_med_exception INTO DATA(lo_mgw_ex).
        MESSAGE lo_mgw_ex->get_text( ) TYPE 'I'.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD get_json_schema_struct.

    DATA: lt_struc_components TYPE abap_component_tab.

    DATA(lo_struct) =  CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( iv_struct ) ).

    construct_schema_struct(
      EXPORTING it_components = lo_struct->get_components( )
      CHANGING ct_components = lt_struc_components ).

    DATA(ls_schema_object) = VALUE t_schema_object( type = 'OBJECT' ).
    DATA(lo_properties_struc) = cl_abap_structdescr=>get( lt_struc_components ).
    CREATE DATA ls_schema_object-properties TYPE HANDLE lo_properties_struc.
    ASSIGN ls_schema_object-properties->* TO FIELD-SYMBOL(<ls_properties>).

    populate_schema_struct( EXPORTING it_components = lo_struct->get_components( )
                            CHANGING cs_schema = <ls_properties> ).

    rv_schema = /ui2/cl_json=>serialize(
                      data = ls_schema_object
                      ts_as_iso8601 = abap_true
                      compress      = abap_true
                      pretty_name   = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.

  METHOD construct_schema_struct.

    LOOP AT it_components REFERENCE INTO DATA(ls_components).

      CASE ls_components->type->kind.
        WHEN cl_abap_structdescr=>kind_elem.
          DATA(lo_property_inner_field) =
            cl_abap_structdescr=>get(
              VALUE #(
                ( name = 'TYPE' type = cl_abap_elemdescr=>get_string( ) ) ) ).

          APPEND VALUE #( name = ls_components->name
                          type = lo_property_inner_field )
                          TO ct_components.

        WHEN cl_abap_structdescr=>kind_struct.

          DATA: lt_struct_components TYPE abap_component_tab.

          construct_schema_struct(
              EXPORTING it_components = CAST cl_abap_structdescr( ls_components->type )->get_components( )
              CHANGING ct_components = lt_struct_components ).

          APPEND VALUE #( name = ls_components->name
                     type = cl_abap_structdescr=>get( VALUE #(
                         ( name = 'TYPE' type = cl_abap_elemdescr=>get_string( ) )
                         ( name = 'PROPERTIES' type = cl_abap_structdescr=>get( lt_struct_components ) ) ) ) )
            TO ct_components.

        WHEN cl_abap_structdescr=>kind_table.

          DATA: lt_table_components TYPE abap_component_tab.

          construct_schema_struct(
              EXPORTING it_components =
                     CAST cl_abap_structdescr(
                        CAST cl_abap_tabledescr( ls_components->type
                        )->get_table_line_type( ) )->get_components( )
              CHANGING ct_components = lt_table_components ).

          APPEND VALUE #( name = ls_components->name
                          type = cl_abap_structdescr=>get( VALUE #(
                           ( name = 'TYPE' type = cl_abap_elemdescr=>get_string( ) )
                           ( name = 'ITEMS' type = cl_abap_structdescr=>get( VALUE #(
                              ( name = 'TYPE' type = cl_abap_elemdescr=>get_string( ) )
                              ( name = 'PROPERTIES' type = cl_abap_structdescr=>get( lt_table_components ) ) ) ) ) ) ) )
                 TO ct_components.

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD populate_schema_struct.

    LOOP AT it_components REFERENCE INTO DATA(ls_components).
      DATA(lv_index) = sy-tabix.
      CASE ls_components->type->kind.
        WHEN cl_abap_structdescr=>kind_elem.
          ASSIGN COMPONENT lv_index OF STRUCTURE cs_schema TO FIELD-SYMBOL(<ls_comp_outer>).
          ASSIGN COMPONENT 1 OF STRUCTURE <ls_comp_outer> TO FIELD-SYMBOL(<lv_comp_inner>).

          <lv_comp_inner> = type_kind_to_swagger( ls_components->type->type_kind ).

        WHEN cl_abap_structdescr=>kind_struct.

          ASSIGN COMPONENT ls_components->name OF STRUCTURE cs_schema TO FIELD-SYMBOL(<ls_object_struc_outer>).

          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_object_struc_outer> TO FIELD-SYMBOL(<ls_object_struc_type>).
          <ls_object_struc_type> = 'OBJECT'.
          ASSIGN COMPONENT 'PROPERTIES' OF STRUCTURE <ls_object_struc_outer> TO FIELD-SYMBOL(<ls_object_struc_properties>).

          populate_schema_struct( EXPORTING it_components = CAST cl_abap_structdescr( ls_components->type )->get_components( )
                                  CHANGING  cs_schema = <ls_object_struc_properties> ).

        WHEN cl_abap_structdescr=>kind_table.

          ASSIGN COMPONENT ls_components->name OF STRUCTURE cs_schema TO FIELD-SYMBOL(<ls_array_struc_outer>).
          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_array_struc_outer> TO FIELD-SYMBOL(<ls_array_struc_outer_type>).
          <ls_array_struc_outer_type> = 'ARRAY'.
          ASSIGN COMPONENT 'ITEMS' OF STRUCTURE <ls_array_struc_outer> TO FIELD-SYMBOL(<ls_array_struc_items>).
          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_array_struc_items> TO FIELD-SYMBOL(<ls_array_struc_items_type>).
          <ls_array_struc_items_type> = 'OBJECT'.
          ASSIGN COMPONENT 'PROPERTIES' OF STRUCTURE <ls_array_struc_items> TO FIELD-SYMBOL(<ls_array_struc_properties>).


          populate_schema_struct( EXPORTING it_components = CAST cl_abap_structdescr(
                        CAST cl_abap_tabledescr( ls_components->type
                        )->get_table_line_type( ) )->get_components( )
                           CHANGING  cs_schema = <ls_array_struc_properties> ).

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_json_schema.

    IF iv_v2_model IS NOT INITIAL AND
       iv_v2_entity IS NOT INITIAL AND
       iv_struct IS NOT INITIAL.

      MESSAGE 'Provide either V2 OData or a structure ' TYPE 'I'.
      RETURN.
    ENDIF.

    IF iv_v2_model IS NOT INITIAL AND
       iv_v2_entity IS NOT INITIAL.

      rv_schema =
         get_json_schema_v2( iv_v2_model = iv_v2_model
                             iv_v2_entity = iv_v2_entity ).

    ENDIF.

    IF iv_struct IS NOT INITIAL.

      rv_schema = get_json_schema_struct( iv_struct ).

    ENDIF.

  ENDMETHOD.

  METHOD type_kind_to_swagger.

    CASE iv_type_kind.
      WHEN cl_abap_typedescr=>typekind_any.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_char.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_class.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_clike.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_csequence.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_data.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_date.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_decfloat.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_decfloat16.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_decfloat34.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_dref.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_float.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_hex.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_int.
        rv_swagger = 'INTEGER'.
      WHEN cl_abap_typedescr=>typekind_int1.
        rv_swagger = 'INTEGER'.
      WHEN cl_abap_typedescr=>typekind_int8.
        rv_swagger = 'INTEGER'.
      WHEN cl_abap_typedescr=>typekind_int2.
        rv_swagger = 'INTEGER'.
      WHEN cl_abap_typedescr=>typekind_intf.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_iref.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_num.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_numeric.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_oref.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_packed.
        rv_swagger = 'NUMBER'.
      WHEN cl_abap_typedescr=>typekind_simple.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_string.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_time.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_utclong.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_w.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_xsequence.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_xstring.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_bref.
        rv_swagger = 'STRING'.
      WHEN cl_abap_typedescr=>typekind_enum.
        rv_swagger = 'STRING'.
      WHEN OTHERS.
        rv_swagger = 'STRING'.
    ENDCASE.


  ENDMETHOD.

  METHOD edm_to_swagger.

    CASE iv_edm.
      WHEN 'AnnotationPath'.
        rv_swagger = 'STRING'.
      WHEN 'Binary'.
        rv_swagger = 'STRING'.
      WHEN 'Boolean'.
        rv_swagger = 'BOOLEAN'.
      WHEN 'Byte'.
        rv_swagger = 'STRING'.
      WHEN 'Date'.
        rv_swagger = 'STRING'.
      WHEN 'DateTime'.
        rv_swagger = 'STRING'.
      WHEN 'DateTimeOffset'.
        rv_swagger = 'STRING'.
      WHEN 'Decimal'.
        rv_swagger = 'NUMBER'.
      WHEN 'Double'.
        rv_swagger = 'NUMBER'.
      WHEN 'Duration'.
        rv_swagger = 'NUMBER'.
      WHEN 'Float'.
        rv_swagger = 'NUMBER'.
      WHEN 'Guid'.
      WHEN 'Int16'.
        rv_swagger = 'INTEGER'.
      WHEN 'Int32'.
        rv_swagger = 'INTEGER'.
      WHEN 'Int64'.
        rv_swagger = 'INTEGER'.
      WHEN 'NavigationPropertyPath'.
        rv_swagger = 'STRING'.
      WHEN 'PrimitiveType'.
        rv_swagger = 'STRING'.
      WHEN 'PropertyPath'.
        rv_swagger = 'STRING'.
      WHEN 'SByte'.
        rv_swagger = 'STRING'.
      WHEN 'Single'.
        rv_swagger = 'STRING'.
      WHEN 'Stream'.
        rv_swagger = 'STRING'.
      WHEN 'String'.
        rv_swagger = 'STRING'.
      WHEN 'Time'.
        rv_swagger = 'STRING'.
      WHEN 'TimeOfDay'.
        rv_swagger = 'STRING'.
      WHEN OTHERS.
    ENDCASE.

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

  lcl_main=>set_pf_status( ).

ENDMODULE.
