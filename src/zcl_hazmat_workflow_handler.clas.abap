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
class ZCL_HAZMAT_WORKFLOW_HANDLER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_wsg_chunk_json,
        id      TYPE string,
        content TYPE string,
      END OF ty_wsg_chunk_json .
  types:
    ty_t_float TYPE STANDARD TABLE OF f WITH DEFAULT KEY .
  types:
    BEGIN OF ty_sds_chunk_json,
        id        TYPE string,
        content   TYPE string,
        embedding TYPE ty_t_float,

      END OF ty_sds_chunk_json .

  class-methods WF_FUNC_CALLING
    importing
      !IV_PROMPT type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods WF_GET_SDS_DATA
    importing
      !IV_PROMPT type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods WF_GET_WSG_DATA
    importing
      !IV_PROMPT type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods WF_GET_PICTOGRAM_DATA
    importing
      !IV_PROMPT type STRING optional
      !IV_IMAGE_BASE64 type STRING optional
    returning
      value(RV_RESPONSE) type STRING .
  PROTECTED SECTION.
private section.

  class-methods GET_PRODUCT_ID
    importing
      !IV_PROMPT type STRING
    returning
      value(RV_PRODUCT_ID) type STRING .
  class-methods GET_WSG_CHUNK_DATA
    importing
      !IV_WSG_CHUNK_ID type STRING
    returning
      value(RV_CHUNK_TEXT) type STRING .
  class-methods GET_PICTOGRAM_DATA
    importing
      !IV_PICTOGRAM_ID type STRING
    returning
      value(RV_PICTOGRAM_DESCRIPTION) type STRING .
  class-methods GET_SDS_CHUNK_DATA
    importing
      !IV_SDS_CHUNK_ID type STRING
    returning
      value(RV_CHUNK_TEXT) type STRING .
ENDCLASS.



CLASS ZCL_HAZMAT_WORKFLOW_HANDLER IMPLEMENTATION.


  METHOD GET_PICTOGRAM_DATA.

    DATA:
      lv_p_bucket       TYPE string,
      lv_p_object       TYPE string,
      lv_xfile          TYPE xstring,
      ls_output         TYPE /goog/cl_storage_v1=>ty_013,
      lv_ret_code       TYPE i,
      lv_err_text       TYPE string,
      ls_err_resp       TYPE /goog/err_resp,
      lv_msg            TYPE string,
      lo_exception      TYPE REF TO /goog/cx_sdk,
      lo_client         TYPE REF TO /goog/cl_storage_v1,
      ls_sds_chunk_data TYPE ty_wsg_chunk_json,
      lv_string         TYPE string.
    DATA lv_key       TYPE /goog/keyname.


    lv_key = 'DEMO_AIPLATFORM'.
    TRY.


        CREATE OBJECT lo_client EXPORTING iv_key_name = lv_key.

        lo_client->add_common_qparam( iv_name  = 'alt'
                                      iv_value = 'media' ).

        lv_p_bucket = 'hazmat-pictogram-description'.
        lv_p_object = iv_pictogram_id && '.txt'.

        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = lv_p_bucket
            iv_p_object = lv_p_object
          IMPORTING
            es_output   = ls_output
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp
            es_raw      = lv_xfile ).
      CATCH /goog/cx_sdk INTO lo_exception.
        lv_msg = lo_exception->get_text( ).
        RETURN.

    ENDTRY.

    IF lo_client->is_success( lv_ret_code ) = abap_true.
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = lv_xfile
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = lv_string.

      rv_pictogram_description = lv_string.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.

    ENDIF.


  ENDMETHOD.


  METHOD get_product_id.

    TRY.
        DATA(lo_vector_search_prod) = NEW /goog/cl_vector_search( iv_search_key = 'HPRO_PROD'  ).
        DATA(ls_nearest_product) = lo_vector_search_prod->find_neighbors_by_string( iv_search_string         = iv_prompt           " Search String
                                                                                    iv_embeddings_model_key  = 'Text-Embeddings'   " Model Key for Embeddings
                                                     )->get_nearest_neighbor( ).

        rv_product_id = ls_nearest_product-datapoint_id.

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        DATA(lv_msg) = lo_exception->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_sds_chunk_data.

    DATA:
      lv_p_bucket       TYPE string,
      lv_p_object       TYPE string,
      lv_xfile          TYPE xstring,
      ls_output         TYPE /goog/cl_storage_v1=>ty_013,
      lv_ret_code       TYPE i,
      lv_err_text       TYPE string,
      ls_err_resp       TYPE /goog/err_resp,
      lv_msg            TYPE string,
      lo_exception      TYPE REF TO /goog/cx_sdk,
      lo_client         TYPE REF TO /goog/cl_storage_v1,
      ls_sds_chunk_data TYPE ty_sds_chunk_json,
      lv_string         TYPE string.
    DATA lv_key       TYPE /goog/keyname.


    lv_key = 'DEMO_AIPLATFORM'.
    CREATE OBJECT lo_client EXPORTING iv_key_name = lv_key.

    lo_client->add_common_qparam( iv_name  = 'alt'
                                  iv_value = 'media' ).

    lv_p_bucket = 'hazmat-sds-embeddings-v2'.
    lv_p_object = iv_sds_chunk_id && '.json'.

    TRY.
        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = lv_p_bucket
            iv_p_object = lv_p_object
          IMPORTING
            es_output   = ls_output
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp
            es_raw      = lv_xfile ).
      CATCH /goog/cx_sdk INTO lo_exception.
        lv_msg = lo_exception->get_text( ).
        RETURN.

    ENDTRY.

    IF lo_client->is_success( lv_ret_code ) = abap_true.
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = lv_xfile
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = lv_string.
      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json          = lv_string
                                                      iv_pretty_name   = /ui2/cl_json=>pretty_mode-extended
                                            IMPORTING es_data          = ls_sds_chunk_data ).
      IF ls_sds_chunk_data IS NOT INITIAL.
        rv_chunk_text = ls_sds_chunk_data-content.

      ENDIF.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.

    ENDIF.
  ENDMETHOD.


  METHOD get_wsg_chunk_data.


    DATA:
      lv_p_bucket       TYPE string,
      lv_p_object       TYPE string,
      lv_xfile          TYPE xstring,
      ls_output         TYPE /goog/cl_storage_v1=>ty_013,
      lv_ret_code       TYPE i,
      lv_err_text       TYPE string,
      ls_err_resp       TYPE /goog/err_resp,
      lv_msg            TYPE string,
      lo_exception      TYPE REF TO /goog/cx_sdk,
      lo_client         TYPE REF TO /goog/cl_storage_v1,
      ls_sds_chunk_data TYPE ty_wsg_chunk_json,
      lv_string         TYPE string.
    DATA lv_key       TYPE /goog/keyname.


    lv_key = 'DEMO_AIPLATFORM'.

    TRY.
        CREATE OBJECT lo_client EXPORTING iv_key_name = lv_key.

        lo_client->add_common_qparam( iv_name  = 'alt'
                                      iv_value = 'media' ).

        lv_p_bucket = 'hazmat-wsg-embeddings'.
        lv_p_object = iv_wsg_chunk_id && '.json'.

        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = lv_p_bucket
            iv_p_object = lv_p_object
          IMPORTING
            es_output   = ls_output
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp
            es_raw      = lv_xfile ).
      CATCH /goog/cx_sdk INTO lo_exception.
        lv_msg = lo_exception->get_text( ).
        RETURN.

    ENDTRY.

    IF lo_client->is_success( lv_ret_code ) = abap_true.
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = lv_xfile
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = lv_string.
      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_string
                                                      iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                            IMPORTING es_data        = ls_sds_chunk_data ).
      IF ls_sds_chunk_data IS NOT INITIAL.
        rv_chunk_text = ls_sds_chunk_data-content.

      ENDIF.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.

    ENDIF.


  ENDMETHOD.


  METHOD wf_func_calling.


    DATA: lt_parameters TYPE /goog/cl_generative_model=>tt_parameter_properties.

    DATA(lv_prompt) = iv_prompt.

    "logic to identify and fetch product ID .
    DATA(lv_product_id) = get_product_id( iv_prompt = lv_prompt ).

    lv_prompt = |Product ID: "{ lv_product_id }". { lv_prompt } |.

    TRY.
        DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = 'Gemini' ).

        APPEND VALUE #( parameter_name = 'product'
                        type           = 'string'
                        description    = '6 character Product ID'
                        is_required    = abap_true ) TO lt_parameters.

* Function calling with SAP Function Module
        DATA(lv_response) = lo_model->add_function_declaration( iv_name        = 'ZFM_HAZMAT_GET_PRODUCT_DATA'
                                                                iv_description = 'Get all information for a given product ID'
                                                                it_parameters  = lt_parameters
                                   )->set_auto_invoke_sap_function( abap_true
                                   )->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                           iv_harm_block_threshold = 'BLOCK_NONE'
                                   )->generate_content( iv_prompt_text = lv_prompt
                                   )->get_text( ).

        rv_response = lv_response.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        rv_response = lo_cx_sdk->get_text( ) .

    ENDTRY.

  ENDMETHOD.


  METHOD wf_get_pictogram_data.


    TRY.
        DATA ls_image TYPE /goog/cl_embeddings_model=>ty_image.
        ls_image-bytes_base64_encoded = iv_image_base64.

        DATA(lt_multi_embed) = NEW /goog/cl_embeddings_model( iv_model_key = 'Multimodal-Embedding'
                                    )->gen_image_embeddings( iv_image = ls_image
                                    )->get_vector( ).

        DATA(lo_vector_search_pictogram) = NEW /goog/cl_vector_search( iv_search_key = 'HPRO_PICTOGRAM' ).

        DATA(ls_nearest_pictogram) = lo_vector_search_pictogram->find_neighbors_by_embedding( it_embeddings = lt_multi_embed
                                                              )->get_nearest_neighbor(  ).

        DATA(lv_prompt) = iv_prompt && ' Respond based on the below context. Context: ' && get_pictogram_data( ls_nearest_pictogram-datapoint_id ).

        DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = 'gemini-1.0-pro-vision' ).


        lo_model->set_inline_data( iv_mime_type = 'image/gif'
                                   iv_data      = iv_image_base64 ).

        DATA(lv_response) = lo_model->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                           iv_harm_block_threshold = 'BLOCK_NONE'
                                   )->generate_content( iv_prompt_text = lv_prompt
                                   )->get_text( ).

        rv_response = lv_response.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        rv_response = lo_cx_sdk->get_text( ) .

    ENDTRY.


  ENDMETHOD.


  METHOD wf_get_sds_data.

    TRY.

        "logic to identify and fetch product ID .
        DATA(lv_product_id) = get_product_id( iv_prompt = iv_prompt ).

        " Create a new prompt which will be used for Searching in the SDS Vector Store
        DATA(lv_query_sds_chunk) = iv_prompt &&
                                   'Metadata: Product ID - ' &&
                                   lv_product_id.

        " Get SDS Chunk with additional context containing product ID
        DATA(lo_vector_search_sds) = NEW /goog/cl_vector_search( iv_search_key = 'HPRO_SDS' ).

        DATA(ls_nearest_chunk) = lo_vector_search_sds->find_neighbors_by_string( iv_search_string         = lv_query_sds_chunk   " Search String
                                                                                 iv_embeddings_model_key  = 'Text-Embeddings'    " Model Key for Embeddings
                                                    )->get_nearest_neighbor(  ).

        IF ls_nearest_chunk-datapoint_id IS NOT INITIAL.
          " Create a new Prompt for generative language model using Gemini.
          DATA(lv_prompt) = iv_prompt &&
                            'Context: ' &&
                            get_sds_chunk_data( ls_nearest_chunk-datapoint_id ).

          DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = 'Gemini' ).

          " Get response from API using the RAG-ed prompt.
          rv_response = lo_model->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                       iv_harm_block_threshold = 'BLOCK_NONE'
                               )->generate_content( lv_prompt
                               )->get_text( ).

        ENDIF.


      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        DATA(lv_msg) = lo_cx_sdk->get_text( ).

    ENDTRY.
  ENDMETHOD.


  METHOD wf_get_wsg_data.

    DATA(lv_prompt) = iv_prompt.

    TRY.

        " Create a client for searching the WSG vector Index
        DATA(lo_vector_search_wsg) = NEW /goog/cl_vector_search( iv_search_key = 'HPRO_WSG' ).

        " Use the Prompt embedding to search the WSG Chunk
        DATA(ls_nearest_document) = lo_vector_search_wsg->find_neighbors_by_string( iv_search_string         = lv_prompt
                                                                                    iv_embeddings_model_key  = 'Text-Embeddings'
                                                       )->get_nearest_neighbor( ).

        " Create a new Query with context added from the GUID.
        lv_prompt = lv_prompt && ' Context: ' && get_wsg_chunk_data( ls_nearest_document-datapoint_id ).

        " Create a client for Generative AI Model
        DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = 'Gemini' ).

        " Use the new prompt query to respond to the updated prompt with context.
        DATA(lv_response) = lo_model->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                           iv_harm_block_threshold = 'BLOCK_NONE'
                                   )->generate_content( iv_prompt_text = lv_prompt
                                   )->get_text( ).

        rv_response = lv_response.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        rv_response = lo_cx_sdk->get_text( ) .
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
