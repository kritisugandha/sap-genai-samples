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

REPORT zgoog_bq_embeddings_sample.

PARAMETERS:
  p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
  p_loc_id TYPE string OBLIGATORY DEFAULT 'us-central1' LOWER CASE,
  p_query  TYPE string OBLIGATORY LOWER CASE.

TRY.
    DATA(lo_bq_query) = NEW /goog/cl_bq_query( iv_key         = p_key
                                               iv_location_id = p_loc_id
                                               iv_query_name  = p_query ).

    DATA(lo_bq_embeddings_model) = NEW /goog/cl_bq_embeddings_model( iv_key = p_key ).
    DATA(lo_model_response) = lo_bq_embeddings_model->generate_embeddings( lo_bq_query ).
    lo_model_response->get_query_job_status( IMPORTING ev_job_complete = DATA(lv_job_complete)
                                                       ev_job_id       = DATA(lv_job_id) ).
    DATA(lt_query_job_errors) = lo_model_response->get_query_job_errors( ).
    IF lv_job_complete = abap_true AND
       lt_query_job_errors IS INITIAL.
      cl_demo_output=>display( 'Embeddings generated and BQ Vector DB updated successfully' ).
    ELSEIF lv_job_id IS NOT INITIAL.
      cl_demo_output=>display( 'Query job scheduled to generate embeddings and update BQ Vector DB' ).
    ELSE.
      cl_demo_output=>display( 'Error while updating vector database' ).

    ENDIF.
  CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
    cl_demo_output=>display( lo_cx_sdk->get_text( ) ).

ENDTRY.
