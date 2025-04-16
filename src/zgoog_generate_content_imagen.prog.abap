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

REPORT zgoog_generate_content_imagen.

PARAMETERS:
  p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
  p_model  TYPE string OBLIGATORY LOWER CASE,
  p_gcs    TYPE string OBLIGATORY LOWER CASE,
  p_loc_id TYPE string OBLIGATORY LOWER CASE,
  p_prompt TYPE string OBLIGATORY LOWER CASE,
  p_no_img TYPE i DEFAULT 4.


TYPES:
  BEGIN OF ty_instance,
    prompt TYPE string,

  END OF ty_instance,

  tt_instances TYPE STANDARD TABLE OF ty_instance,

  BEGIN OF ty_parameters,
    sample_count TYPE i,
    aspect_ratio TYPE string,
    storage_uri  TYPE string,

  END OF ty_parameters.

DATA:
  ls_input      TYPE /goog/cl_aiplatform_v1=>ty_001,
  ls_instance   TYPE ty_instance,
  lt_instances  TYPE tt_instances,
  ls_parameters TYPE ty_parameters,
  lv_raw        TYPE string.

TRY.

* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_aiplatform_v1( iv_key_name = p_key ).

    ls_instance-prompt = p_prompt.
    APPEND ls_instance TO lt_instances.
    CLEAR ls_instance.

    GET REFERENCE OF lt_instances INTO ls_input-instances.

    ls_parameters-sample_count = p_no_img.
    ls_parameters-storage_uri = p_gcs.
    GET REFERENCE OF ls_parameters INTO ls_input-parameters.

* Call API method: aiplatform.projects.locations.publishers.models.predict
    CALL METHOD lo_client->predict_models
      EXPORTING
        iv_p_projects_id   = CONV #( lo_client->gv_project_id )
        iv_p_locations_id  = p_loc_id
        iv_p_publishers_id = 'google'
        iv_p_models_id     = p_model
        is_input           = ls_input
      IMPORTING
        es_raw             = lv_raw
        es_output          = DATA(ls_output)
        ev_ret_code        = DATA(lv_ret_code)
        ev_err_text        = DATA(lv_err_text)
        es_err_resp        = DATA(ls_err_resp).
    IF lo_client->is_success( lv_ret_code ).
      cl_demo_output=>display( |Images generated and placed in GCS Bucket | && p_gcs ).
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.

ENDTRY.
