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

REPORT zgoog_bq_gemini_sample.

PARAMETERS:
  p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
  p_loc_id TYPE string OBLIGATORY DEFAULT 'us-central1' LOWER CASE,
  p_query  TYPE string OBLIGATORY LOWER CASE.

DATA:
  lv_prompt_text      TYPE string,
  ls_query_parameter  TYPE /goog/cl_bq_query=>ty_query_parameter,
  lt_query_parameters TYPE /goog/cl_bq_query=>ty_t_query_parameters.

lv_prompt_text = |You are a legal advisor for my company. Your job is to analyze delivery comments | &&
                 |in a purchase order and look for any risky clauses for my company. Respond to me in | &&
                 |plain text JSON format with 4 fields. Field 1 should be "issue" with possible values | &&
                 |Yes or No. Field 2 should be "issueCategory" with possible values Indemnification | &&
                 |Overreach, Unilateral Termination Clause, Confidentiality Overextension, Unreasonable | &&
                 |Warranty, Ambiguous Governing Law, Unclear Dispute Resolution, Force Majeure Limitation, | &&
                 |Unbalanced Liability, Intellectual Property Ambiguity, Compliance with Sanctions, | &&
                 |Others or None. Field 3 should be "explanation" - use this field to give a short | &&
                 |explanation for your response on Field 1. Field 4 should be "recommendation" - use | &&
                 |this field to give a short recommendation of how to mitigate such issues. DO NOT INCLUDE | &&
                 |BACKTICKS IN THE RESPONSE. Analyze the given delivery comment text and let me know if | &&
                 |it could cause issues to my company. Delivery comment: |.

ls_query_parameter-parameter_name  = 'PROMPT_TEXT'.
ls_query_parameter-parameter_type  = 'STRING'.
ls_query_parameter-parameter_value = lv_prompt_text.

APPEND ls_query_parameter TO lt_query_parameters.
CLEAR ls_query_parameter.

TRY.
    DATA(lo_bq_query) = NEW /goog/cl_bq_query( iv_key         = p_key
                                               iv_location_id = p_loc_id
                                               iv_query_name  = p_query ).
    lo_bq_query->set_query_parameters( lt_query_parameters ).

    DATA(lo_bq_generative_model) = NEW /goog/cl_bq_generative_model( iv_key = p_key ).
    DATA(lt_bq_gemini_response) = lo_bq_generative_model->execute_query( lo_bq_query
                                                       )->get_text_response( ).
    IF lt_bq_gemini_response IS NOT INITIAL.
      cl_demo_output=>display( lt_bq_gemini_response ).

    ENDIF.
  CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
    cl_demo_output=>display( lo_cx_sdk->get_text( ) ).

ENDTRY.
