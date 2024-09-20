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
REPORT zr_hazmat_ingest_data_gcs.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_tbuc TYPE c LENGTH 30 LOWER CASE DEFAULT 'hazmat-data',
              p_ckey TYPE /goog/keyname          DEFAULT 'DEMO_AIPLATFORM',
              p_mkey TYPE /goog/model_key LOWER CASE DEFAULT 'Gemini'.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS: p_sds  RADIOBUTTON GROUP rbg1 DEFAULT 'X' USER-COMMAND uc1,
              p_wsg  RADIOBUTTON GROUP rbg1,
              p_pict RADIOBUTTON GROUP rbg1,
              p_prod RADIOBUTTON GROUP rbg1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
  PARAMETERS: p_sbuc TYPE c LENGTH 30 LOWER CASE DEFAULT 'hazmat-data' MODIF ID lc1.
SELECTION-SCREEN END OF BLOCK b3.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_prod = abap_true.
      IF screen-group1 = 'LC1'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  IF p_sds = abap_true.
    DATA(lv_data_type) = zcl_hazmat_gcs_data_ingester=>c_data_type-sds_documents.
  ELSEIF p_prod = abap_true.
    lv_data_type = zcl_hazmat_gcs_data_ingester=>c_data_type-product_data.
  ELSEIF p_wsg = abap_true.
    lv_data_type = zcl_hazmat_gcs_data_ingester=>c_data_type-wsg_documents.
  ELSEIF p_pict = abap_true.
    lv_data_type = zcl_hazmat_gcs_data_ingester=>c_data_type-pic_images.
  ELSE.
    MESSAGE 'Unknown selection' TYPE 'E'.
  ENDIF.

  TRY.
      DATA(lo_document_ingester) = NEW zcl_hazmat_gcs_data_ingester(
        iv_key_name   = p_ckey
        iv_model_key  = p_mkey
        iv_src_bucket = CONV #( p_sbuc )
        iv_tgt_bucket = CONV #( p_tbuc )
      ).

      lo_document_ingester->execute( EXPORTING iv_data_type = lv_data_type
                                     IMPORTING ev_err_text  = DATA(lv_err_text) ).
      IF lv_err_text IS NOT INITIAL.
        MESSAGE lv_err_text TYPE 'E'.
      ELSE.
        MESSAGE 'Embeddings creation was successful' TYPE 'S'.
      ENDIF.
    CATCH /goog/cx_sdk.

  ENDTRY.
