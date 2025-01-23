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
class ZGOOG_CL_NL2SQL_CDS_VIEW definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_MODEL_KEY type /GOOG/MODEL_KEY
      !IV_LOG_OBJ type BALOBJ_D optional
      !IV_LOG_SUBOBJ type BALSUBOBJ optional
    raising
      /GOOG/CX_SDK .
  methods GENERATE_SQL
    importing
      !IV_PROMPT type STRING
      !IV_CDS_VIEW type DDLNAME
    returning
      value(RV_ABAP_SQL) type STRING
    raising
      /GOOG/CX_SDK .
protected section.

  data GO_GENAI_MODEL type ref to /GOOG/CL_GENERATIVE_MODEL .
private section.
ENDCLASS.



CLASS ZGOOG_CL_NL2SQL_CDS_VIEW IMPLEMENTATION.


  METHOD constructor.

    go_genai_model = NEW #(
      iv_model_key = iv_model_key
      iv_log_obj   = iv_log_obj
      iv_log_subobj = iv_log_subobj ).

  ENDMETHOD.


  METHOD generate_sql.

    DATA(lo_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        lo_ddl->read(
          EXPORTING
            name             = iv_cds_view
            get_state        = 'M'
            withtext         = abap_true
            langu            = 'E'
          IMPORTING
            ddddlsrcv_wa     = DATA(ls_ddl_src)
            baseinfo_string  = DATA(lv_baseinfo) ).

      CATCH cx_dd_ddl_read INTO DATA(lo_ddl_read).
        RAISE EXCEPTION TYPE /goog/cx_sdk
          EXPORTING
            previous = lo_ddl_read.
    ENDTRY.

    DATA(lv_prompt) = 'Your task is to generate ABAP SQL statement for the given prompt below.' &&
                 'Only output the SQL statement without any annotations.' &&
                 'Generate a SELECT * statement by only using fields of DDL statement in where clause.' &&
                 cl_abap_char_utilities=>cr_lf &&
                 'DDL Statement : ' && ls_ddl_src-source && '.' &&
                 cl_abap_char_utilities=>cr_lf &&
                 'User Prompt: ' && iv_prompt.

    DATA(lo_response) =  go_genai_model->generate_content( lv_prompt ).

    rv_abap_sql = lo_response->get_text( ).


  ENDMETHOD.
ENDCLASS.
