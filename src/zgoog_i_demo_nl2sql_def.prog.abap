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

DATA : gr_cont_prompt   TYPE REF TO cl_gui_custom_container,
       gr_cont_response TYPE REF TO cl_gui_custom_container,
       gr_text_prompt   TYPE REF TO cl_gui_textedit,
       gr_text_response TYPE REF TO cl_gui_textedit,
       gt_text_prompt   TYPE soli_tab,
       p_model_key      TYPE /goog/model_key,
       p_cds_view       TYPE ddlname.

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_containers,
      create_text_editors,
      read_text_editor,
      set_pf_status.

  PRIVATE SECTION.
    CLASS-METHODS:
      execute,
      convert_string_to_table
        IMPORTING
          iv_response      TYPE string
        EXPORTING
          et_text_response TYPE soli_tab.

ENDCLASS.
