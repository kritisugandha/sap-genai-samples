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

TYPES:
  BEGIN OF t_schema_object,
    type       TYPE string,
    properties TYPE REF TO data,
  END OF t_schema_object.

TYPES:
  BEGIN OF t_schema_array,
    type  TYPE string,
    items TYPE REF TO data,
  END OF t_schema_array.

TYPES:
  BEGIN OF gty_file_table,
    mime_type TYPE string,
    filename  TYPE file_table-filename,

  END OF gty_file_table.

TYPES:
  BEGIN OF gty_file_data,
    filename  TYPE string,
    mime_type TYPE string,
    filedata  TYPE string,

  END OF gty_file_data,

  gtt_file_data TYPE STANDARD TABLE OF gty_file_data.

DATA : gr_cont_prompt   TYPE REF TO cl_gui_custom_container,
       gr_cont_response TYPE REF TO cl_gui_custom_container,
       gr_text_prompt   TYPE REF TO cl_gui_textedit,
       gr_text_response TYPE REF TO cl_gui_textedit,
       gt_text_prompt   TYPE soli_tab,
       p_rb1            TYPE abap_bool VALUE abap_true,
       p_rb2            TYPE abap_bool,
       p_model_key      TYPE /goog/model_key,
       p_v2_model       TYPE string,
       p_v2_entity      TYPE string,
       p_struct         TYPE string,
       p_mime_gcs       TYPE string VALUE 'application/pdf',
       p_mime_file      TYPE string VALUE 'application/pdf',
       p_gcs            TYPE string,
       gt_file_table    TYPE STANDARD TABLE OF gty_file_table,
       p_no_files       TYPE string.

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.

    TYPES: tt_entity TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    TYPES:
      BEGIN OF t_entity_hash,
        entity  TYPE string,
        parents TYPE tt_entity,
      END OF t_entity_hash,

      tt_entity_hash TYPE HASHED TABLE OF t_entity_hash WITH UNIQUE KEY entity.

    CLASS-METHODS:
      authorization_check,
      create_containers,
      create_text_editors,
      read_text_editor,
      set_pf_status.

  PRIVATE SECTION.
    CLASS-METHODS:
      execute,
      browse_files,
      get_file_data
        EXPORTING
          ev_error     TYPE abap_bool
          et_file_data TYPE gtt_file_data,
      convert_string_to_table
        IMPORTING
          iv_response      TYPE string
        EXPORTING
          et_text_response TYPE soli_tab,

      edm_to_swagger
        IMPORTING iv_edm            TYPE string
        RETURNING VALUE(rv_swagger) TYPE string,

      type_kind_to_swagger
        IMPORTING iv_type_kind    TYPE ABAP_TYPEKIND
        RETURNING VALUE(rv_swagger) TYPE string,

      get_json_schema
        IMPORTING iv_v2_model      TYPE string
                  iv_v2_entity     TYPE string
                  iv_struct        TYPE string
        RETURNING VALUE(rv_schema) TYPE string,

      get_json_schema_v2
        IMPORTING iv_v2_model      TYPE string
                  iv_v2_entity     TYPE string
        RETURNING VALUE(rv_schema) TYPE string,

      get_json_schema_struct
        IMPORTING iv_struct        TYPE string
        RETURNING VALUE(rv_schema) TYPE string,

      construct_schema_struct
        IMPORTING it_components    TYPE abap_component_tab
        CHANGING  ct_components TYPE abap_component_tab,

      populate_schema_struct
        IMPORTING it_components    TYPE abap_component_tab
        CHANGING  cs_schema TYPE any.

ENDCLASS.
