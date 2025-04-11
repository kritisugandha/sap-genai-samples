************************************************************************************************************************
* Copyright 2024 Google LLC                                                                                            *
* ABAP SDK for Google Cloud is made available as "Software" under the agreement governing your use of                  *
* Google Cloud Platform including the Service Specific Terms available at                                              *
*                                                                                                                      *
* https://cloud.google.com/terms/service-terms                                                                         *
*                                                                                                                      *
* Without limiting the generality of the above terms, you may not modify or distribute ABAP SDK for Google Cloud       *
* without express written permission from Google.                                                                      *
************************************************************************************************************************

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

DATA : gr_cont_sysins   TYPE REF TO cl_gui_custom_container,
       gr_cont_prompt   TYPE REF TO cl_gui_custom_container,
       gr_cont_response TYPE REF TO cl_gui_custom_container,
       gr_text_sysins   TYPE REF TO cl_gui_textedit,
       gr_text_prompt   TYPE REF TO cl_gui_textedit,
       gr_text_response TYPE REF TO cl_gui_textedit,
       gt_text_sysins   TYPE soli_tab,
       gt_text_prompt   TYPE soli_tab,
       p_rb1            TYPE abap_bool VALUE abap_true,
       p_rb2            TYPE abap_bool,
       p_model_key      TYPE /goog/model_key,
       p_mime_gcs       TYPE string VALUE 'application/pdf',
       p_mime_file      TYPE string VALUE 'application/pdf',
       p_gcs            TYPE string,
       gt_file_table    TYPE STANDARD TABLE OF gty_file_table,
       p_no_files       TYPE string,
       gv_first_run     TYPE abap_bool VALUE abap_true.

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
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
          et_text_response TYPE soli_tab.

ENDCLASS.
