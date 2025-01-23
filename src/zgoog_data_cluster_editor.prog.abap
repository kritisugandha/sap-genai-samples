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
REPORT zgoog_data_cluster_editor.

PARAMETERS: p_relid TYPE zgoog_clust_conf-relid OBLIGATORY DEFAULT 'XX',
            p_srtfd TYPE zgoog_clust_conf-srtfd OBLIGATORY,
            p_edit  TYPE flag.

DATA: go_container TYPE REF TO cl_gui_custom_container.
DATA: go_editor TYPE REF TO cl_proxy_xml_edit.
DATA: gs_config TYPE zgoog_clust_conf.
DATA: gv_xstring TYPE xstring.

CLASS lcl_editor DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS: start,
      pbo,
      pai.

ENDCLASS.


START-OF-SELECTION.

  lcl_editor=>start( ).

CLASS lcl_editor IMPLEMENTATION.

  METHOD start.

    SELECT SINGLE *
      FROM zgoog_clust_conf
      INTO gs_config
      WHERE relid = p_relid
        AND srtfd = p_srtfd.

    IF gs_config IS INITIAL.
      MESSAGE: 'Entry not found' TYPE 'S'.
      RETURN.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.

  METHOD pbo.

    DATA: lt_fcode TYPE TABLE OF sy-ucomm.

    IF p_edit IS INITIAL.
      APPEND 'SAVE' TO lt_fcode.
    ENDIF.

    SET PF-STATUS 'STATUS_0100' EXCLUDING lt_fcode.
    SET TITLEBAR 'TITL_0100'.

    IF  go_container IS INITIAL.

      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_INPUT'.

      CREATE OBJECT go_editor
        EXPORTING
          parent = go_container.

      DATA: lo_serial TYPE REF TO object.
      DATA: lr_data TYPE REF TO data.

      FIELD-SYMBOLS:
        <lv_data> TYPE any.

      IF gs_config-class IS NOT INITIAL.
        ASSIGN lo_serial TO <lv_data>.
      ELSE.
        CREATE DATA lr_data TYPE (gs_config-data_type).
        ASSIGN lr_data->* TO <lv_data>.
      ENDIF.

      CALL METHOD zgoog_cl_data_cluster=>import_data
        EXPORTING
          iv_table = gs_config-cluster_table
          iv_area  = gs_config-relid
          iv_id    = gs_config-srtfd
        IMPORTING
          ev_data  = <lv_data>.

      IF <lv_data> IS INITIAL AND
         gs_config-class IS NOT INITIAL.
        CREATE OBJECT lo_serial TYPE (gs_config-class).
      ENDIF.

      CALL METHOD zgoog_cl_data_cluster=>abap_to_json
        EXPORTING
          iv_data         = <lv_data>
          iv_pretty       = abap_true
        IMPORTING
          ev_json_xstring = gv_xstring.

      go_editor->set_xstring( gv_xstring ).

      go_editor->toggle_change_mode( ).

      IF p_edit IS INITIAL.
        go_editor->set_readonly( abap_true ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD pai.

    CASE sy-ucomm.
      WHEN 'BACK' OR 'UP' OR 'EXIT'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'SAVE'.

        DATA: lv_xstring TYPE xstring.

        lv_xstring = go_editor->get_xstring( ).

        DATA: lo_serial TYPE REF TO object.
        DATA: lr_data TYPE REF TO data.

        FIELD-SYMBOLS:
          <lv_data> TYPE any.

        IF gs_config-class IS NOT INITIAL.
          ASSIGN lo_serial TO <lv_data>.
        ELSE.
          CREATE DATA lr_data TYPE (gs_config-data_type).
          ASSIGN lr_data->* TO <lv_data>.
        ENDIF.


        CALL METHOD zgoog_cl_data_cluster=>json_to_abap
          EXPORTING
            iv_json_xstring = lv_xstring
          IMPORTING
            ev_data         = <lv_data>.

        DATA: lv_count TYPE i.
        CALL METHOD zgoog_cl_data_cluster=>export_data
          EXPORTING
            iv_table    = gs_config-cluster_table
            iv_area     = gs_config-relid
            iv_id       = gs_config-srtfd
            iv_data     = <lv_data>
            iv_format   = if_sxml=>co_xt_json
          IMPORTING
            ev_db_count = lv_count.

        IF lv_count > 0.
          MESSAGE 'Data saved successfully' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.

  lcl_editor=>pbo( ).


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.

  lcl_editor=>pai( ).


ENDMODULE.
