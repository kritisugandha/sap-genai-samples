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
class ZGOOG_CL_DATA_CLUSTER definition
  public
  create public .

public section.

  types:
*"* public components of class ZGOOG_CL_DATA_CLUSTER
*"* do not include other source files here!!!
    tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY .

  constants Q type C value '''' ##NO_TEXT.

  class-methods EXPORT_DATA_MULTIPLE
    importing
      !IV_TABLE type TABNAME
      !IV_DATA_FIELD type FIELDNAME
      !IT_DATA type ANY TABLE
      !IV_FORMAT type IF_SXML=>XML_STREAM_TYPE default IF_SXML=>CO_XT_XML10
    exporting
      !EV_DB_COUNT type I .
  class-methods EXPORT_DATA
    importing
      !IV_TABLE type TABNAME
      !IV_AREA type INDX_RELID
      !IV_ID type CLIKE
      !IV_DATA type ANY
      !IV_FORMAT type IF_SXML=>XML_STREAM_TYPE default IF_SXML=>CO_XT_XML10
      !IS_ADDTL_DATA type ANY optional
    exporting
      !EV_DB_COUNT type I .
  class-methods IMPORT_DATA
    importing
      !IV_TABLE type TABNAME
      !IV_AREA type INDX_RELID
      !IV_ID type CLIKE
    exporting
      !EV_DATA type ANY
      !ES_ADDTL_DATA type ANY .
  class-methods IMPORT_DATA_MULTIPLE
    importing
      !IV_TABLE type TABNAME
      !IV_DATA_FIELD type FIELDNAME
      !IT_KEY type ANY TABLE
    exporting
      !ET_RESULT type ANY TABLE .
  class-methods DESERIALIZE
    importing
      !IV_XSTRING type XSTRING
    exporting
      !EV_DATA type ANY .
  class-methods SERIALIZE
    importing
      !IV_DATA type ANY
      !IV_FORMAT type IF_SXML=>XML_STREAM_TYPE default IF_SXML=>CO_XT_XML10
    exporting
      !EO_XML_WRITER type ref to CL_SXML_STRING_WRITER
      !EV_XSTRING type XSTRING .
  class-methods ABAP_TO_JSON
    importing
      !IV_DATA type ANY
      !IV_PRETTY type FLAG optional
    exporting
      !EV_JSON_XSTRING type XSTRING
      !EV_JSON_STRING type STRING .
  class-methods JSON_TO_ABAP
    importing
      !IV_JSON_STRING type STRING optional
      !IV_JSON_XSTRING type XSTRING optional
    exporting
      !EV_DATA type ANY .
  class-methods DELETE_DATA
    importing
      !IV_TABLE type TABNAME
      !IS_KEY type CLIKE
    exporting
      !EV_DB_COUNT type I .
  class-methods DELETE_DATA_MULTILE
    importing
      !IV_TABLE type TABNAME
      !IT_KEY type ANY TABLE
    exporting
      !EV_DB_COUNT type I .
  PROTECTED SECTION.
private section.

*"* private components of class ZGOOG_CL_DATA_CLUSTER
*"* do not include other source files here!!!
  class-methods BUILD_EXPORT_KEY_ITAB
    importing
      !IS_EXPORT_WA type ANY
      !IV_DATA type ANY
      !IV_FORMAT type IF_SXML=>XML_STREAM_TYPE
    changing
      !CT_EXPORT type STANDARD TABLE
      !CT_KEY type STANDARD TABLE .
  class-methods SET_FVAL
    importing
      !IR_DATA type ref to DATA
      !IV_FIELD type STRING
      !IV_VAL type ANY
    returning
      value(RV_RESULT) type I .
  class-methods GET_CLUSTD_FROM_WA
    importing
      !IS_DB_RECORD type ANY
    changing
      !CV_DATA_STRING type STRING .
  class-methods CONSTRUCT_KEY_TABLE
    importing
      !IV_TABLE type TABNAME
      !IV_INCL_SRTF2 type FLAG optional
    returning
      value(ER_TABDESCR) type ref to DATA .
  class-methods SELECT_RECORDS
    importing
      !IV_TABLE type TABNAME
      !IT_KEY type ANY TABLE
    exporting
      !ET_RESULT type ANY TABLE .
  class-methods COLLECT_RESULT
    importing
      !IV_DATA_FIELD type FIELDNAME
    changing
      !CV_STRING type STRING
      !CS_RESULT type ANY
      !CT_RESULT type ANY TABLE .
ENDCLASS.



CLASS ZGOOG_CL_DATA_CLUSTER IMPLEMENTATION.


  METHOD ABAP_TO_JSON.

    serialize(
     EXPORTING
        iv_data    = iv_data
        iv_format  = if_sxml=>co_xt_json
     IMPORTING
        ev_xstring = ev_json_xstring ).

    IF iv_pretty IS NOT INITIAL.

      DATA: lo_reader TYPE REF TO cl_sxml_string_reader.

      lo_reader ?= cl_sxml_string_reader=>create( ev_json_xstring ).

      DATA: lo_writer TYPE REF TO cl_sxml_string_writer.

      lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

      lo_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks ).
      lo_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_indent ).

      lo_reader->if_sxml_reader~next_node( ).
      lo_reader->if_sxml_reader~skip_node( lo_writer ).

      ev_json_xstring = lo_writer->get_output( ).

    ENDIF.

    ev_json_string = cl_abap_codepage=>convert_from( ev_json_xstring ).

  ENDMETHOD.


  METHOD BUILD_EXPORT_KEY_ITAB.

    DATA: lr_insert_row TYPE REF TO data.

    CREATE DATA lr_insert_row LIKE is_export_wa.

    FIELD-SYMBOLS: <ls_insert_row> TYPE any.

    ASSIGN lr_insert_row->* TO <ls_insert_row>.

    <ls_insert_row> = is_export_wa.

    FIELD-SYMBOLS:
      <lv_clustr> TYPE any,
      <lv_clustd> TYPE any,
      <lv_srtf2>  TYPE any.

    ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <ls_insert_row>
           TO <lv_clustr>.

    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <ls_insert_row>
           TO <lv_clustd>.

    ASSIGN COMPONENT 'SRTF2' OF STRUCTURE <ls_insert_row>
           TO <lv_srtf2>.

    DATA: lo_clustd_def TYPE REF TO cl_abap_elemdescr.

    lo_clustd_def ?= cl_abap_typedescr=>describe_by_data( <lv_clustd> ).

    DATA: lv_data_buffer TYPE xstring.

    serialize(
     EXPORTING
       iv_data       = iv_data
       iv_format     = iv_format
     IMPORTING
       ev_xstring    = lv_data_buffer ).

    DATA: lv_data_string TYPE string.

    lv_data_string = lv_data_buffer.

    DATA: lv_string_len TYPE i.

    lv_string_len = strlen( lv_data_string ).

    DATA: lv_offset TYPE i.

    WHILE lv_string_len > 0.
      DATA: lv_part_len TYPE i.

      IF lv_string_len > lo_clustd_def->length.
        lv_part_len = lo_clustd_def->length.
      ELSE.
        lv_part_len = lv_string_len.
      ENDIF.

      lv_string_len = lv_string_len - lo_clustd_def->length.

      <lv_clustd> = lv_data_string+lv_offset(lv_part_len).
      <lv_clustr> = lv_part_len.
      <lv_srtf2> = sy-index.

      lv_offset = lv_offset + lv_part_len.

      APPEND <ls_insert_row> TO ct_export.
    ENDWHILE.

    DATA: lr_key_wa  TYPE REF TO data.
    FIELD-SYMBOLS: <ls_select_key> TYPE any.

    CREATE DATA lr_key_wa LIKE LINE OF ct_key.
    ASSIGN lr_key_wa->* TO <ls_select_key>.
    MOVE-CORRESPONDING is_export_wa TO <ls_select_key>.
    APPEND <ls_select_key> TO ct_key.

  ENDMETHOD.


  METHOD COLLECT_RESULT.

    DATA: lv_data_xstring TYPE xstring.

    lv_data_xstring = cv_string.

    FIELD-SYMBOLS: <lv_cluster_data> TYPE any.

    ASSIGN COMPONENT iv_data_field OF STRUCTURE cs_result
           TO <lv_cluster_data>.

    DATA: lo_reader  TYPE REF TO cl_sxml_string_reader.

    lo_reader ?= cl_sxml_string_reader=>create( lv_data_xstring ).

    CALL TRANSFORMATION id
      SOURCE XML lo_reader
      RESULT data = <lv_cluster_data>.

    INSERT cs_result INTO TABLE ct_result.

    CLEAR: cv_string.

  ENDMETHOD.


METHOD CONSTRUCT_KEY_TABLE.

  DATA: lo_struc_def TYPE REF TO cl_abap_structdescr.

  lo_struc_def ?= cl_abap_typedescr=>describe_by_name( iv_table ).

  DATA: lt_key_components TYPE abap_component_tab,
        lr_component TYPE REF TO abap_compdescr.

  LOOP AT lo_struc_def->components REFERENCE INTO lr_component.

    IF iv_incl_srtf2 IS INITIAL.
      IF lr_component->name = 'SRTF2'.
        EXIT.
      ENDIF.
    ENDIF.

    DATA: ls_comp_descr TYPE abap_componentdescr.
    ls_comp_descr-name = lr_component->name.
    ls_comp_descr-type = lo_struc_def->get_component_type( lr_component->name ).
    APPEND ls_comp_descr TO lt_key_components.

    IF lr_component->name = 'SRTF2'.
      EXIT.
    ENDIF.
  ENDLOOP.

  DATA: lo_tab_descr TYPE REF TO cl_abap_tabledescr.

  lo_tab_descr = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_key_components ) ).
  CREATE DATA er_tabdescr TYPE HANDLE lo_tab_descr.

ENDMETHOD.


  METHOD DELETE_DATA.

    DATA: lr_key_sel TYPE REF TO data.
    CREATE DATA lr_key_sel LIKE STANDARD TABLE OF is_key.
    FIELD-SYMBOLS: <lt_key_sel> TYPE STANDARD TABLE.
    ASSIGN lr_key_sel->* TO <lt_key_sel>.

    APPEND is_key TO <lt_key_sel>.

    delete_data_multile(
     EXPORTING
       iv_table    = iv_table
       it_key      = <lt_key_sel>
     IMPORTING
       ev_db_count = ev_db_count ).

  ENDMETHOD.


METHOD DELETE_DATA_MULTILE.

  DATA: lr_delete_key TYPE REF TO data.
  FIELD-SYMBOLS: <lt_delete_key> TYPE STANDARD TABLE.

  lr_delete_key =
   construct_key_table(
      iv_table      = iv_table
      iv_incl_srtf2 = abap_true ).

  ASSIGN lr_delete_key->* TO <lt_delete_key>.

  select_records(
   EXPORTING
     iv_table  = iv_table
     it_key    = it_key
   IMPORTING
     et_result = <lt_delete_key> ).

  IF <lt_delete_key> IS NOT INITIAL.
    DELETE (iv_table) FROM TABLE <lt_delete_key>.
    ev_db_count = sy-dbcnt.
  ENDIF.

ENDMETHOD.


  METHOD DESERIALIZE.

    DATA: lo_reader TYPE REF TO cl_sxml_string_reader.

    lo_reader ?= cl_sxml_string_reader=>create( iv_xstring ).

    CALL TRANSFORMATION id SOURCE XML lo_reader
       RESULT data = ev_data.

  ENDMETHOD.


  METHOD EXPORT_DATA.

    FIELD-SYMBOLS:
      <lt_insert> TYPE STANDARD TABLE.

    DATA: lr_insert_row TYPE REF TO data.
    DATA: lr_insert_table TYPE REF TO data.
    FIELD-SYMBOLS:
       <ls_insert_row> TYPE any.

    CREATE DATA lr_insert_row TYPE (iv_table).
    ASSIGN lr_insert_row->* TO <ls_insert_row>.
    CREATE DATA lr_insert_table TYPE STANDARD TABLE OF (iv_table).
    ASSIGN lr_insert_table->* TO <lt_insert>.

    DATA: lv_offset TYPE i VALUE 2.

    IF
    set_fval(
       ir_data    = lr_insert_row
       iv_field  = 'MANDT'
       iv_val    = sy-mandt ) > 0.
      lv_offset = 5.
    ENDIF.

    set_fval(
       ir_data    = lr_insert_row
       iv_field  = 'RELID'
       iv_val    = iv_area ).

    DATA: lv_id_length TYPE i.

    lv_id_length = strlen( iv_id ).

    <ls_insert_row>+lv_offset(lv_id_length) = iv_id.

    IF is_addtl_data IS SUPPLIED.
      MOVE-CORRESPONDING is_addtl_data TO <ls_insert_row>.
    ENDIF.

    DATA: lr_key_tab TYPE REF TO data.
    FIELD-SYMBOLS: <lt_key_sel> TYPE ANY TABLE.

    lr_key_tab = construct_key_table( iv_table ).
    ASSIGN lr_key_tab->* TO <lt_key_sel>.

    build_export_key_itab(
     EXPORTING
       is_export_wa     = <ls_insert_row>
       iv_data          = iv_data
       iv_format        = iv_format
     CHANGING
       ct_export       = <lt_insert>
       ct_key          = <lt_key_sel> ).

    delete_data_multile(
        iv_table    = iv_table
        it_key      = <lt_key_sel> ).

    IF <lt_insert> IS NOT INITIAL.
      INSERT (iv_table) FROM TABLE <lt_insert>.
      ev_db_count = sy-dbcnt.
    ENDIF.

  ENDMETHOD.


  METHOD EXPORT_DATA_MULTIPLE.

    FIELD-SYMBOLS:
      <lt_source_data> TYPE ANY TABLE,
      <lt_insert>      TYPE STANDARD TABLE.

    ASSIGN it_data TO <lt_source_data>.

    DATA: lr_insert_row TYPE REF TO data.
    DATA: lr_insert_table TYPE REF TO data.
    FIELD-SYMBOLS:
       <ls_insert_row> TYPE any.

    CREATE DATA lr_insert_row TYPE (iv_table).
    ASSIGN lr_insert_row->* TO <ls_insert_row>.
    CREATE DATA lr_insert_table TYPE STANDARD TABLE OF (iv_table).
    ASSIGN lr_insert_table->* TO <lt_insert>.


    DATA: lr_key_tab TYPE REF TO data.

    FIELD-SYMBOLS: <ls_source_wa>  TYPE any,
                   <lt_select_key> TYPE STANDARD TABLE.

    lr_key_tab = construct_key_table( iv_table ).

    ASSIGN lr_key_tab->* TO <lt_select_key>.

    LOOP AT <lt_source_data> ASSIGNING <ls_source_wa>.
      CLEAR: <ls_insert_row>.

      MOVE-CORRESPONDING <ls_source_wa> TO
        <ls_insert_row>.

      FIELD-SYMBOLS: <lv_data_value> TYPE any.

      ASSIGN COMPONENT iv_data_field OF STRUCTURE <ls_source_wa>
             TO <lv_data_value>.

      build_export_key_itab(
       EXPORTING
         is_export_wa     = <ls_insert_row>
         iv_data          = <lv_data_value>
         iv_format        = iv_format
       CHANGING
         ct_export       = <lt_insert>
         ct_key          = <lt_select_key> ).

    ENDLOOP.

    delete_data_multile(
       iv_table    = iv_table
       it_key      = <lt_select_key> ).

    IF <lt_insert> IS NOT INITIAL.
      INSERT (iv_table) FROM TABLE <lt_insert>.
      ev_db_count = sy-dbcnt.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CLUSTD_FROM_WA.

    FIELD-SYMBOLS:
      <lv_clustr> type indx_clstr,
      <lv_clustd> type any.

    ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE is_db_record
       TO <lv_clustr> CASTING.

    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE is_db_record
           TO <lv_clustd>.

    DATA: lv_blob_string type string.
    lv_blob_string = <lv_clustd>.

    cv_data_string = cv_data_string && lv_blob_string(<lv_clustr>).

  ENDMETHOD.


  METHOD IMPORT_DATA.

    DATA:
      li_db_data TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_db_data> TYPE STANDARD TABLE.

    CREATE DATA li_db_data TYPE STANDARD TABLE OF (iv_table) WITH DEFAULT KEY.
    ASSIGN li_db_data->* TO <lt_db_data>.


    DATA: lr_key_tab TYPE REF TO data,
          lr_key_template TYPE REF TO data.

    FIELD-SYMBOLS: <lt_key_tab> TYPE STANDARD TABLE,
                   <ls_key_template> TYPE clike.

    lr_key_tab = construct_key_table( iv_table ).
    ASSIGN lr_key_tab->* TO <lt_key_tab>.
    APPEND INITIAL LINE TO <lt_key_tab> ASSIGNING <ls_key_template>.
    GET REFERENCE OF <ls_key_template> INTO lr_key_template.

    DATA: lv_offset TYPE i VALUE 2.

    IF
    set_fval(
       ir_data    = lr_key_template
       iv_field  = 'MANDT'
       iv_val    = sy-mandt ) > 0.
      lv_offset = 5.
    ENDIF.

    set_fval(
       ir_data   = lr_key_template
       iv_field  = 'RELID'
       iv_val    = iv_area ).

    DATA: lv_id_length TYPE i.

    lv_id_length = strlen( iv_id ).

    <ls_key_template>+lv_offset(lv_id_length) = iv_id.

    select_records(
      EXPORTING
        iv_table  = iv_table
        it_key    = <lt_key_tab>
      IMPORTING
        et_result = <lt_db_data> ).

    DATA: lv_data_string TYPE string.

    FIELD-SYMBOLS: <ls_db_row> TYPE any.

    LOOP AT <lt_db_data> ASSIGNING <ls_db_row>.

      get_clustd_from_wa(
       EXPORTING
         is_db_record   = <ls_db_row>
       CHANGING
         cv_data_string = lv_data_string ).

    ENDLOOP.

    IF lv_data_string IS NOT INITIAL.

      DATA: lv_data_xstring TYPE xstring.

      lv_data_xstring = lv_data_string.

      DATA: lo_reader TYPE REF TO cl_sxml_string_reader.

      lo_reader ?= cl_sxml_string_reader=>create( lv_data_xstring ).

      CALL TRANSFORMATION id SOURCE XML lo_reader
         RESULT data = ev_data.

      IF es_addtl_data IS SUPPLIED.
        MOVE-CORRESPONDING <ls_db_row> TO es_addtl_data.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD IMPORT_DATA_MULTIPLE.

    DATA:
          lr_db_data       TYPE REF TO data.

    FIELD-SYMBOLS:
          <lt_db_data> TYPE STANDARD TABLE.

    CREATE DATA lr_db_data TYPE STANDARD TABLE OF (iv_table) WITH DEFAULT KEY.
    ASSIGN lr_db_data->* TO <lt_db_data>.

    select_records(
     EXPORTING
       iv_table  = iv_table
       it_key    = it_key
     IMPORTING
       et_result = <lt_db_data> ).

    DATA: lr_result_data TYPE REF TO data.

    CREATE DATA lr_result_data LIKE LINE OF et_result.
    FIELD-SYMBOLS: <ls_result_wa> TYPE any.

    ASSIGN lr_result_data->* TO <ls_result_wa>.

    DATA:
      lr_current_key  TYPE REF TO data,
      lr_previous_key TYPE REF TO data.

    CREATE DATA lr_current_key LIKE LINE OF it_key.
    CREATE DATA lr_previous_key LIKE LINE OF it_key.

    FIELD-SYMBOLS: <ls_current_key> TYPE any.
    FIELD-SYMBOLS: <ls_previous_key> TYPE any.

    ASSIGN lr_current_key->* TO <ls_current_key>.
    ASSIGN lr_previous_key->* TO <ls_previous_key>.

    DATA: lv_data_string TYPE string.

    FIELD-SYMBOLS: <ls_db_data_wa> TYPE any.

    LOOP AT <lt_db_data>
         ASSIGNING <ls_db_data_wa>.

      DATA: lv_insert_flag TYPE flag.

      CLEAR: <ls_current_key>.
      MOVE-CORRESPONDING <ls_db_data_wa> TO <ls_current_key>.

      IF <ls_previous_key> IS NOT INITIAL.
        IF <ls_previous_key> <> <ls_current_key>.

          lv_insert_flag = abap_true.
        ENDIF.
      ENDIF.

      IF lv_insert_flag IS NOT INITIAL.
        collect_result(
          EXPORTING
            iv_data_field = iv_data_field
          CHANGING
            cv_string     = lv_data_string
            cs_result     = <ls_result_wa>
            ct_result     = et_result ).
      ENDIF.

      CLEAR: <ls_result_wa>.
      MOVE-CORRESPONDING <ls_db_data_wa> TO <ls_result_wa>.

      get_clustd_from_wa(
       EXPORTING
         is_db_record   = <ls_db_data_wa>
       CHANGING
         cv_data_string = lv_data_string ).

      <ls_previous_key> = <ls_current_key>.

    ENDLOOP.

    IF lv_data_string IS NOT INITIAL.
      collect_result(
        EXPORTING
          iv_data_field = iv_data_field
        CHANGING
          cv_string     = lv_data_string
          cs_result     = <ls_result_wa>
          ct_result     = et_result ).
    ENDIF.


  ENDMETHOD.


  METHOD JSON_TO_ABAP.

    DATA: lv_xstring TYPE xstring.

    IF iv_json_string IS NOT INITIAL.
      lv_xstring =
        cl_abap_codepage=>convert_to( iv_json_string ).
    ELSE.
      lv_xstring = iv_json_xstring.
    ENDIF.

    deserialize(
      EXPORTING
        iv_xstring = lv_xstring
      IMPORTING
        ev_data    = ev_data ).

  ENDMETHOD.


METHOD SELECT_RECORDS.

  DATA: lo_struc_def TYPE REF TO cl_abap_structdescr.

  lo_struc_def ?=
       cl_abap_typedescr=>describe_by_name( iv_table ).

  DATA: lt_where_clause TYPE tt_string.
  DATA: lv_and_clause TYPE string.

  DATA: lr_component TYPE REF TO abap_compdescr.

  LOOP AT lo_struc_def->components
       REFERENCE INTO lr_component.

    IF lr_component->name = 'MANDT'.
      CONTINUE.
    ENDIF.

    IF lr_component->name = 'SRTF2'.
      EXIT.
    ENDIF.

    DATA: lv_where TYPE string.

    lv_where =
      |{ lv_and_clause }| &
      |{ lr_component->name }| &
      | EQ | &
      | IT_KEY-{ lr_component->name } |.

    APPEND lv_where TO lt_where_clause.

    lv_and_clause = |AND |.
  ENDLOOP.

  IF it_key IS NOT INITIAL.
    SELECT *
      FROM (iv_table)
      INTO CORRESPONDING FIELDS OF TABLE et_result
      FOR ALL ENTRIES IN it_key
      WHERE (lt_where_clause).
  ENDIF.

ENDMETHOD.


  METHOD SERIALIZE.

    eo_xml_writer =
      cl_sxml_string_writer=>create( type = iv_format ).

    CALL TRANSFORMATION id
    SOURCE data = iv_data
          RESULT XML eo_xml_writer.

    ev_xstring = eo_xml_writer->get_output( ).

  ENDMETHOD.


  METHOD SET_FVAL.

    rv_result = 0.

    FIELD-SYMBOLS: <ls_data> type any.

    ASSIGN ir_data->* TO <ls_data>.

    IF <ls_data> IS ASSIGNED.
      FIELD-SYMBOLS: <lv_field> type any.

      ASSIGN COMPONENT iv_field OF STRUCTURE <ls_data> TO <lv_field>.
    ENDIF.

    IF <lv_field> IS ASSIGNED.
      <lv_field> = iv_val.
      rv_result = 1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
