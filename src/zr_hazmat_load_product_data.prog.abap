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
REPORT zr_hazmat_load_product_data.

PARAMETERS: p_file TYPE string OBLIGATORY LOWER CASE,
            p_save TYPE c AS CHECKBOX.

CLASS lcl_file_uploader DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: f4_file_name CHANGING cv_file_name TYPE string,
      execute IMPORTING iv_file_name TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: mt_products TYPE STANDARD TABLE OF zhazmat_products.
    CLASS-METHODS:
      convert_to_itab IMPORTING iv_file_name TYPE string,
      display_alv,
      save_to_db.

ENDCLASS.

CLASS lcl_file_uploader IMPLEMENTATION .
  METHOD f4_file_name.

    DATA: lt_filetab TYPE filetable,
          lv_rc      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_extension       = 'CSV'
      CHANGING
        file_table              = lt_filetab
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1                " "Open File" dialog failed
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        not_supported_by_gui    = 4                " GUI does not support this
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0 OR lv_rc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      TRY.
          cv_file_name = lt_filetab[ 1 ].
        CATCH cx_sy_itab_line_not_found.
          MESSAGE 'Please select a file'(001) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD execute.
    convert_to_itab( iv_file_name = iv_file_name ).
    save_to_db( ).
    display_alv( ).
  ENDMETHOD.

  METHOD convert_to_itab.
    TYPES : BEGIN OF ty_file,
              text(1200) TYPE c,
            END OF ty_file.
    DATA: lt_file_data TYPE STANDARD TABLE OF ty_file.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_file_name
      CHANGING
        data_tab                = lt_file_data
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Front-End Function Cannot Be Executed in Backgrnd
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No Upload Authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not Enough Memory in DataProvider
        disk_full               = 15               " Storage Medium full
        dp_timeout              = 16               " Timeout of DataProvider
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      LOOP AT lt_file_data ASSIGNING FIELD-SYMBOL(<ls_data>) FROM 2.

        APPEND INITIAL LINE TO mt_products ASSIGNING FIELD-SYMBOL(<ls_product>).
        SPLIT <ls_data> AT ',' INTO TABLE DATA(lt_values).

        LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<lv_value>).
          ASSIGN COMPONENT ( sy-tabix + 1 ) OF STRUCTURE <ls_product> TO FIELD-SYMBOL(<lv_cell>).
          IF sy-subrc = 0 AND <lv_cell> IS ASSIGNED.

            IF sy-tabix = 6. "EXPDT
              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                  date_external            = <lv_value>
                IMPORTING
                  date_internal            = <lv_cell>
                EXCEPTIONS
                  date_external_is_invalid = 1
                  OTHERS                   = 2.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDIF.
            ELSE.
              <lv_cell> = <lv_value>.
            ENDIF.
          ENDIF.
          <ls_product>-mandt = sy-mandt.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD save_to_db.
    IF p_save = abap_true AND mt_products IS NOT INITIAL.
      MODIFY zhazmat_products FROM TABLE mt_products.
      IF sy-subrc = 0.
        DATA(lv_msg) = |{ lines( mt_products ) } Hazardous materials were created and saved. |.
        MESSAGE lv_msg TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD display_alv.
    DATA: lo_column TYPE REF TO cl_salv_column_table.

* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = DATA(lo_gr_alv)
          CHANGING
            t_table      = mt_products.

** Show all default buttons of ALV
        DATA(lo_gr_functions) = lo_gr_alv->get_functions( ).
        lo_gr_functions->set_all( abap_true ).

** Fit the columns
        DATA(lo_columns) = lo_gr_alv->get_columns( ).
        lo_columns->set_optimize( 'X' ).

** Set Column Headings
        lo_columns->get_column( 'MATNR' )->set_short_text( 'Material' ).
        lo_columns->get_column( 'EXPDT' )->set_short_text( 'Expiration' ).

        lo_gr_alv->display( ).
      CATCH cx_root.
        MESSAGE 'Error occurred during ALV display!' TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_file_uploader=>f4_file_name(
    CHANGING
      cv_file_name = p_file
  ).

START-OF-SELECTION.
  lcl_file_uploader=>execute( iv_file_name = p_file ).
