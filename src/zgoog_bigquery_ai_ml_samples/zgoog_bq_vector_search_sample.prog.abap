*&---------------------------------------------------------------------*
*& Report ZGOOG_BQ_GEMINI_SAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_bq_vector_search_sample.

PARAMETERS:
  p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
  p_loc_id TYPE string OBLIGATORY DEFAULT 'us-central1' LOWER CASE,
  p_query  TYPE string OBLIGATORY LOWER CASE,
  p_sh_str TYPE string OBLIGATORY LOWER CASE,
  p_neigh  TYPE i.

DATA:
  ls_query_parameter  TYPE /goog/cl_bq_query=>ty_query_parameter,
  lt_query_parameters TYPE /goog/cl_bq_query=>ty_t_query_parameters.

ls_query_parameter-parameter_name  = 'SEARCH_STRING'.
ls_query_parameter-parameter_type  = 'STRING'.
ls_query_parameter-parameter_value = 'Pullovers'.

APPEND ls_query_parameter TO lt_query_parameters.
CLEAR ls_query_parameter.

TRY.
    DATA(lo_bq_query) = NEW /goog/cl_bq_query( iv_key         = p_key
                                               iv_location_id = p_loc_id
                                               iv_query_name  = p_query ).
    lo_bq_query->set_query_parameters( lt_query_parameters ).

    DATA(lo_bq_vector_search) = NEW /goog/cl_bq_vector_search( iv_key = p_key ).
    DATA(lt_search_response) = lo_bq_vector_search->set_search_parameters( iv_top_k = p_neigh
                                                 )->find_nearest_neighbors( lo_bq_query
                                                 )->get_nearest_neighbors( ).
    IF lt_search_response IS NOT INITIAL.
      cl_demo_output=>display( lt_search_response ).

    ENDIF.
  CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
    cl_demo_output=>display( lo_cx_sdk->get_text( ) ).

ENDTRY.
