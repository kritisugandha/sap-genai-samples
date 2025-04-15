FUNCTION z_tool_get_air_quality.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_FUNCTION_PARAMETERS) TYPE
*"                             /GOOG/T_FUNCTION_PARAMETERS
*"  EXPORTING
*"     REFERENCE(EV_FUNCTION_RESPONSE) TYPE  STRING
*"  CHANGING
*"     REFERENCE(CV_PROMPT) TYPE  STRING
*"--------------------------------------------------------------------

  DATA(lv_location) = VALUE #( it_function_parameters[ parameter_name = 'LOCATION' ]-parameter_value OPTIONAL ).

  ev_function_response = |Current AirQuality in { lv_location } is 34.7 |.

ENDFUNCTION.
