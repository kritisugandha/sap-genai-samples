FUNCTION z_tool_get_geocode.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_FUNCTION_PARAMETERS) TYPE
*"        /GOOG/T_FUNCTION_PARAMETERS
*"  EXPORTING
*"     REFERENCE(EV_FUNCTION_RESPONSE) TYPE  STRING
*"  CHANGING
*"     REFERENCE(CV_PROMPT) TYPE  STRING
*"----------------------------------------------------------------------
  DATA(ls_params) = VALUE #( it_function_parameters[ parameter_name = 'LOCATION' ] OPTIONAL ).

  " TODO: Get the actual geocode using google maps.

  ev_function_response = |'The geocodes for { ls_params-parameter_value } are latitude 12.9715987 and longitude 77.5945627'|.
ENDFUNCTION.
