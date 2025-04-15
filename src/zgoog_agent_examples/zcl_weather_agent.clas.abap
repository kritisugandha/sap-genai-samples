class ZCL_WEATHER_AGENT definition
  public
  inheriting from ZCL_GEMINI_AGENT_BASE
  create public .

public section.

  methods GET_SYSTEM_INSTRUCTION
    redefinition .
  methods GET_TOOL_DEFINITIONS
    redefinition .
  methods GET_MODEL_ID
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WEATHER_AGENT IMPLEMENTATION.


  METHOD get_model_id.

    r_result = model_keys-gemini_flash. " Or a specific model

  ENDMETHOD.


  METHOD get_system_instruction.

    r_result =
    |You are a helpful weather and environmental | &&
    |information assistant bot. Your capabilities | &&
    |include:{ cl_abap_char_utilities=>newline }| &&
    |1.  Weather Information:{  cl_abap_char_utilities=>newline }| &&
    |    * Identify the location mention in the prompt and use the tool to get geocode | &&
    |    * Retrieve and provide current weather  | &&
    |      information, including air quality.{ cl_abap_char_utilities=>newline }| &&
    |2.  Natural Disaster Alerts:{ cl_abap_char_utilities=>newline }| &&
    |    * Alert the user about significant     | &&
    |      earthquakes.{ cl_abap_char_utilities=>newline }| &&
    |    * Alert the user about significant     | &&
    |      storms.{ cl_abap_char_utilities=>newline }| &&
    |3.  Air Quality Information:{ cl_abap_char_utilities=>newline }| &&
    |    * Provide air quality information for a | &&
    |      specified location.{ cl_abap_char_utilities=>newline }| &&
    cl_abap_char_utilities=>newline &&
    |Please use the tools to get any missing information. | &&
    |You are strictly limited to these tasks. Do | &&
    | not perform any other actions or provide    | &&
    |information outside of weather, earthquakes, | &&
    |storms, and air quality.|.

  ENDMETHOD.


  METHOD get_tool_definitions.

    r_result = VALUE #(
      ( name           = 'Z_TOOL_GET_CURRENT_LOCATION'      " Must match FM Name
        description    = 'Get the current geographical location of the user'
        implementation = 'Z_TOOL_GET_CURRENT_LOCATION' " For reference
        parameters     = VALUE tt_tool_parameters( ) ) " No input params defined here
      ( name           = 'Z_TOOL_GET_GEOCODE'      " Must match FM Name
        description    = 'Get geocodes in the form of latitude and longitude coordinates for a specified location'
        implementation = 'Z_TOOL_GET_GEOCODE' " For reference
        parameters     = VALUE tt_tool_parameters(
                                   type        = 'string'
                                   is_required = abap_true
                                   ( name = 'LOCATION'  description = 'Location details to identify Geocode' ) ) )
      ( name           = 'Z_TOOL_GET_AIR_QUALITY' " Must match FM Name
        description    = 'Gets air quality information for a given location specified by latitude and longitude'
        implementation = 'Z_TOOL_GET_AIR_QUALITY' " For reference
        parameters     = VALUE tt_tool_parameters(
                                   type        = 'string'
                                   is_required = abap_true
                                   ( name = 'LOCATION'  description = 'Location details' )
                                   ( name = 'LATITUDE'  description = 'Latitude of the location' )
                                   ( name = 'LONGITUDE' description = 'Longitude of the location' ) ) )
        ).

  ENDMETHOD.
ENDCLASS.
