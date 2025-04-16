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
REPORT zdemo_use_weather_agent.

" -----------------------------------------------------------------------
" Selection Screen
" -----------------------------------------------------------------------
PARAMETERS p_prompt TYPE string DEFAULT 'What is the air quality in my current location?' LOWER CASE.

" -----------------------------------------------------------------------
" Main Processing Block
" -----------------------------------------------------------------------

START-OF-SELECTION.

  DATA(lo_out) = cl_demo_output=>new( ).

  TRY.
      " 1. Instantiate the specific Agent class
      "    The constructor of ZCL_GENAI_WEATHER_AGENT calls super->constructor(),
      "    which in turn calls the redefined GET_... methods to configure
      "    the agent within the base class logic.
      DATA(lo_weather_agent) = NEW zcl_weather_agent( ).
      lo_weather_agent->initialize_agent( ).

      " 2. Process the user's prompt
      "    The process_prompt method is inherited from the base class
      "    and uses the fully configured mo_model instance.
      DATA(lv_response) = lo_weather_agent->process_prompt( iv_prompt = p_prompt ).

      " 3. Display the result
      lo_out->begin_section( 'User Prompt:' ).
      lo_out->write_text( p_prompt ).
      lo_out->begin_section( 'Agent Response:' ).
      lo_out->write_text( lv_response  ).
      lo_out->display( ).

    CATCH /goog/cx_sdk INTO DATA(lx_sdk).
      " Handle potential SDK errors (authentication, API issues, etc.)
      lo_out->begin_section( |!!! Error occurred !!!| ).
      lo_out->write_text( lx_sdk->get_text( ) ).
      lo_out->display( ).

  ENDTRY.

  " 4. IMPORTANT: Clean up the agent resources
  "    Always ensure the close method is called to release the
  "    SDK connection and resources, even if errors occurred.
  IF lo_weather_agent IS BOUND.
    lo_weather_agent->close( ).
  ENDIF.
