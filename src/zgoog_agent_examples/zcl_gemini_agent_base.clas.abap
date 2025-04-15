class ZCL_GEMINI_AGENT_BASE definition
  public
  abstract
  create public .

public section.

  types:
      "! Structure for defining agent tools (ABAP Functions/Methods)
    BEGIN OF ty_tool_parameter,
        name        TYPE string,
        type        TYPE string, " e.g., 'string', 'number', 'integer', 'boolean', 'array', 'object'
        description TYPE string,
        is_required TYPE abap_boolean,
      END OF ty_tool_parameter .
  types:
    tt_tool_parameters TYPE STANDARD TABLE OF ty_tool_parameter WITH EMPTY KEY .
  types:
    BEGIN OF ty_tool_definition,
        name           TYPE string,             " Name of the Function Module or Method (as registered)
        description    TYPE string,
        parameters     TYPE tt_tool_parameters,
        implementation TYPE string,             " Function Module name or Class-Method name (for reference)
      END OF ty_tool_definition .
  types:
    tt_tool_definitions TYPE STANDARD TABLE OF ty_tool_definition WITH EMPTY KEY .

  constants:
      "! <p>Agent specific constants</p>
    BEGIN OF model_keys,
        gemini_flash TYPE /goog/model_key VALUE 'gemini-flash-2', " Or other desired model key
        " gemini_pro   TYPE /GOOG/MODEL_KEY VALUE 'gemini-1.5-pro-latest',
      END OF model_keys .

  methods PROCESS_PROMPT
    importing
      !IV_PROMPT type STRING
    returning
      value(R_RESULT) type STRING
    raising
      /GOOG/CX_SDK .
  methods CLOSE .
    "! <p>ABSTRACT: Subclass must provide the specific system instructions.</p>
    "! @returning result | The system instruction string |
  methods GET_SYSTEM_INSTRUCTION
  abstract
    returning
      value(R_RESULT) type STRING .
    "! <p>ABSTRACT: Subclass must provide the specific tool definitions.</p>
    "! @returning result | Table of tool definitions |
  methods GET_TOOL_DEFINITIONS
  abstract
    returning
      value(R_RESULT) type TT_TOOL_DEFINITIONS .
    "! <p>ABSTRACT: Subclass must provide the model ID to use.</p>
    "! @returning result | The Google Cloud Model ID string |
  methods GET_MODEL_ID
  abstract
    returning
      value(R_RESULT) type /GOOG/MODEL_KEY .
  methods INITIALIZE_AGENT .
protected section.

  data MO_MODEL type ref to /GOOG/CL_GENERATIVE_MODEL .
  data MV_LAST_RESPONSE type STRING .
  data MT_TOOLS_REGISTERED type TT_TOOL_DEFINITIONS .
  data MV_IS_INITIALIZED type ABAP_BOOLEAN value ABAP_FALSE ##NO_TEXT.
private section.

    "! Helper method to register tools with the SDK model
    "!
    "! @parameter it_tool_definitions | Tool definitions
    "! @raising /goog/cx_sdk | Google Cloud SDK Exception
  methods REGISTER_TOOLS
    importing
      !IT_TOOL_DEFINITIONS type TT_TOOL_DEFINITIONS
    raising
      /GOOG/CX_SDK .
ENDCLASS.



CLASS ZCL_GEMINI_AGENT_BASE IMPLEMENTATION.


  METHOD close.
    " Ensure the model object exists and close the connection
    IF me->mo_model IS BOUND.
      me->mo_model->close( ).
      CLEAR me->mo_model. " Release the reference
    ENDIF.
  ENDMETHOD.


  METHOD initialize_agent.
    IF mv_is_initialized = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_model_id) = get_model_id( ). " Call abstract method
    DATA(lv_system_instruction) = get_system_instruction( ). " Call abstract method
    DATA(lt_tools) = get_tool_definitions( ). " Call abstract method

    TRY.
        " 1. Create the Model Instance
        CREATE OBJECT me->mo_model
          EXPORTING
            iv_model_key = lv_model_id.

        " 2. Set System Instructions
        me->mo_model->set_system_instructions( lv_system_instruction ).

        " 3. Register Tools (Function Declarations) if any
        IF lt_tools IS NOT INITIAL.
          register_tools( lt_tools ).
          me->mo_model->set_auto_invoke_sap_function( abap_true ).
        ENDIF.

        mv_is_initialized = abap_true.

      CATCH /goog/cx_sdk INTO DATA(lx_sdk).
        " Log exception or handle appropriately
        RAISE EXCEPTION lx_sdk.
    ENDTRY.
  ENDMETHOD.


  METHOD process_prompt.
    CLEAR me->mv_last_response.
    TRY.
        " Generate content using the prompt
        " The SDK handles the conversation history and potential tool calls internally
        DATA(lo_response) = me->mo_model->generate_content( iv_prompt_text = iv_prompt ).

        " Get the final text response
        r_result = lo_response->get_text( ).
        me->mv_last_response = r_result.

      CATCH /goog/cx_sdk INTO DATA(lx_sdk).
        " Log exception or handle appropriately
        RAISE EXCEPTION lx_sdk.
    ENDTRY.
  ENDMETHOD.


  METHOD register_tools.
    LOOP AT it_tool_definitions ASSIGNING FIELD-SYMBOL(<fs_tool>).
      DATA(lt_sdk_params) = VALUE /goog/cl_generative_model=>tt_parameter_properties( ).

      " Convert tool parameters to SDK format
      LOOP AT <fs_tool>-parameters ASSIGNING FIELD-SYMBOL(<fs_param>).
        APPEND VALUE #( parameter_name = <fs_param>-name
                        type           = <fs_param>-type
                        description    = <fs_param>-description
                        is_required    = <fs_param>-is_required )
               TO lt_sdk_params.
      ENDLOOP.

      " Add the function declaration to the SDK model
      me->mo_model->add_function_declaration( iv_name        = <fs_tool>-name        " Must match the FM name if using auto-invoke
                                              iv_description = <fs_tool>-description
                                              it_parameters  = lt_sdk_params ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
