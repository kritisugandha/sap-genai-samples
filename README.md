# ![Google Cloud](https://avatars.githubusercontent.com/u/2810941?s=60&v=4) Google Cloud Generative AI Samples for SAP

[Vertex AI](https://cloud.google.com/vertex-ai) is a fully-managed, unified AI development platform for building and using generative AI. For developing Generative applications in SAP system, Google Cloud offers [Vertex AI SDK for ABAP](https://cloud.google.com/solutions/sap/docs/abap-sdk/vertex-ai-sdk/latest/overview) that can be installed in any SAP ERP system and BTP ABAP Environment. 

This repository contains code samples, sample apps, and other resources that demonstrate how to use, develop and manage generative AI applications in your SAP environment by using Vertex AI SDK for ABAP.

## Prerequisite
In order to use these sample applications in your systems, you first need to [Install and Configure](https://cloud.google.com/solutions/sap/docs/abap-sdk/vertex-ai-sdk/latest/install-configure-vertex-ai-sdk-abap) Vertex AI SDK for ABAP. Please note the Model Key from the [configure model parameters](https://cloud.google.com/solutions/sap/docs/abap-sdk/vertex-ai-sdk/latest/install-configure-vertex-ai-sdk-abap#configure-model-generation-parameters) step - which you will use as an input in the below samples


## NL 2 SQL
You can use the program ZGOOG_R_DEMO_NL2SQL to dynamically generate SQL statement for natural language queries. The AI model will use the CDS view definition to map users natural language query to corresponding where clauses in SQL statement. Below is a sample output for query on Products table 

![alt_text](images/NL2SQL.png "NL2SQL Screen")

By using the approach demonstrated in this sample, you can build your own AI powered Fiori chatbot for your SAP business application. 

## Structured Output
You can use the program ZGOOG_R_DEMO_STRUCT_OUTPUT to generate a generate structured output conforming to a JSON schema for unstructured inputs such as text and PDF. You can specify an OData Entity or any structure (flat / deep) to which the JSON output should be mapped or transformed. 

Here's a example JSON output for a [PDF Purchase Order](https://github.com/google-cloud-abap/demo-po-so/blob/main/Sample%20Purchase%20Order%20Documents.zip) by using the JSON schema of type ZGOOG_CL_METADATA_SAMPLE=>TS_DEEP_ENTITY (included in this repo).

![alt_text](images/Structured_Output.png "Structured Output Screen")

You can use the approach demonstrated here to convert various unstructured inputs into structured output. The generated output can be easily converted to ABAP format by using the method /UI2/CL_JSON=>DESERIALIZE

## Gemini powered SAP Conversation Agent
You can use the class ZGOOG_CL_CONVERSATION_AGENT to build GenAI based SAP conversation agents to perform multi turn chat-like conversation powered by Gemini, for a sequence of user prompts. The solution takes into account the history of the conversation for each new prompt and invokes Gemini with the context of the history to respond. The solution tracks the conversation sessions though GUID based session ids, and saves the conversation history against these session ids in table ZGOOG_CONV_HIST shipped with the solution.

Below are the steps for SAP developers to build ABAP based conversation agents with the class.

### Instantiate the class
You can instantiate the class ZGOOG_CL_CONVERSATION_AGENT in two ways using the model key, which you would have configured in table /GOOG/AI_CONFIG of Vertex AI SDK for ABAP.
* Instantiate with only the model key if you would like to start a new conversation session.
```
DATA(lo_conversation_agent) = NEW zgoog_cl_conversation_agent( iv_model_key = '<MODEL_KEY>' ).
```
* Instantiate with the model key along with a session id if you would like to do a follow on conversation for a previous conversation for that session id.
```
DATA(lo_conversation_agent) = NEW zgoog_cl_conversation_agent( iv_model_key  = '<MODEL_KEY>'
                                                               iv_session_id = '<SESSION_ID>' ).
```

### Send a conversation message
You can use the method SEND_MESSAGE_CONTENT to send a conversation message to Gemini.
* The passed message is the first message in the conversation if the class is instantiated without any session id.
* The passed message is a follow-on message to a previous or an ongoing conversation against the session id if the class is instantiated with a session id.
```
DATA(lo_agent_response) = lo_conversation_agent->send_message_content( iv_prompt_text = '<CONVERSATION_MESSAGE>' ).
```

You can also pass raw files and GCS URI of files (documents, images and videos) along with the conversation message in the IT_RAW_FILE_DATA and IT_GCS_FILE_URIS import parameters of the method. Gemini takes into consideration the files and the context is stored in the history against the session id to be brought in during the next conversation turn for the same session.

### Get response for the conversation
You can use the method GET_TEXT_RESPONSE to get the conversation response for any input conversation message. The response is in context with the history of the conversation done against the session with which the class ZGOOG_CL_CONVERSATION_AGENT is instantiated.

```
DATA(lv_response) = lo_agent_response->get_text_response( ).
```

### Get conversation session id
You can use the method GET_CONVERSATION_SESSION_ID to retrieve the session id of the current conversation. You can use it to resume the conversation with the next conversation message by instantiating the class ZGOOG_CL_CONVERSATION_AGENT with it.

```
DATA(lv_session_id) = lo_agent_response->get_conversation_session_id( ).
```

### Get conversation history
You can use the method GET_CONVERSATION_HISTORY to retrieve the conversation history for a session id with which the class ZGOOG_CL_CONVERSATION_AGENT is instantiated.

```
DATA(lt_conversation_history) = lo_agent_response->get_conversation_history( ).
```

### Clear conversation history
You can use the method CLEAR_CONVERSATION_HISTORY to clear or purge the conversation history for a session id from table ZGOOG_CONV_HIST.

```
lo_conversation_agent->clear_conversation_history( '<SESSION_ID>' ).
```

You can also extend the class to create your own class for building conversation agents to put in logic as per your requirements.

## Error and exception handling
​​You can leverage ABAP SDK exception class /GOOG/CX_SDK to catch errors and exceptions raised by the methods of the conversation agent class ZGOOG_CL_CONVERSATION_AGENT.

## A sample invocation
Below is a sample code of a report program to hold conversations based on session ids. 
* If you would like to start a conversation, execute the report program without providing the session id,
* If you would like to do a follow on conversation, execute the report program with the session id of the previous conversation.

```
PARAMETERS:
  p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
  p_sessid TYPE guid LOWER CASE,
  p_prompt TYPE string OBLIGATORY LOWER CASE.

DATA:
      lo_agent_response TYPE REF TO zgoog_cl_conversation_agent.

TRY.
    IF p_sessid IS INITIAL.
      DATA(lo_conversation_agent) = NEW zgoog_cl_conversation_agent( iv_model_key = 'gemini-flash' ).
    ELSE.
      lo_conversation_agent = NEW zgoog_cl_conversation_agent( iv_model_key  = 'gemini-flash'
                                                               iv_session_id = p_sessid ).

    ENDIF.

    lo_agent_response = lo_conversation_agent->send_message_content( iv_prompt_text = p_prompt ).
    DATA(lv_response) = lo_agent_response->get_text_response( ).
    DATA(lv_session_id) = lo_agent_response->get_conversation_session_id( ).

    WRITE:
      / |Gemini Response: | && lv_response,
      / |Session ID: | && lv_session_id.

  CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
    MESSAGE lo_cx_sdk->get_text( ) TYPE 'E'.

ENDTRY.
```


