FUNCTION ZFM_HAZMAT_GET_PRODUCT_DATA.
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
* This can answer below questions:
*DATA(lv_prompt) = 'How much of Acetone do we have in stock?'.
*DATA(lv_prompt) = 'What is the expiration date for Acetone?'.
*DATA(lv_prompt) = 'Where is Acetone stored in the warehouse?'.


  READ TABLE it_function_parameters ASSIGNING FIELD-SYMBOL(<ls_param>) WITH KEY parameter_name = 'PRODUCT'.
  IF sy-subrc = 0.
    DATA(lv_matnr) = CONV matnr( <ls_param>-parameter_value ).

    SELECT * FROM zhazmat_products INTO TABLE @DATA(lt_product_info) WHERE matnr = @lv_matnr.
    IF sy-subrc = 0.
      ev_function_response = /goog/cl_json=>serialize( EXPORTING data = lt_product_info ).
    ENDIF.

    DATA(lv_format) = 'Based on the question, choose the most relevant format to answer the question. Allowed formats:' &&
                      '\n"<Name of Product>(<ID of the product>) is stored in Plant <Plant ID>, Storage Location <Storage location> in the warehouse."' &&
                      '\n"We have <quantity> units of <Name of Product>(<ID of the product>) in stock."' &&
                      '\n"The expiration date for <name of Product>(<ID of the product>) is <date of expiration>"'.

    cv_prompt = cv_prompt &&
                'Given below is the product information for this product per storage location in JSON Format. ' &&
                ev_function_response &&
                '\nThe field mapping for the JSON is provided below: ' &&
                'MANDT: SAP Client key [Ignore This field]' &&
                'MATNR: Product Id' &&
                'WERKS: Plant ID' &&
                'LGORT: Storage Location ID' &&
                'LGOBE: Storage Location Description' &&
                'ERFMG: Stock Quantity ' &&
                'ERFME: Stock Quantity Unit' &&
                'EXPDT: Date of expiratation' &&
                lv_format.
  ELSE.

  ENDIF.

ENDFUNCTION.
