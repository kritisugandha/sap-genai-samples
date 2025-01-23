CLASS zgoog_cl_metadata_sample DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_document,
        documentname    TYPE string,
        mimetype        TYPE string,
        documentcontent TYPE xstring,
      END OF ts_document .
    TYPES:
  tt_document TYPE STANDARD TABLE OF ts_document .
    TYPES:
      BEGIN OF ts_text_element,
        artifact_name        TYPE c LENGTH 40,       " technical name
        artifact_type        TYPE c LENGTH 4,
        parent_artifact_name TYPE c LENGTH 40, " technical name
        parent_artifact_type TYPE c LENGTH 4,
        text_symbol          TYPE textpoolky,
      END OF ts_text_element .
    TYPES:
           tt_text_elements TYPE STANDARD TABLE OF ts_text_element WITH KEY text_symbol .
    TYPES:
      BEGIN OF ts_documentdata,
        documentname                   TYPE string,
        purchaseordernumber            TYPE string,
        purchaseorderdate              TYPE string,
        requesteddeliverydate          TYPE string,
        soldtonumber                   TYPE c LENGTH 10,
        soldtoname                     TYPE string,
        soldtoaddress                  TYPE string,
        shiptoname                     TYPE string,
        shiptonumber                   TYPE c LENGTH 10,
        shiptoaddress                  TYPE string,
        deliveryinstructions           TYPE string,
        deliveryinstructionsoriginal   TYPE string,
        deliveryinstructionsoriginalla TYPE string,
        salesordernumber               TYPE c LENGTH 10,
      END OF ts_documentdata .
    TYPES:
  tt_documentdata TYPE STANDARD TABLE OF ts_documentdata .
    TYPES:
      BEGIN OF ts_purchaseorder,
        documentname TYPE string,
        mimetype     TYPE string,
      END OF ts_purchaseorder .
    TYPES:
  tt_purchaseorder TYPE STANDARD TABLE OF ts_purchaseorder .
    TYPES:
      BEGIN OF ts_documentdataitem,
        documentname           TYPE string,
        itemid                 TYPE /iwbep/sb_odata_ty_int2,
        customermaterialnumber TYPE string,
        quantity               TYPE p LENGTH 7 DECIMALS 3,
      END OF ts_documentdataitem .
    TYPES:
  tt_documentdataitem TYPE STANDARD TABLE OF ts_documentdataitem .

    TYPES:
      BEGIN OF ts_deep_entity,
        documentname          TYPE ts_documentdata-documentname,
        purchaseordernumber   TYPE  ts_documentdata-purchaseordernumber,
        purchaseorderdate     TYPE ts_documentdata-purchaseorderdate,
        requesteddeliverydate TYPE ts_documentdata-requesteddeliverydate,
        soldtonumber          TYPE ts_documentdata-soldtonumber,
        soldtoname            TYPE ts_documentdata-soldtoname,
        soldtoaddress         TYPE ts_documentdata-soldtoaddress,
        shiptonumber          TYPE  ts_documentdata-shiptonumber,
        shiptoname            TYPE  ts_documentdata-shiptoname,
        shiptoaddress         TYPE  ts_documentdata-shiptoaddress,
        deliveryinstructions  TYPE  ts_documentdata-deliveryinstructions,
        salesordernumber      TYPE  ts_documentdata-salesordernumber,
        documentdataitemset   TYPE TABLE OF ts_documentdataitem WITH DEFAULT KEY,
      END OF ts_deep_entity.
protected section.
private section.
ENDCLASS.



CLASS ZGOOG_CL_METADATA_SAMPLE IMPLEMENTATION.
ENDCLASS.
