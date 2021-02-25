CLASS zcl_gql_response_200 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_gql_response.

    DATA: message TYPE string.

    METHODS:
      constructor
        IMPORTING
          iv_message TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gql_response_200 IMPLEMENTATION.
  METHOD constructor.
    message = iv_message.
  ENDMETHOD.

  METHOD zif_gql_response~is_error.

  ENDMETHOD.

  METHOD zif_gql_response~get_json.
    rv_result = /ui2/cl_json=>serialize( message ).
  ENDMETHOD.
ENDCLASS.
