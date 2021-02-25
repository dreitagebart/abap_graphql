CLASS zcl_gql_response_400 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_gql_response.

    DATA: mv_message TYPE string.

    METHODS:
      constructor
        IMPORTING
          iv_message TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_gql_response_400 IMPLEMENTATION.
  METHOD constructor.
    mv_message = iv_message.
  ENDMETHOD.

  METHOD zif_gql_response~is_error.
    rv_result = abap_true.
  ENDMETHOD.

  METHOD zif_gql_response~get_json.
    rv_result = mv_message.
  ENDMETHOD.
ENDCLASS.
