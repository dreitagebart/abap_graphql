CLASS zcl_gql_response_200 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
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
ENDCLASS.
