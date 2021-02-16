CLASS zcl_gql_response_schema DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA: object   TYPE string,
          query    TYPE string,
          mutation TYPE string.

    METHODS:
      constructor
        IMPORTING
          iv_object   TYPE string
          iv_query    TYPE string
          iv_mutation TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gql_response_schema IMPLEMENTATION.
  METHOD constructor.
    object = iv_object.
    query = iv_query.
    mutation = iv_mutation.
  ENDMETHOD.
ENDCLASS.
