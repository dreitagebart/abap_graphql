CLASS zcl_gql_response_schema DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_gql_response.

    TYPES: tr_type     TYPE REF TO zcl_gql_schema_type,
           tr_query    TYPE REF TO zcl_gql_schema_query,
           tr_mutation TYPE REF TO zcl_gql_schema_mutation,

           tt_type     TYPE TABLE OF tr_type.

    METHODS:
      constructor
        IMPORTING
          it_types    TYPE tt_type
          io_query    TYPE tr_query
          io_mutation TYPE tr_mutation.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mt_types    TYPE tt_type,
          mr_query    TYPE tr_query,
          mr_mutation TYPE tr_mutation.

ENDCLASS.



CLASS zcl_gql_response_schema IMPLEMENTATION.
  METHOD constructor.
    mt_types = it_types.
    mr_query = io_query.
    mr_mutation = io_mutation.
  ENDMETHOD.

  METHOD zif_gql_response~is_error.

  ENDMETHOD.

  METHOD zif_gql_response~get_json.

  ENDMETHOD.
ENDCLASS.
