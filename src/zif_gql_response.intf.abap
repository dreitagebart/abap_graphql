INTERFACE zif_gql_response
  PUBLIC.

  METHODS:
    get_json
      RETURNING VALUE(rv_result) TYPE string,
    is_error
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDINTERFACE.
