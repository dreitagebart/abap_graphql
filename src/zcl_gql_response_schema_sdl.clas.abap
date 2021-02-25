CLASS zcl_gql_response_schema_sdl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_gql_response.

    DATA: mv_types    TYPE string,
          mv_query    TYPE string,
          mv_mutation TYPE string.

    METHODS:
      constructor
        IMPORTING
          iv_types    TYPE string
          iv_query    TYPE string
          iv_mutation TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_gql_response_schema_sdl IMPLEMENTATION.
  METHOD constructor.
    mv_types = iv_types.
    mv_query = iv_query.
    mv_mutation = iv_mutation.
  ENDMETHOD.

  METHOD zif_gql_response~is_error.

  ENDMETHOD.

  METHOD zif_gql_response~get_json.
    rv_result = /ui2/cl_json=>serialize(
      data             = me
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      name_mappings    = VALUE #( ( abap = 'MV_TYPES'    json = 'types' )
                                  ( abap = 'MV_QUERY'    json = 'query' )
                                  ( abap = 'MV_MUTATION' json = 'mutation' ) )
    ).
  ENDMETHOD.
ENDCLASS.
