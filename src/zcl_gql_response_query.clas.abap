CLASS zcl_gql_response_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_gql_response.

    TYPES: BEGIN OF ts_data,
             name TYPE REF TO data,
           END OF ts_data.

    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string
          io_data TYPE REF TO data.

    DATA: ms_data TYPE ts_data.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name TYPE string.

ENDCLASS.



CLASS zcl_gql_response_query IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
    ms_data-name = io_data.
  ENDMETHOD.

  METHOD zif_gql_response~is_error.

  ENDMETHOD.

  METHOD zif_gql_response~get_json.
    rv_result = /ui2/cl_json=>serialize(
                  data             = me
                  pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
                  name_mappings    = VALUE #( ( abap = 'MS_DATA' json = 'data' )
                                              ( abap = 'NAME' json = mv_name ) )
                ).
  ENDMETHOD.
ENDCLASS.
