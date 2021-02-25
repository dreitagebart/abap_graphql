CLASS zcl_gql_response_search_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_gql_response.

    TYPES: BEGIN OF ts_object,
             key  TYPE string,
             text TYPE string,
           END OF ts_object,

           tt_object TYPE TABLE OF ts_object.

    DATA: mt_objects TYPE tt_object.

    METHODS:
      constructor
        IMPORTING
          it_objects TYPE tt_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gql_response_search_object IMPLEMENTATION.
  METHOD constructor.
    mt_objects = it_objects.
  ENDMETHOD.

  METHOD zif_gql_response~is_error.

  ENDMETHOD.

  METHOD zif_gql_response~get_json.
    rv_result = /ui2/cl_json=>serialize(
                  data = me
                  pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                  name_mappings = VALUE #( ( abap = 'MT_OBJECTS' json = 'objects' ) )
                ).
  ENDMETHOD.
ENDCLASS.
