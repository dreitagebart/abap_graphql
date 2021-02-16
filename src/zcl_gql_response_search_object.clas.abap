CLASS zcl_gql_response_search_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_object,
             key  TYPE string,
             text TYPE string,
           END OF ts_object,

           tt_object TYPE TABLE OF ts_object.

    DATA: objects TYPE tt_object.

    METHODS:
      constructor
        IMPORTING
          it_objects TYPE tt_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gql_response_search_object IMPLEMENTATION.
  METHOD constructor.
    objects = it_objects.
  ENDMETHOD.
ENDCLASS.
