CLASS zcl_gql_schema_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_type,
             name     TYPE string,
             instance TYPE REF TO zcl_gql_schema_type,
           END OF ts_type,

           tt_type TYPE TABLE OF ts_type WITH KEY name.

    METHODS:
      type
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_type.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_types TYPE tt_type.

ENDCLASS.



CLASS zcl_gql_schema_generator IMPLEMENTATION.
  METHOD type.
    DATA(lo_type) = NEW zcl_gql_schema_type( iv_name ).

    APPEND VALUE #( name     = iv_name
                    instance = lo_type ) TO mt_types.

    ro_result = lo_type.
  ENDMETHOD.
ENDCLASS.
