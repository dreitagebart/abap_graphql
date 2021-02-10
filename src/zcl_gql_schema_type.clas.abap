CLASS zcl_gql_schema_type DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_property,
             name     TYPE string,
             instance TYPE REF TO zcl_gql_schema_property,
           END OF ts_property,

           tt_property TYPE TABLE OF ts_property WITH KEY name.

    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      property
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_property.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name       TYPE string,
          mt_properties TYPE tt_property.

ENDCLASS.



CLASS zcl_gql_schema_type IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD property.
    DATA(lo_property) = NEW zcl_gql_schema_property( iv_name ).

    APPEND VALUE #( name     = iv_name
                    instance = lo_property ) TO mt_properties.

    ro_result = lo_property.
  ENDMETHOD.
ENDCLASS.
