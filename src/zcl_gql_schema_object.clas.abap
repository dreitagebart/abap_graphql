CLASS zcl_gql_schema_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_field,
             instance TYPE REF TO zcl_gql_schema_field,
           END OF ts_field,

           tt_field TYPE TABLE OF ts_field WITH DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      field
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name   TYPE string,
          mt_fields TYPE tt_field.

ENDCLASS.



CLASS zcl_gql_schema_object IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD field.
    DATA(lo_field) = NEW zcl_gql_schema_field( iv_name ).

    APPEND VALUE #( instance = lo_field ) TO mt_fields.

    ro_result = lo_field.
  ENDMETHOD.
ENDCLASS.
