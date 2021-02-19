CLASS zcl_gql_schema_mutation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC GLOBAL FRIENDS zcl_gql_schema_generator.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      field
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field,
      result
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_result.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name   TYPE string,
          mt_fields TYPE zif_gql_schema=>tt_field,
          mo_result TYPE REF TO zcl_gql_schema_result.

ENDCLASS.



CLASS zcl_gql_schema_mutation IMPLEMENTATION.
  METHOD constructor.
    mv_name = zcl_gql_schema_generator=>camel_case( iv_name ).
  ENDMETHOD.

  METHOD field.
    DATA(lo_field) = NEW zcl_gql_schema_field( iv_name ).

    APPEND VALUE #( instance = lo_field ) TO mt_fields.

    ro_result = lo_field.
  ENDMETHOD.

  METHOD result.
    mo_result = NEW zcl_gql_schema_result( iv_name ).

    ro_result = mo_result.
  ENDMETHOD.
ENDCLASS.
