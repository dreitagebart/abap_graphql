CLASS zcl_gql_schema_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_gql_schema_generator
                 zcl_gql_query_parser.

  PUBLIC SECTION.
    INTERFACES: if_serializable_object.

    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      description
        IMPORTING
          iv_description TYPE string,
      directive
        IMPORTING
          iv_directive TYPE string,
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
    DATA: mv_name        TYPE string,
          mv_description TYPE string,
          mv_directive   TYPE string,
          mt_fields      TYPE zif_gql_schema=>tt_field,
          mo_result      TYPE REF TO zcl_gql_schema_result.

ENDCLASS.



CLASS zcl_gql_schema_query IMPLEMENTATION.
  METHOD directive.
    mv_directive = iv_directive.
  ENDMETHOD.

  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD description.
    mv_description = iv_description.
  ENDMETHOD.

  METHOD field.
    DATA(lo_field) = NEW zcl_gql_schema_field( iv_name ).

    APPEND lo_field TO mt_fields.

    ro_result = lo_field.
  ENDMETHOD.

  METHOD result.
    mo_result = NEW zcl_gql_schema_result( iv_name ).

    ro_result = mo_result.
  ENDMETHOD.
ENDCLASS.
