CLASS zcl_gql_schema_input DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_gql_schema_generator.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      description
        IMPORTING
          iv_description TYPE string,
      field
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name        TYPE string,
          mv_description TYPE string,
          mt_fields      TYPE zif_gql_schema=>tt_field.

ENDCLASS.



CLASS zcl_gql_schema_input IMPLEMENTATION.
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
ENDCLASS.
