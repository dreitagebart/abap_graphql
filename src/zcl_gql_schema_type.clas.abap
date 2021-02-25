CLASS zcl_gql_schema_type DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_gql_schema_generator
                 zcl_gql_query_parser
                 zcl_gql_sdl_parser.

  PUBLIC SECTION.
    INTERFACES: if_serializable_object.

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



CLASS zcl_gql_schema_type IMPLEMENTATION.
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
