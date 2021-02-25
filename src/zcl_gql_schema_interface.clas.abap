CLASS zcl_gql_schema_interface DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      description
        IMPORTING
          iv_description TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name        TYPE string,
          mv_description TYPE string.

ENDCLASS.



CLASS zcl_gql_schema_interface IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD description.
    mv_description = iv_description.
  ENDMETHOD.
ENDCLASS.
