CLASS zcl_gql_schema_input DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name TYPE string.
ENDCLASS.



CLASS zcl_gql_schema_input IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
ENDCLASS.
