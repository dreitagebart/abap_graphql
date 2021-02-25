CLASS zca_gql_schema_directive DEFINITION
  PUBLIC
  ABSTRACT
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



CLASS zca_gql_schema_directive IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
ENDCLASS.
