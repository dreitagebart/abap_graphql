CLASS zcl_gql_directive_function DEFINITION
  PUBLIC
  INHERITING FROM zca_gql_schema_directive
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_gql_directive_function IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 'FUNCTION' ).
  ENDMETHOD.
ENDCLASS.
