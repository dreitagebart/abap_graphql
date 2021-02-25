CLASS zcl_gql_directive_class DEFINITION
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



CLASS zcl_gql_directive_class IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 'CLASS' ).
  ENDMETHOD.
ENDCLASS.
