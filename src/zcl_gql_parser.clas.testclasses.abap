CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_parse_types FOR TESTING,
      test_parse_mutation FOR TESTING,
      test_parse_query FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.
  METHOD test_parse_types.
    BREAK developer.
    TRY.
        zcl_gql_parser=>parse_types( |type Material \{ number: String! description: String! authorizationGroup: String! \}|
                                     && | type PlantMaterial \{ number: String! description: String! authorizationGroup: String! \}| ).
      CATCH zcx_gql_parser_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_query.
    TRY.
        zcl_gql_parser=>parse_query( |type Query \{ getMaterial(id: String!): Material! \}| ).
      CATCH zcx_gql_parser_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_mutation.
    TRY.
        zcl_gql_parser=>parse_mutation( |type Mutation \{ updateMaterial(id: String!): Material! \}| ).
      CATCH zcx_gql_parser_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
