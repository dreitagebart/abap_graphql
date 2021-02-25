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
    TRY.
        zcl_gql_sdl_parser=>parse_types( |type Material \{ number: String! description: String! authorizationGroup: String! \}|
                                     && | type PlantMaterial \{ number: String! description: String! authorizationGroup: String! \}| ).
      CATCH zcx_gql_parser_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_query.
    TRY.
        DATA(lt_queries) = zcl_gql_sdl_parser=>parse_queries( |type Query \{ getMaterial(id: String!, name: String): Material!   getMaterials: [Material!]!  getPlantMaterial(id: String!): Material! \}| ).
        BREAK developer.
      CATCH zcx_gql_parser_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_mutation.
    TRY.
        DATA(lt_mutations) = zcl_gql_sdl_parser=>parse_mutations( |type Mutation \{ updateMaterial(id: String!): Material!    deleteMaterial(id: String!): Material! \}| ).
        BREAK developer.
      CATCH zcx_gql_parser_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
