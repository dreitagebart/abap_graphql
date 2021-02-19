CLASS zcl_gql_test_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_material,
             client              TYPE mandt,
             material            TYPE matnr,
             material_type       TYPE mtart,
             material_group      TYPE matkl,
             authorization_group TYPE begru,
           END OF ts_material,

           tt_material TYPE TABLE OF ts_material WITH KEY material.

    INTERFACES: zif_gql_resolver.

    METHODS:
      get_material
        IMPORTING
                  material      TYPE matnr
        RETURNING VALUE(result) TYPE ts_material,
      update_material
        IMPORTING
                  material            TYPE matnr
                  material_type       TYPE mtart OPTIONAL
                  authorization_group TYPE begru OPTIONAL
        RETURNING VALUE(result)       TYPE ts_material,
      delete_material
        IMPORTING
                  material      TYPE matnr
        RETURNING VALUE(result) TYPE abap_bool,
      get_materials
        RETURNING VALUE(result) TYPE tt_material.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gql_test_class IMPLEMENTATION.
  METHOD zif_gql_resolver~get_name.
    rv_result = 'getGraphQL'.
  ENDMETHOD.

  METHOD zif_gql_resolver~get_methods.
    rt_result = VALUE #( ( name = 'GET_MATERIAL'    type = zif_gql_resolver=>mc_types-query )
                         ( name = 'UPDATE_MATERIAL' type = zif_gql_resolver=>mc_types-mutation )
                         ( name = 'DELETE_MATERIAL' type = zif_gql_resolver=>mc_types-mutation )
                         ( name = 'GET_MATERIALS'   type = zif_gql_resolver=>mc_types-query ) ).
  ENDMETHOD.

  METHOD get_material.
    SELECT SINGLE * FROM zmara INTO @result WHERE material = @material.
  ENDMETHOD.

  METHOD update_material.

  ENDMETHOD.

  METHOD delete_material.

  ENDMETHOD.

  METHOD get_materials.

  ENDMETHOD.
ENDCLASS.
