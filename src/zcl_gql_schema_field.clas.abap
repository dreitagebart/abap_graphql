CLASS zcl_gql_schema_field DEFINITION
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
      string,
      boolean,
      float,
      int,
      type
        IMPORTING
          iv_name TYPE string,
      required
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field,
      list
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name        TYPE string,
          mv_description TYPE string,
          mv_list        TYPE abap_bool,
          mv_required    TYPE abap_bool,
          mv_type        TYPE string.
ENDCLASS.



CLASS zcl_gql_schema_field IMPLEMENTATION.
  METHOD required.
    mv_required = abap_true.

    ro_result = me.
  ENDMETHOD.

  METHOD list.
    mv_list = abap_true.

    ro_result = me.
  ENDMETHOD.

  METHOD constructor.
    mv_required = abap_false.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD description.
    mv_description = iv_description.
  ENDMETHOD.

  METHOD string.
    mv_type = zif_gql_schema=>mc_basic_types-string.
  ENDMETHOD.

  METHOD boolean.
    mv_type = zif_gql_schema=>mc_basic_types-boolean.
  ENDMETHOD.

  METHOD float.
    mv_type = zif_gql_schema=>mc_basic_types-float.
  ENDMETHOD.

  METHOD int.
    mv_type = zif_gql_schema=>mc_basic_types-int.
  ENDMETHOD.

  METHOD type.
    mv_type = iv_name.
  ENDMETHOD.
ENDCLASS.
