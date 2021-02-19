CLASS zcl_gql_schema_field DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC GLOBAL FRIENDS zcl_gql_schema_generator.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      string,
      boolean,
      float,
      int,
      required
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field,
      list
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_field.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name     TYPE string,
          mv_list     TYPE abap_bool,
          mv_required TYPE abap_bool,
          mv_type     TYPE c LENGTH 1.
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
    mv_name = zcl_gql_schema_generator=>camel_case( iv_name ).
  ENDMETHOD.

  METHOD string.
    mv_type = zif_gql_schema=>mc_types-string.
  ENDMETHOD.

  METHOD boolean.
    mv_type = zif_gql_schema=>mc_types-boolean.
  ENDMETHOD.

  METHOD float.
    mv_type = zif_gql_schema=>mc_types-float.
  ENDMETHOD.

  METHOD int.
    mv_type = zif_gql_schema=>mc_types-int.
  ENDMETHOD.
ENDCLASS.
