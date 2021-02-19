CLASS zcl_gql_schema_result DEFINITION
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
      object
        IMPORTING
                  io_object        TYPE REF TO zcl_gql_schema_object
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_result,
      required
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_result,
      list
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_result.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name     TYPE string,
          mv_list     TYPE abap_bool,
          mv_required TYPE abap_bool,
          mv_complex  TYPE abap_bool,
          mv_type     TYPE c LENGTH 1,
          mo_object   TYPE REF TO zcl_gql_schema_object.
ENDCLASS.



CLASS zcl_gql_schema_result IMPLEMENTATION.
  METHOD object.
    mv_complex = abap_true.
    mo_object = io_object.
  ENDMETHOD.

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
    mv_complex = abap_false.
    mv_type = zif_gql_schema=>mc_types-string.
  ENDMETHOD.

  METHOD boolean.
    mv_complex = abap_false.
    mv_type = zif_gql_schema=>mc_types-boolean.
  ENDMETHOD.

  METHOD float.
    mv_complex = abap_false.
    mv_type = zif_gql_schema=>mc_types-float.
  ENDMETHOD.

  METHOD int.
    mv_complex = abap_false.
    mv_type = zif_gql_schema=>mc_types-int.
  ENDMETHOD.
ENDCLASS.
