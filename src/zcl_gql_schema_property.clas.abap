CLASS zcl_gql_schema_property DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,
      required
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_property,
      list
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_property.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_name     TYPE string,
          mv_list     TYPE abap_bool,
          mv_required TYPE abap_bool.
ENDCLASS.



CLASS zcl_gql_schema_property IMPLEMENTATION.
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
ENDCLASS.
