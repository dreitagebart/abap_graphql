CLASS zcl_gql_schema_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_object,
             instance TYPE REF TO zcl_gql_schema_object,
           END OF ts_object,

           BEGIN OF ts_query,
             instance TYPE REF TO zcl_gql_schema_query,
           END OF ts_query,

           BEGIN OF ts_mutation,
             instance TYPE REF TO zcl_gql_schema_mutation,
           END OF ts_mutation,

           BEGIN OF ts_interface,
             instance TYPE REF TO zcl_gql_schema_interface,
           END OF ts_interface,

           BEGIN OF ts_input,
             instance TYPE REF TO zcl_gql_schema_input,
           END OF ts_input,

           BEGIN OF ts_enum,
             instance TYPE REF TO zcl_gql_schema_enum,
           END OF ts_enum,

           tt_enum      TYPE TABLE OF ts_enum WITH DEFAULT KEY,
           tt_interface TYPE TABLE OF ts_interface WITH DEFAULT KEY,
           tt_input     TYPE TABLE OF ts_input WITH DEFAULT KEY,
           tt_object    TYPE TABLE OF ts_object WITH DEFAULT KEY,
           tt_query     TYPE TABLE OF ts_query WITH DEFAULT KEY,
           tt_mutation  TYPE TABLE OF ts_mutation WITH DEFAULT KEY.

    METHODS:
      enum
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_enum,
      input
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_input,
      interface
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_interface,
      query
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_query,
      mutation
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_mutation,
      object
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_object.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mt_objects    TYPE tt_object,
          mt_enums      TYPE tt_enum,
          mt_inputs     TYPE tt_input,
          mt_interfaces TYPE tt_interface,
          mt_queries    TYPE tt_query,
          mt_mutations  TYPE tt_mutation.

ENDCLASS.



CLASS zcl_gql_schema_generator IMPLEMENTATION.
  METHOD enum.
    DATA(lo_enum) = NEW zcl_gql_schema_enum( iv_name ).

    APPEND VALUE #( instance = lo_enum ) TO mt_enums.

    ro_result = lo_enum.
  ENDMETHOD.

  METHOD mutation.
    DATA(lo_mutation) = NEW zcl_gql_schema_mutation( iv_name ).

    APPEND VALUE #( instance = lo_mutation ) TO mt_mutations.

    ro_result = lo_mutation.
  ENDMETHOD.

  METHOD query.
    DATA(lo_query) = NEW zcl_gql_schema_query( iv_name ).

    APPEND VALUE #( instance = lo_query ) TO mt_queries.

    ro_result = lo_query.
  ENDMETHOD.

  METHOD input.
    DATA(lo_input) = NEW zcl_gql_schema_input( iv_name ).

    APPEND VALUE #( instance = lo_input ) TO mt_inputs.

    ro_result = lo_input.
  ENDMETHOD.

  METHOD interface.
    DATA(lo_interface) = NEW zcl_gql_schema_interface( iv_name ).

    APPEND VALUE #( instance = lo_interface ) TO mt_interfaces.

    ro_result = lo_interface.
  ENDMETHOD.

  METHOD object.
    DATA(lo_object) = NEW zcl_gql_schema_object( iv_name ).

    APPEND VALUE #( instance = lo_object ) TO mt_objects.

    ro_result = lo_object.
  ENDMETHOD.
ENDCLASS.
