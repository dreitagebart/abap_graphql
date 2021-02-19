INTERFACE zif_gql_schema
  PUBLIC.

  TYPES: BEGIN OF ts_field,
           instance TYPE REF TO zcl_gql_schema_field,
         END OF ts_field,

         BEGIN OF ts_object,
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
         tt_mutation  TYPE TABLE OF ts_mutation WITH DEFAULT KEY,
         tt_field     TYPE TABLE OF ts_field.

  CONSTANTS: BEGIN OF mc_types,
               string  TYPE c LENGTH 1 VALUE 'S',
               boolean TYPE c LENGTH 1 VALUE 'B',
               float   TYPE c LENGTH 1 VALUE 'F',
               int     TYPE c LENGTH 1 VALUE 'I',
             END OF mc_types.
ENDINTERFACE.
