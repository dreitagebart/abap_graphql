INTERFACE zif_gql_schema
  PUBLIC.

  TYPES: tr_field     TYPE REF TO zcl_gql_schema_field,
         tr_type      TYPE REF TO zcl_gql_schema_type,
         tr_query     TYPE REF TO zcl_gql_schema_query,
         tr_mutation  TYPE REF TO zcl_gql_schema_mutation,
         tr_interface TYPE REF TO zcl_gql_schema_interface,
         tr_input     TYPE REF TO zcl_gql_schema_input,
         tr_enum      TYPE REF TO zcl_gql_schema_enum,

         tt_enum      TYPE TABLE OF tr_enum WITH DEFAULT KEY,
         tt_interface TYPE TABLE OF tr_interface WITH DEFAULT KEY,
         tt_input     TYPE TABLE OF tr_input WITH DEFAULT KEY,
         tt_type      TYPE TABLE OF tr_type WITH DEFAULT KEY,
         tt_query     TYPE TABLE OF tr_query WITH DEFAULT KEY,
         tt_mutation  TYPE TABLE OF tr_mutation WITH DEFAULT KEY,
         tt_field     TYPE TABLE OF tr_field WITH DEFAULT KEY.

  CONSTANTS: BEGIN OF mc_basic_types,
               string  TYPE string VALUE 'String',
               boolean TYPE string VALUE 'Boolean',
               int     TYPE string VALUE 'Int',
               float   TYPE string VALUE 'Float',
             END OF mc_basic_types,

             BEGIN OF mc_types,
               string  TYPE c LENGTH 1 VALUE 'S',
               boolean TYPE c LENGTH 1 VALUE 'B',
               float   TYPE c LENGTH 1 VALUE 'F',
               int     TYPE c LENGTH 1 VALUE 'I',
             END OF mc_types.
ENDINTERFACE.
