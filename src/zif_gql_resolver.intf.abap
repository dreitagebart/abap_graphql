INTERFACE zif_gql_resolver
  PUBLIC.

  CONSTANTS: BEGIN OF mc_types,
               query    TYPE c LENGTH 1 VALUE 'Q',
               mutation TYPE c LENGTH 1 VALUE 'M',
             END OF mc_types.

  TYPES: BEGIN OF ts_method,
           name TYPE string,
           type TYPE c LENGTH 1,
         END OF ts_method,

         tt_method TYPE TABLE OF ts_method WITH KEY name.

  METHODS:
    get_methods
      RETURNING VALUE(rt_result) TYPE tt_method,
    get_name
      RETURNING VALUE(rv_result) TYPE string.
ENDINTERFACE.
