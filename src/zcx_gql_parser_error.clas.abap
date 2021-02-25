CLASS zcx_gql_parser_error DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.

    METHODS:
      constructor
        IMPORTING
          position TYPE string
          message  TYPE string,
      get_position
        RETURNING VALUE(rv_result) TYPE string,
      get_message
        RETURNING VALUE(rv_result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_position TYPE string,
          mv_message  TYPE string.
ENDCLASS.



CLASS ZCX_GQL_PARSER_ERROR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).

    if_t100_message~t100key-msgid = 'ZGQL_MESSAGES'.

    mv_position = position.
    mv_message = message.
  ENDMETHOD.


  METHOD get_message.
    rv_result = mv_message.
  ENDMETHOD.


  METHOD get_position.
    rv_result = mv_position.
  ENDMETHOD.
ENDCLASS.
