CLASS zcx_gql_error DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.

    METHODS:
      constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_gql_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).

    if_t100_message~t100key-msgid = 'ZGQL_MESSAGES'.
  ENDMETHOD.
ENDCLASS.
