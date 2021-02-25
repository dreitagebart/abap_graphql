CLASS zcl_gql_schema_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_type_name
        IMPORTING
                  io_type          TYPE REF TO zcl_gql_schema_type
        RETURNING VALUE(rv_result) TYPE string,
      get_query_name
        IMPORTING
                  io_query         TYPE REF TO zcl_gql_schema_query
        RETURNING VALUE(rv_result) TYPE string,
      get_mutation_name
        IMPORTING
                  io_mutation      TYPE REF TO zcl_gql_schema_mutation
        RETURNING VALUE(rv_result) TYPE string.

    METHODS:
      add_type
        IMPORTING
          io_type TYPE REF TO zcl_gql_schema_type,
      add_query
        IMPORTING
          io_query TYPE REF TO zcl_gql_schema_query,
      add_mutation
        IMPORTING
          io_mutation TYPE REF TO zcl_gql_schema_mutation,
      generate
        EXPORTING
                  ev_types    TYPE string
                  ev_query    TYPE string
                  ev_mutation TYPE string
        RAISING   zcx_gql_error,
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
      type
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_type.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mt_types      TYPE zif_gql_schema=>tt_type,
          mt_enums      TYPE zif_gql_schema=>tt_enum,
          mt_inputs     TYPE zif_gql_schema=>tt_input,
          mt_interfaces TYPE zif_gql_schema=>tt_interface,
          mt_queries    TYPE zif_gql_schema=>tt_query,
          mt_mutations  TYPE zif_gql_schema=>tt_mutation.

    METHODS:
      get_type
        IMPORTING
                  iv_type          TYPE char1
        RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.



CLASS zcl_gql_schema_generator IMPLEMENTATION.
  METHOD add_type.
    APPEND io_type TO mt_types.
  ENDMETHOD.

  METHOD add_query.
    APPEND io_query TO mt_queries.
  ENDMETHOD.

  METHOD add_mutation.
    APPEND io_mutation TO mt_mutations.
  ENDMETHOD.

  METHOD get_type.
    CASE iv_type.
      WHEN zif_gql_schema=>mc_types-string.
        rv_result = 'String'.
      WHEN zif_gql_schema=>mc_types-boolean.
        rv_result = 'Boolean'.
      WHEN zif_gql_schema=>mc_types-float.
        rv_result = 'Float'.
      WHEN zif_gql_schema=>mc_types-int.
        rv_result = 'Int'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_type_name.
    rv_result = io_type->mv_name.
  ENDMETHOD.

  METHOD get_mutation_name.
    rv_result = io_mutation->mv_name.
  ENDMETHOD.

  METHOD get_query_name.
    rv_result = io_query->mv_name.
  ENDMETHOD.

  METHOD generate.
    DATA: lv_types    TYPE string,
          lv_query    TYPE string,
          lv_mutation TYPE string.

    DATA(lv_count) = lines( mt_types ).

    LOOP AT mt_types REFERENCE INTO DATA(lr_type).
      lv_types = lv_types && |type | && lr_type->*->mv_name
                 && | \{| && cl_abap_char_utilities=>cr_lf.

      IF lines( lr_type->*->mt_fields ) = 0.
        RAISE EXCEPTION TYPE zcx_gql_error
          MESSAGE e007 WITH lr_type->*->mv_name.
      ENDIF.

      LOOP AT lr_type->*->mt_fields REFERENCE INTO DATA(lr_typefield).
        lv_types = lv_types && cl_abap_char_utilities=>horizontal_tab.

        lv_types = lv_types && lr_typefield->*->mv_name && |: |
                   && SWITCH #( lr_typefield->*->mv_list WHEN abap_true  THEN '['
                                                         WHEN abap_false THEN '' )
                   && lr_typefield->*->mv_type && |!|
                   && SWITCH #( lr_typefield->*->mv_list WHEN abap_true  THEN ']'
                                                         WHEN abap_false THEN '' )
                   && SWITCH #( lr_typefield->*->mv_list WHEN abap_true  THEN '!'
                                                         WHEN abap_false THEN '' ).

        lv_types = lv_types && cl_abap_char_utilities=>cr_lf.
      ENDLOOP.

      lv_types = lv_types && |\}| && cl_abap_char_utilities=>cr_lf && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.

    DATA(lv_query_count) = lines( mt_queries ).

    IF lv_query_count > 0.
      lv_query = |type Query \{| && cl_abap_char_utilities=>cr_lf.

      LOOP AT mt_queries REFERENCE INTO DATA(lr_query).
        DATA(lv_query_index) = sy-tabix.

        lv_query = lv_query && cl_abap_char_utilities=>horizontal_tab.

        IF lr_query->*->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e005 WITH lr_query->*->mv_name.
        ENDIF.

        lv_query = lv_query && lr_query->*->mv_name.

        lv_count = lines( lr_query->*->mt_fields ).

        LOOP AT lr_query->*->mt_fields REFERENCE INTO DATA(lr_queryfield).
          DATA(lv_index) = sy-tabix.

          IF lv_index = 1.
            lv_query = lv_query && |(|.
          ENDIF.

          lv_query = lv_query && lr_queryfield->*->mv_name && |: |
                     && SWITCH #( lr_queryfield->*->mv_list WHEN abap_true  THEN '['
                                                            WHEN abap_false THEN '' )
                     && lr_queryfield->*->mv_type
                     && SWITCH #( lr_queryfield->*->mv_required WHEN abap_true  THEN '!'
                                                                WHEN abap_false THEN '' )
                     && SWITCH #( lr_queryfield->*->mv_list WHEN abap_true  THEN ']'
                                                            WHEN abap_false THEN '' ).
          IF lv_index < lv_count.
            lv_query = lv_query && |, |.
          ENDIF.

          IF lv_index = lv_count.
            lv_query = lv_query && |)|.
          ENDIF.
        ENDLOOP.

        IF lr_query->*->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e008 WITH lr_query->*->mv_name.
        ENDIF.

        lv_query = lv_query && |: |.

        lv_query = lv_query && SWITCH #( lr_query->*->mo_result->mv_list WHEN abap_true  THEN '['
                                                                         WHEN abap_false THEN '' ).

        lv_query = lv_query && lr_query->*->mo_result->mv_type.

        lv_query = lv_query && SWITCH #( lr_query->*->mo_result->mv_required WHEN abap_true  THEN '!'
                                                                             WHEN abap_false THEN '' ).

        lv_query = lv_query && SWITCH #( lr_query->*->mo_result->mv_list WHEN abap_true  THEN ']'
                                                                         WHEN abap_false THEN '' ).

        lv_query = lv_query && | @class(name: "| && lr_query->*->mv_directive && |")|.

        IF lv_query_count <> lv_query_index.
          lv_query = lv_query && cl_abap_char_utilities=>cr_lf.
        ENDIF.
      ENDLOOP.

      lv_query = lv_query && cl_abap_char_utilities=>cr_lf && |\}|.
    ENDIF.

    DATA(lv_mutation_count) = lines( mt_mutations ).

    IF lv_mutation_count > 0.
      lv_mutation = |type Mutation \{| && cl_abap_char_utilities=>cr_lf.

      LOOP AT mt_mutations REFERENCE INTO DATA(lr_mutation).
        DATA(lv_mutation_index) = sy-tabix.

        lv_mutation = lv_mutation && cl_abap_char_utilities=>horizontal_tab.

        IF lr_mutation->*->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e005 WITH lr_mutation->*->mv_name.
        ENDIF.

        lv_mutation = lv_mutation && lr_mutation->*->mv_name.

        lv_count = lines( lr_mutation->*->mt_fields ).

        LOOP AT lr_mutation->*->mt_fields REFERENCE INTO DATA(lr_mutationfield).
          lv_index = sy-tabix.

          IF lv_index = 1.
            lv_mutation = lv_mutation && |(|.
          ENDIF.

          lv_mutation = lv_mutation && lr_mutationfield->*->mv_name && |: |
                        && SWITCH #( lr_mutationfield->*->mv_list WHEN abap_true  THEN '['
                                                                  WHEN abap_false THEN '' )
                        && lr_mutationfield->*->mv_type
                        && SWITCH #( lr_mutationfield->*->mv_required WHEN abap_true  THEN '!'
                                                                      WHEN abap_false THEN '' )
                        && SWITCH #( lr_mutationfield->*->mv_list WHEN abap_true  THEN ']'
                                                                  WHEN abap_false THEN '' ).
          IF lv_index < lv_count.
            lv_mutation = lv_mutation && |, |.
          ENDIF.

          IF lv_index = lv_count.
            lv_mutation = lv_mutation && |)|.
          ENDIF.
        ENDLOOP.

        IF lr_mutation->*->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e008 WITH lr_mutation->*->mv_name.
        ENDIF.

        lv_mutation = lv_mutation && |: |.

        lv_mutation = lv_mutation && SWITCH #( lr_mutation->*->mo_result->mv_list WHEN abap_true  THEN '['
                                                                                  WHEN abap_false THEN '' ).

        lv_mutation = lv_mutation && lr_mutation->*->mo_result->mv_type.

        lv_mutation = lv_mutation && SWITCH #( lr_mutation->*->mo_result->mv_required WHEN abap_true  THEN '!'
                                                                                WHEN abap_false THEN '' ).

        lv_mutation = lv_mutation && SWITCH #( lr_mutation->*->mo_result->mv_list WHEN abap_true  THEN ']'
                                                                                  WHEN abap_false THEN '' ).

        lv_mutation = lv_mutation && | @class(name: "| && lr_mutation->*->mv_directive && |")|.

        IF lv_mutation_count <> lv_mutation_index.
          lv_mutation = lv_mutation && cl_abap_char_utilities=>cr_lf.
        ENDIF.
      ENDLOOP.

      lv_mutation = lv_mutation && cl_abap_char_utilities=>cr_lf && |\}|.
    ENDIF.

    ev_types = lv_types.
    ev_query = lv_query.
    ev_mutation = lv_mutation.
  ENDMETHOD.

  METHOD enum.
    DATA(lo_enum) = NEW zcl_gql_schema_enum( iv_name ).

    APPEND lo_enum TO mt_enums.

    ro_result = lo_enum.
  ENDMETHOD.

  METHOD mutation.
    DATA(lo_mutation) = NEW zcl_gql_schema_mutation( iv_name ).

    APPEND lo_mutation TO mt_mutations.

    ro_result = lo_mutation.
  ENDMETHOD.

  METHOD query.
    DATA(lo_query) = NEW zcl_gql_schema_query( iv_name ).

    APPEND lo_query TO mt_queries.

    ro_result = lo_query.
  ENDMETHOD.

  METHOD input.
    DATA(lo_input) = NEW zcl_gql_schema_input( iv_name ).

    APPEND lo_input TO mt_inputs.

    ro_result = lo_input.
  ENDMETHOD.

  METHOD interface.
    DATA(lo_interface) = NEW zcl_gql_schema_interface( iv_name ).

    APPEND lo_interface TO mt_interfaces.

    ro_result = lo_interface.
  ENDMETHOD.

  METHOD type.
    DATA(lo_type) = NEW zcl_gql_schema_type( iv_name ).

    APPEND lo_type TO mt_types.

    ro_result = lo_type.
  ENDMETHOD.
ENDCLASS.
