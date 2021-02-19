CLASS zcl_gql_schema_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      camel_case
        IMPORTING
                  iv_name          TYPE string
                  iv_capitalized   TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rv_result) TYPE string.

    METHODS:
      generate
        EXPORTING
                  ev_object   TYPE string
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
      object
        IMPORTING
                  iv_name          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_object.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mt_objects    TYPE zif_gql_schema=>tt_object,
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
  METHOD camel_case.
    DATA lv_name TYPE string.

    lv_name = iv_name.

    rv_result = /ui2/cl_abap2json=>convert_to_camel_case( lv_name ).

    IF iv_capitalized = abap_true.
      DATA(lv_length) = strlen( rv_result ) - 1.

      DATA(lv_capital) = to_upper( rv_result(1) ).

      IF lv_length > 0.
        DATA(lv_rest) = rv_result+1(lv_length).

        rv_result = lv_capital && lv_rest.
      ELSE.
        rv_result = lv_capital.
      ENDIF.
    ENDIF.
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

  METHOD generate.
    DATA: lv_object   TYPE string,
          lv_query    TYPE string,
          lv_mutation TYPE string.

    DATA(lv_count) = lines( mt_objects ).
    BREAK-POINT.
    LOOP AT mt_objects REFERENCE INTO DATA(lr_object).
      lv_object = lv_object && |type | && lr_object->instance->mv_name
                  && | \{| && cl_abap_char_utilities=>cr_lf.

      IF lines( lr_object->instance->mt_fields ) = 0.
        RAISE EXCEPTION TYPE zcx_gql_error
          MESSAGE e007 WITH lr_object->instance->mv_name.
      ENDIF.

      LOOP AT lr_object->instance->mt_fields REFERENCE INTO DATA(lr_objectfield).
        lv_object = lv_object && cl_abap_char_utilities=>horizontal_tab.

        lv_object = lv_object
                    && lr_objectfield->instance->mv_name && |: |
                    && SWITCH #( lr_objectfield->instance->mv_list WHEN abap_true  THEN '['
                                                                   WHEN abap_false THEN '' )
                    && get_type( lr_objectfield->instance->mv_type ) && |!|
                    && SWITCH #( lr_objectfield->instance->mv_list WHEN abap_true  THEN ']'
                                                                   WHEN abap_false THEN '' )
                    && SWITCH #( lr_objectfield->instance->mv_required WHEN abap_true  THEN '!'
                                                                       WHEN abap_false THEN '' ).

        lv_object = lv_object && cl_abap_char_utilities=>cr_lf.
      ENDLOOP.

      lv_object = lv_object && |\}| && cl_abap_char_utilities=>cr_lf && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.

    DATA(lv_query_count) = lines( mt_queries ).

    IF lv_query_count > 0.
      lv_query = |type Query \{| && cl_abap_char_utilities=>cr_lf.

      LOOP AT mt_queries REFERENCE INTO DATA(lr_query).
        DATA(lv_query_index) = sy-tabix.

        lv_query = lv_query && cl_abap_char_utilities=>horizontal_tab.

        IF lr_query->instance->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e005 WITH lr_query->instance->mv_name.
        ENDIF.

        lv_query = lv_query && lr_query->instance->mv_name.

        lv_count = lines( lr_query->instance->mt_fields ).

        LOOP AT lr_query->instance->mt_fields REFERENCE INTO DATA(lr_queryfield).
          DATA(lv_index) = sy-tabix.

          IF lv_index = 1.
            lv_query = lv_query && |(|.
          ENDIF.

          lv_query = lv_query && lr_queryfield->instance->mv_name && |: |
                     && SWITCH #( lr_queryfield->instance->mv_list WHEN abap_true  THEN '['
                                                                   WHEN abap_false THEN '' )
                     && get_type( lr_queryfield->instance->mv_type )
                     && SWITCH #( lr_queryfield->instance->mv_required WHEN abap_true  THEN '!'
                                                                       WHEN abap_false THEN '' )
                     && SWITCH #( lr_queryfield->instance->mv_list WHEN abap_true  THEN ']'
                                                                   WHEN abap_false THEN '' ).
          IF lv_index < lv_count.
            lv_query = lv_query && |, |.
          ENDIF.

          IF lv_index = lv_count.
            lv_query = lv_query && |)|.
          ENDIF.
        ENDLOOP.

        IF lr_query->instance->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e008 WITH lr_query->instance->mv_name.
        ENDIF.

        lv_query = lv_query && |: |.

        lv_query = lv_query && SWITCH #( lr_query->instance->mo_result->mv_list WHEN abap_true  THEN '['
                                                                                WHEN abap_false THEN '' ).

        IF lr_query->instance->mo_result->mv_complex = abap_true.
          lv_query = lv_query && lr_query->instance->mo_result->mo_object->mv_name.
        ELSE.
          lv_query = lv_query && get_type( lr_query->instance->mo_result->mv_type ).
        ENDIF.

        lv_query = lv_query && SWITCH #( lr_queryfield->instance->mv_required WHEN abap_true  THEN '!'
                                                                               WHEN abap_false THEN '' ).

        lv_query = lv_query && SWITCH #( lr_query->instance->mo_result->mv_list WHEN abap_true  THEN ']'
                                                                                WHEN abap_false THEN '' ).

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

        IF lr_mutation->instance->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e005 WITH lr_mutation->instance->mv_name.
        ENDIF.

        lv_mutation = lv_mutation && lr_mutation->instance->mv_name.

        lv_count = lines( lr_mutation->instance->mt_fields ).

        LOOP AT lr_mutation->instance->mt_fields REFERENCE INTO DATA(lr_mutationfield).
          lv_index = sy-tabix.

          IF lv_index = 1.
            lv_mutation = lv_mutation && |(|.
          ENDIF.

          lv_mutation = lv_mutation && lr_mutationfield->instance->mv_name && |: |
                        && SWITCH #( lr_mutationfield->instance->mv_list WHEN abap_true  THEN '['
                                                                         WHEN abap_false THEN '' )
                        && get_type( lr_mutationfield->instance->mv_type )
                        && SWITCH #( lr_mutationfield->instance->mv_required WHEN abap_true  THEN '!'
                                                                            WHEN abap_false THEN '' )
                        && SWITCH #( lr_mutationfield->instance->mv_list WHEN abap_true  THEN ']'
                                                                        WHEN abap_false THEN '' ).
          IF lv_index < lv_count.
            lv_mutation = lv_mutation && |, |.
          ENDIF.

          IF lv_index = lv_count.
            lv_mutation = lv_mutation && |)|.
          ENDIF.
        ENDLOOP.

        IF lr_mutation->instance->mo_result IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_gql_error
            MESSAGE e008 WITH lr_mutation->instance->mv_name.
        ENDIF.

        lv_mutation = lv_mutation && |: |.

        lv_mutation = lv_mutation && SWITCH #( lr_mutation->instance->mo_result->mv_list WHEN abap_true  THEN '['
                                                                                         WHEN abap_false THEN '' ).

        IF lr_mutation->instance->mo_result->mv_complex = abap_true.
          lv_mutation = lv_mutation && lr_mutation->instance->mo_result->mo_object->mv_name.
        ELSE.
          lv_mutation = lv_mutation && get_type( lr_mutation->instance->mo_result->mv_type ).
        ENDIF.

        lv_mutation = lv_mutation && SWITCH #( lr_mutationfield->instance->mv_required WHEN abap_true  THEN '!'
                                                                                       WHEN abap_false THEN '' ).

        lv_mutation = lv_mutation && SWITCH #( lr_mutation->instance->mo_result->mv_list WHEN abap_true  THEN ']'
                                                                                         WHEN abap_false THEN '' ).

        IF lv_mutation_count <> lv_mutation_index.
          lv_mutation = lv_mutation && cl_abap_char_utilities=>cr_lf.
        ENDIF.
      ENDLOOP.

      lv_mutation = lv_mutation && cl_abap_char_utilities=>cr_lf && |\}|.
    ENDIF.

    ev_object = lv_object.
    ev_query = lv_query.
    ev_mutation = lv_mutation.
  ENDMETHOD.

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
