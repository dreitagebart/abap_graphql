CLASS zcl_gql_sdl_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      parse_type
        IMPORTING
                  iv_type          TYPE string
        RETURNING VALUE(ro_result) TYPE zif_gql_schema=>tr_type
        RAISING   zcx_gql_parser_error,
      parse_types
        IMPORTING
                  iv_types         TYPE string
        RETURNING VALUE(rt_result) TYPE zif_gql_schema=>tt_type
        RAISING   zcx_gql_parser_error,
      parse_mutations
        IMPORTING
                  iv_mutation      TYPE string
        RETURNING VALUE(rt_result) TYPE zif_gql_schema=>tt_mutation
        RAISING   zcx_gql_parser_error,
      parse_queries
        IMPORTING
                  iv_query         TYPE string
        RETURNING VALUE(rt_result) TYPE zif_gql_schema=>tt_query
        RAISING   zcx_gql_parser_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS:
      get_parser
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_sdl_parser.

    METHODS:
      eat
        IMPORTING
          iv_sdl       TYPE string
          iv_type      TYPE char1
        EXPORTING
          et_queries   TYPE zif_gql_schema=>tt_query
          et_mutations TYPE zif_gql_schema=>tt_mutation,
      assert_list
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      assert_parameters
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      assert_required
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      assert_query
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      assert_mutation
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      assert_type
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      assert_directive
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE abap_bool,
      condense
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE string,
      constructor,
      get_type_fields
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rt_result) TYPE zif_gql_schema=>tt_field
        RAISING   zcx_gql_parser_error,
      get_type_name
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS zcl_gql_sdl_parser IMPLEMENTATION.
  METHOD condense.
    DATA(lv_string) = iv_string.

    CONDENSE lv_string.

    rv_result = lv_string.
  ENDMETHOD.

  METHOD eat.
    DATA: lv_string TYPE string,
          lv_name   TYPE string.

    lv_string = condense( iv_sdl ).

    DATA(lv_length) = strlen( iv_sdl ).

    DATA(lv_next) = abap_true.

    DATA(lv_count) = 0.

    DATA(lv_new) = abap_true.
    DATA(lv_comma) = abap_false.
    DATA lv_digit TYPE c LENGTH 1.

    WHILE lv_next = abap_true.
      lv_digit = lv_string+lv_count(1).

      CASE lv_digit.
        WHEN '('.
          DATA(lv_type) = abap_false.
          DATA(lv_params) = abap_true.

          lv_name = lv_string(lv_count).

          lv_count = lv_count + 1.

          lv_length = lv_length - lv_count.

          lv_string = lv_string+lv_count(lv_length).

          CASE iv_type.
            WHEN zif_gql_resolver=>mc_types-query.
              DATA(lo_query) = NEW zcl_gql_schema_query( lv_name ).

              APPEND lo_query TO et_queries.
            WHEN zif_gql_resolver=>mc_types-mutation.
              DATA(lo_mutation) = NEW zcl_gql_schema_mutation( lv_name ).

              APPEND lo_mutation TO et_mutations.
          ENDCASE.

          lv_count = 0.

          lv_new = abap_false.
        WHEN ':'.
          IF lv_new = abap_true.
            lv_name = lv_string(lv_count).

            lv_name = condense( lv_name ).

            lv_count = lv_count + 1.

            lv_length = lv_length - lv_count.

            lv_string = lv_string+lv_count(lv_length).

            CASE iv_type.
              WHEN zif_gql_resolver=>mc_types-query.
                lo_query = NEW zcl_gql_schema_query( lv_name ).

                APPEND lo_query TO et_queries.
              WHEN zif_gql_resolver=>mc_types-mutation.
                lo_mutation = NEW zcl_gql_schema_mutation( lv_name ).

                APPEND lo_mutation TO et_mutations.
            ENDCASE.

            lv_count = 0.
          ENDIF.

          IF lv_params = abap_true.
            lv_name = lv_string(lv_count).

            CASE iv_type.
              WHEN zif_gql_resolver=>mc_types-query.
                DATA(lo_field) = lo_query->field( lv_name ).
              WHEN zif_gql_resolver=>mc_types-mutation.
                lo_field = lo_mutation->field( lv_name ).
            ENDCASE.

            lv_count = lv_count + 1.

            lv_length = lv_length - lv_count.

            lv_string = lv_string+lv_count(lv_length).

            lv_count = 0.

            lv_new = abap_false.
          ELSE.
            lv_count = lv_count + 1.

            lv_length = lv_length - lv_count.

            lv_string = lv_string+lv_count(lv_length).

            lv_count = 0.

            lv_new = abap_true.
          ENDIF.

          lv_type = abap_true.
        WHEN ','.
          IF lv_comma = abap_true.
            lv_name = lv_string(lv_count).

            lv_name = condense( lv_name ).

            IF lv_name IS NOT INITIAL.
              lo_field->type( lv_name ).
            ENDIF.
          ENDIF.

          lv_count = lv_count + 1.

          lv_length = lv_length - lv_count.

          lv_string = lv_string+lv_count(lv_length).

          lv_count = 0.

          lv_comma = abap_true.
          lv_type = abap_false.
          lv_new = abap_false.
        WHEN ')'.
          IF lv_type = abap_true.
            lv_name = lv_string(lv_count).

            lv_name = condense( lv_name ).

            IF assert_required( lv_name ) = abap_true.
              lv_length = strlen( lv_name ).

              lv_length = lv_length - 1.

              lv_name = lv_name(lv_length).

              lo_field->required( ).
            ENDIF.

            lo_field->type( lv_name ).
          ENDIF.

          lv_count = lv_count + 1.

          lv_length = lv_length - lv_count.

          lv_string = lv_string+lv_count(lv_length).

          lv_count = 0.

          lv_params = abap_false.
          lv_type = abap_false.
          lv_new = abap_false.
          lv_comma = abap_false.
        WHEN '['.
          DATA(lv_list) = abap_true.

          lv_count = lv_count + 1.

          lv_length = lv_length - lv_count.

          lv_string = lv_string+lv_count(lv_length).

          lv_count = 0.

          lv_new = abap_false.
        WHEN ']'.
          lv_list = abap_false.

          lv_count = lv_count + 1.

          lv_length = lv_length - lv_count.

          lv_string = lv_string+lv_count(lv_length).

          lv_count = 0.

          lv_type = abap_false.
          lv_new = abap_false.
        WHEN '!'.
          IF lv_params = abap_true.
            IF lv_type = abap_true.
              lv_count = lv_count + 1.

              lv_name = lv_string(lv_count).

              lv_name = condense( lv_name ).

              lv_length = lv_length - lv_count.

              lv_string = lv_string+lv_count(lv_length).

              IF assert_required( lv_name ) = abap_true.
                lv_length = strlen( lv_name ).

                lv_length = lv_length - 1.

                lv_name = lv_name(lv_length).

                lo_field->required( ).
              ENDIF.

              lo_field->type( lv_name ).

              lv_type = abap_false.
              lv_count = 0.
            ENDIF.
          ELSE.
            IF lv_type = abap_true.
              lv_count = lv_count + 1.

              lv_name = lv_string(lv_count).

              lv_name = condense( lv_name ).

              lv_length = lv_length - lv_count.

              lv_string = lv_string+lv_count(lv_length).

              IF assert_required( lv_name ) = abap_true.
                lv_length = strlen( lv_name ).

                lv_length = lv_length - 1.

                lv_name = lv_name(lv_length).

                CASE iv_type.
                  WHEN zif_gql_resolver=>mc_types-query.
                    DATA(lo_result) = lo_query->result( lv_name ).
                  WHEN zif_gql_resolver=>mc_types-mutation.
                    lo_result = lo_mutation->result( lv_name ).
                ENDCASE.

                lo_result->type( lv_name ).

                IF lv_list = abap_true.
                  lo_result->list( ).
                ENDIF.

                lo_result->required( ).
              ENDIF.

              lv_count = 0.
            ELSE.
              lv_count = lv_count + 1.

              lv_length = lv_length - lv_count.

              lv_string = lv_string+lv_count(lv_length).

              lv_count = 0.
            ENDIF.
          ENDIF.
        WHEN '@'.
          lv_string = substring_after( val = lv_string sub = 'name:' ).

          lv_name = substring_before( val = lv_string sub = ')' ).

          REPLACE ALL OCCURRENCES OF |"| IN lv_name WITH ''.

          lv_name = condense( lv_name ).

          lv_string = substring_after( val = lv_string sub = ')' ).

          CASE iv_type.
            WHEN zif_gql_resolver=>mc_types-query.
              lo_query->directive( lv_name ).
            WHEN zif_gql_resolver=>mc_types-mutation.
              lo_mutation->directive( lv_name ).
          ENDCASE.
*          lv_count = lv_count + 1.

*          lv_length = lv_length - lv_count.

*          lv_string = lv_string+lv_count(lv_length).

          lv_count = 0.
        WHEN ' '.
          lv_count = lv_count + 1.

          lv_length = lv_length - lv_count.

          lv_string = lv_string+lv_count(lv_length).

          lv_count = 0.
        WHEN OTHERS.
          lv_count = lv_count + 1.
      ENDCASE.

      lv_length = strlen( lv_string ).

      IF lv_count >= lv_length.
        lv_next = abap_false.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_parser.
    ro_result = NEW zcl_gql_sdl_parser( ).
  ENDMETHOD.

  METHOD parse_type.
    TRY.
        DATA(lt_types) = parse_types( iv_type ).
      CATCH zcx_gql_parser_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    ro_result = lt_types[ 1 ].
  ENDMETHOD.

  METHOD parse_types.
    DATA(lo_parser) = get_parser( ).

    SPLIT iv_types AT 'type' INTO TABLE DATA(lt_flakes).

    DELETE lt_flakes INDEX 1.

    LOOP AT lt_flakes REFERENCE INTO DATA(lr_flake).
      IF lo_parser->assert_type( lr_flake->* ) = abap_false.
        RAISE EXCEPTION TYPE zcx_gql_parser_error
          EXPORTING
            message  = 'type not defined'
            position = 'type parsing'.
      ELSE.
        DATA(lo_type) = NEW zcl_gql_schema_type( lo_parser->get_type_name( lr_flake->* ) ).

        TRY.
            LOOP AT lo_parser->get_type_fields( lr_flake->* ) REFERENCE INTO DATA(lr_field).
              DATA(lo_field) = lo_type->field( lr_field->*->mv_name ).

              lo_field->mv_description = lr_field->*->mv_description.
              lo_field->mv_list = lr_field->*->mv_list.
              lo_field->mv_required = lr_field->*->mv_required.
              lo_field->mv_type = lr_field->*->mv_type.

*              CASE lr_field->*->mv_type.
*                WHEN zif_gql_schema=>mc_types-string.
*                  lo_field->string( ).
*                WHEN zif_gql_schema=>mc_types-boolean.
*                  lo_field->boolean( ).
*                WHEN zif_gql_schema=>mc_types-float.
*                  lo_field->float( ).
*                WHEN zif_gql_schema=>mc_types-int.
*                  lo_field->int( ).
*              ENDCASE.
*
*              IF lr_field->*->mv_list = abap_true.
*                lo_field->list( ).
*              ENDIF.
*
*              IF lr_field->*->mv_required = abap_true.
*                lo_field->required( ).
*              ENDIF.
            ENDLOOP.
          CATCH zcx_gql_parser_error INTO DATA(lx_error).
            RAISE EXCEPTION lx_error.
        ENDTRY.
      ENDIF.

      APPEND lo_type TO rt_result.
    ENDLOOP.

    DATA lt_names TYPE TABLE OF string.

    LOOP AT rt_result REFERENCE INTO DATA(lr_result).
      APPEND lr_result->*->mv_name TO lt_names.
    ENDLOOP.

    SORT lt_names BY table_line.
    DELETE ADJACENT DUPLICATES FROM lt_names COMPARING table_line.

    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_gql_parser_error
        EXPORTING
          message  = 'Duplicate types in types definition'
          position = 'unknown'.
    ENDIF.
  ENDMETHOD.

  METHOD parse_queries.
    DATA lv_string TYPE string.

    CHECK iv_query IS NOT INITIAL.

    DATA(lo_parser) = get_parser( ).

    SPLIT iv_query AT 'type' INTO TABLE DATA(lt_flakes).

    IF lo_parser->assert_query( lt_flakes[ 2 ] ) = abap_false.
      RAISE EXCEPTION TYPE zcx_gql_parser_error
        EXPORTING
          message  = 'type is not a query'
          position = 'Query'.
    ENDIF.

    DELETE lt_flakes INDEX 1.

    SPLIT lt_flakes[ 1 ] AT |\{| INTO TABLE DATA(lt_splits).

    DELETE lt_splits INDEX 1.

    lv_string = lt_splits[ 1 ].

    REPLACE ALL OCCURRENCES OF |\}| IN lv_string WITH ''.

    lo_parser->eat(
      EXPORTING
        iv_sdl    = lv_string
        iv_type   = zif_gql_resolver=>mc_types-query
      IMPORTING
        et_queries = rt_result
    ).
  ENDMETHOD.

  METHOD parse_mutations.
    DATA lv_string TYPE string.

    CHECK iv_mutation IS NOT INITIAL.

    DATA(lo_parser) = get_parser( ).

    SPLIT iv_mutation AT 'type' INTO TABLE DATA(lt_flakes).

    IF lo_parser->assert_mutation( lt_flakes[ 2 ] ) = abap_false.
      RAISE EXCEPTION TYPE zcx_gql_parser_error
        EXPORTING
          message  = 'type is not a mutation'
          position = 'Mutation'.
    ENDIF.

    DELETE lt_flakes INDEX 1.

    SPLIT lt_flakes[ 1 ] AT |\{| INTO TABLE DATA(lt_splits).

    DELETE lt_splits INDEX 1.

    lv_string = lt_splits[ 1 ].

    REPLACE ALL OCCURRENCES OF |\}| IN lv_string WITH ''.

    lo_parser->eat(
      EXPORTING
        iv_sdl    = lv_string
        iv_type   = zif_gql_resolver=>mc_types-mutation
      IMPORTING
        et_mutations = rt_result
    ).
  ENDMETHOD.

  METHOD assert_mutation.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    IF condense( lt_flakes[ 1 ] ) = 'Mutation'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_query.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    IF condense( lt_flakes[ 1 ] ) = 'Query'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_list.
    IF iv_string CP '*['.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_parameters.
    IF iv_string CP '*(*)*'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_required.
    IF iv_string CP '*!'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_type_fields.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    DELETE lt_flakes INDEX 1.

    LOOP AT lt_flakes REFERENCE INTO DATA(lr_flake).
      SPLIT lr_flake->* AT ':' INTO TABLE DATA(lt_fields).

      TRY.
          DATA(lv_field) = condense( lt_fields[ 1 ] ).

          DATA(lo_field) = NEW zcl_gql_schema_field( lv_field ).

          DELETE lt_fields INDEX 1.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      DATA(lv_field_count) = lines( lt_fields ).

      LOOP AT lt_fields REFERENCE INTO DATA(lr_field).
        DATA(lv_index) = sy-tabix.

        SPLIT condense( lr_field->* ) AT ' ' INTO TABLE DATA(lt_fragments).

        DATA(lv_type) = condense( lt_fragments[ 1 ] ).

        IF assert_required( lv_type ) = abap_true.
          REPLACE ALL OCCURRENCES OF '!' IN lv_type WITH ''.

          lo_field->required( ).
        ENDIF.

        CASE lv_type.
          WHEN zif_gql_schema=>mc_basic_types-string.
            lo_field->string( ).
          WHEN zif_gql_schema=>mc_basic_types-boolean.
            lo_field->boolean( ).
          WHEN zif_gql_schema=>mc_basic_types-float.
            lo_field->float( ).
          WHEN zif_gql_schema=>mc_basic_types-int.
            lo_field->int( ).
          WHEN OTHERS.
            lo_field->type( lv_type ).
        ENDCASE.

        APPEND lo_field TO rt_result.

        IF lv_index < lv_field_count.
          lv_field = condense( lt_fragments[ 2 ] ).

          lo_field = NEW zcl_gql_schema_field( lv_field ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_type_name.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    rv_result = condense( lt_flakes[ 1 ] ).
  ENDMETHOD.

  METHOD assert_type.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    IF condense( lt_flakes[ 1 ] ) IS NOT INITIAL.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_directive.

  ENDMETHOD.

  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
