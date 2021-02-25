CLASS zcl_gql_parser DEFINITION
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
      parse_mutation
        IMPORTING
                  iv_mutation      TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_mutation
        RAISING   zcx_gql_parser_error,
      parse_query
        IMPORTING
                  iv_query         TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_query
        RAISING   zcx_gql_parser_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS:
      get_parser
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_parser.

    METHODS:
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



CLASS zcl_gql_parser IMPLEMENTATION.
  METHOD get_parser.
    ro_result = NEW zcl_gql_parser( ).
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

  METHOD parse_query.
    DATA(lo_parser) = get_parser( ).

    SPLIT iv_query AT 'type' INTO TABLE DATA(lt_flakes).

    IF lo_parser->assert_query( lt_flakes[ 2 ] ) = abap_false.
      RAISE EXCEPTION TYPE zcx_gql_parser_error
        EXPORTING
          message  = 'type is not a query'
          position = 'Query'.
    ENDIF.

    BREAK developer.

    DELETE lt_flakes INDEX 1.

    SPLIT lt_flakes[ 1 ] AT |\{| INTO TABLE DATA(lt_splits).

    DELETE lt_splits INDEX 1.

    DATA(lv_string) = lt_splits[ 1 ].

    REPLACE ALL OCCURRENCES OF |\}| IN lv_string WITH ''.

    SPLIT lt_splits[ 1 ] AT '(' INTO TABLE DATA(lt_fragments).

    DATA(lv_query) = lt_fragments[ 1 ].

    CONDENSE lv_query.

    DATA(lo_query) = NEW zcl_gql_schema_query( lv_query ).
  ENDMETHOD.

  METHOD parse_mutation.
    DATA(lo_parser) = get_parser( ).

    SPLIT iv_mutation AT 'type' INTO TABLE DATA(lt_flakes).

    IF lo_parser->assert_mutation( lt_flakes[ 2 ] ) = abap_false.
      RAISE EXCEPTION TYPE zcx_gql_parser_error
        EXPORTING
          message  = 'type is not a mutation'
          position = 'Mutation'.
    ENDIF.
  ENDMETHOD.

  METHOD assert_mutation.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    DATA(lv_string) = lt_flakes[ 1 ].

    CONDENSE lv_string.

    IF lv_string = 'Mutation'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_query.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    DATA(lv_string) = lt_flakes[ 1 ].

    CONDENSE lv_string.

    IF lv_string = 'Query'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_required.
    IF iv_string CP '*!'.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_type_fields.
    BREAK developer.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    DELETE lt_flakes INDEX 1.

    LOOP AT lt_flakes REFERENCE INTO DATA(lr_flake).
      SPLIT lr_flake->* AT ':' INTO TABLE DATA(lt_fields).

      TRY.
          DATA(lv_field) = lt_fields[ 1 ].

          CONDENSE lv_field.

          DATA(lo_field) = NEW zcl_gql_schema_field( lv_field ).

          DELETE lt_fields INDEX 1.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      DATA(lv_field_count) = lines( lt_fields ).

      LOOP AT lt_fields REFERENCE INTO DATA(lr_field).
        DATA(lv_index) = sy-tabix.

        CONDENSE lr_field->*.

        SPLIT lr_field->* AT ' ' INTO TABLE DATA(lt_fragments).

        DATA(lv_type) = lt_fragments[ 1 ].

        CONDENSE lv_type.

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
        ENDCASE.

        APPEND lo_field TO rt_result.

        IF lv_index < lv_field_count.
          lv_field = lt_fragments[ 2 ].

          CONDENSE lv_field.

          lo_field = NEW zcl_gql_schema_field( lv_field ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_type_name.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    DATA(lv_string) = lt_flakes[ 1 ].

    CONDENSE lv_string.

    rv_result = lv_string.
  ENDMETHOD.

  METHOD assert_type.
    SPLIT iv_string AT |\{| INTO TABLE DATA(lt_flakes).

    DATA(lv_string) = lt_flakes[ 1 ].

    CONDENSE lv_string.

    IF lv_string IS NOT INITIAL.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_directive.

  ENDMETHOD.

  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
