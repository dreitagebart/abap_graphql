CLASS zcl_gql_server DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_http_extension.

    CONSTANTS: BEGIN OF mc_methods,
                 get     TYPE string VALUE 'GET',
                 post    TYPE string VALUE 'POST',
                 options TYPE string VALUE 'OPTIONS',
               END OF mc_methods,

               BEGIN OF mc_gql_fields,
                 operation_name TYPE string VALUE 'OPERATION_NAME',
                 query          TYPE string VALUE 'QUERY',
                 variables      TYPE string VALUE 'VARIABLES',
               END OF mc_gql_fields,

               BEGIN OF mc_content_types,
                 application_json TYPE string VALUE 'application/json',
               END OF mc_content_types,

               BEGIN OF mc_operations,
                 search_function TYPE string VALUE '/searchFunction',
                 search_class    TYPE string VALUE '/searchClass',
                 import_function TYPE string VALUE '/importFunction',
                 import_class    TYPE string VALUE '/importClass',
                 save_schema     TYPE string VALUE '/saveSchema',
                 load_schema     TYPE string VALUE '/loadSchema',
               END OF mc_operations.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_response,
             data TYPE string,
           END OF ts_response.

    DATA: mo_class     TYPE REF TO cl_abap_classdescr,
          mo_schema    TYPE REF TO zcl_gql_schema_generator,
          mo_server    TYPE REF TO if_http_server,
          mo_json      TYPE REF TO data,
          mif_response TYPE REF TO zif_gql_response,
          mv_operation TYPE string.

    METHODS:
      allow_cors,
      assert_operation
        RETURNING VALUE(rv_result) TYPE string,
      get_endpoint
        RETURNING VALUE(rv_result) TYPE string,
      get_operation_value
        RETURNING VALUE(rv_result) TYPE string,
      get_query_string
        RETURNING VALUE(rv_result) TYPE string,
      get_method
        RETURNING VALUE(rv_result) TYPE string,
      get_content_type
        RETURNING VALUE(rv_result) TYPE string,
      get_json_field_ref
        IMPORTING
                  iv_field         TYPE string
        RETURNING VALUE(rr_result) TYPE REF TO data
        RAISING   zcx_gql_error,
      get_json_field
        IMPORTING
                  iv_field         TYPE string
        RETURNING VALUE(rv_result) TYPE string
        RAISING   zcx_gql_error,
      handle_graphql,
      handle_operation,
      handle_options,
      handle_get,
      handle_post,
      handle_response,
      handle_request,
      read_json,
      load_schema,
      read_graphql_data
        RAISING zcx_gql_error,
      import_function
        RAISING zcx_gql_error,
      import_class
        RAISING zcx_gql_error,
      set_input_type
        IMPORTING
                  iv_name  TYPE abap_compname
                  iv_type  TYPE abap_typekind
                  io_field TYPE REF TO zcl_gql_schema_field
        RAISING   zcx_gql_error,
      set_result_type
        IMPORTING
                  iv_method TYPE abap_methname
                  iv_name   TYPE abap_compname
                  iv_type   TYPE abap_typekind
                  io_result TYPE REF TO zcl_gql_schema_result
        RAISING   zcx_gql_error,
      save_schema,
      search_class,
      search_function.
ENDCLASS.



CLASS zcl_gql_server IMPLEMENTATION.
  METHOD load_schema.
    DATA(lv_endpoint) = get_endpoint( ).

    SELECT schema_type,
           type_name,
           data FROM zgql_schema INTO TABLE @DATA(lt_schema) WHERE endpoint = @lv_endpoint.

    DATA(lo_generator) = NEW zcl_gql_schema_generator( ).

    LOOP AT lt_schema REFERENCE INTO DATA(lr_schema).
      CASE lr_schema->schema_type.
        WHEN 'T'.
          DATA(lo_type) = CAST zcl_gql_schema_type( zcl_gql_schema_utils=>deserialize( zcl_gql_schema_utils=>uncompress( lr_schema->data ) ) ).

          lo_generator->add_type( lo_type ).
        WHEN 'Q'.
          DATA(lo_query) = CAST zcl_gql_schema_query( zcl_gql_schema_utils=>deserialize( zcl_gql_schema_utils=>uncompress( lr_schema->data ) ) ).

          lo_generator->add_query( lo_query ).
        WHEN 'M'.
          DATA(lo_mutation) = CAST zcl_gql_schema_mutation( zcl_gql_schema_utils=>deserialize( zcl_gql_schema_utils=>uncompress( lr_schema->data ) ) ).

          lo_generator->add_mutation( lo_mutation ).
      ENDCASE.
    ENDLOOP.

    TRY.
        lo_generator->generate(
          IMPORTING
            ev_types    = DATA(lv_types)
            ev_query    = DATA(lv_query)
            ev_mutation = DATA(lv_mutation)
        ).
      CATCH zcx_gql_error INTO DATA(lx_error).
        mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    mif_response = NEW zcl_gql_response_schema_sdl(
      iv_types    = lv_types
      iv_query    = lv_query
      iv_mutation = lv_mutation
    ).
  ENDMETHOD.

  METHOD import_function.
    TRY.
        DATA(lv_object) = get_json_field( 'OBJECT' ).
      CATCH zcx_gql_error INTO DATA(lx_error).
    ENDTRY.

    TRANSLATE lv_object TO UPPER CASE.

    SELECT SINGLE funcname FROM tfdir INTO @DATA(lv_function) WHERE funcname = @lv_object.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gql_error
        MESSAGE e001 WITH lv_object.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD import_class.
    TRY.
        DATA(lv_class) = get_json_field( 'OBJECT' ).
      CATCH zcx_gql_error INTO DATA(lx_error).
        mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    TRANSLATE lv_class TO UPPER CASE.

    SELECT SINGLE clsname FROM seoclass INTO @DATA(lv_classname) WHERE clsname = @lv_class.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gql_error
        MESSAGE e002 WITH lv_class.
    ENDIF.

    mo_class = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_name( lv_class ) ).

    TRY.
        DATA(lr_target) = REF #( mo_class->interfaces[ name = 'ZIF_GQL_RESOLVER' ] ).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_gql_error
          MESSAGE e003 WITH 'ZIF_GQL_RESOLVER'.
    ENDTRY.

    DATA lif_instance TYPE REF TO zif_gql_resolver.

    CREATE OBJECT lif_instance TYPE (lv_class).

    DATA(lv_name) = lif_instance->get_name( ).

    mo_schema = NEW zcl_gql_schema_generator( ).

    LOOP AT lif_instance->get_methods( ) REFERENCE INTO DATA(lr_method).
      TRY.
          DATA(lr_meth) = REF #( mo_class->methods[ name = lr_method->name ] ).

          CASE lr_method->type.
            WHEN zif_gql_resolver=>mc_types-query.
              DATA(lo_query) = mo_schema->query( zcl_gql_schema_utils=>camel_case( lr_method->name ) ).

              lo_query->directive( lv_class ).

              LOOP AT lr_meth->parameters REFERENCE INTO DATA(lr_param) WHERE parm_kind = cl_abap_classdescr=>importing.
                DATA(lo_field) = lo_query->field( zcl_gql_schema_utils=>camel_case( CONV #( lr_param->name ) ) ).

                IF lr_param->is_optional = abap_false.
                  lo_field->required( ).
                ENDIF.

                TRY.
                    set_input_type(
                      iv_name  = lr_param->name
                      iv_type  = lr_param->type_kind
                      io_field = lo_field
                    ).
                  CATCH zcx_gql_error INTO lx_error.
                    RAISE EXCEPTION lx_error.
                ENDTRY.
              ENDLOOP.

              LOOP AT lr_meth->parameters REFERENCE INTO lr_param WHERE parm_kind = cl_abap_classdescr=>returning.
                DATA(lo_result) = lo_query->result( zcl_gql_schema_utils=>upper_camel_case( CONV #( lr_param->name ) ) ).

                TRY.
                    set_result_type(
                      iv_method = lr_meth->name
                      iv_name   = lr_param->name
                      iv_type   = lr_param->type_kind
                      io_result = lo_result
                    ).
                  CATCH zcx_gql_error INTO lx_error.
                    mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
                    RETURN.
                ENDTRY.
              ENDLOOP.
            WHEN zif_gql_resolver=>mc_types-mutation.
              DATA(lo_mutation) = mo_schema->mutation( zcl_gql_schema_utils=>camel_case( lr_method->name ) ).

              lo_mutation->directive( lv_class ).

              LOOP AT lr_meth->parameters REFERENCE INTO lr_param WHERE parm_kind = cl_abap_classdescr=>importing.
                lo_field = lo_mutation->field( zcl_gql_schema_utils=>camel_case( CONV #( lr_param->name ) ) ).

                IF lr_param->is_optional = abap_false.
                  lo_field->required( ).
                ENDIF.

                TRY.
                    set_input_type(
                      iv_name = lr_param->name
                      iv_type = lr_param->type_kind
                      io_field = lo_field
                    ).
                  CATCH zcx_gql_error INTO lx_error.
                    mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
                    RETURN.
                ENDTRY.
              ENDLOOP.

              LOOP AT lr_meth->parameters REFERENCE INTO lr_param WHERE parm_kind = cl_abap_classdescr=>returning.
                lo_result = lo_mutation->result( zcl_gql_schema_utils=>upper_camel_case( CONV #( lr_param->name ) ) ).

                TRY.
                    set_result_type(
                      iv_method = lr_meth->name
                      iv_name   = lr_param->name
                      iv_type   = lr_param->type_kind
                      io_result = lo_result
                    ).
                  CATCH zcx_gql_error INTO lx_error.
                    mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
                    RETURN.
                ENDTRY.
              ENDLOOP.
          ENDCASE.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.

    TRY.
        mo_schema->generate(
          IMPORTING
            ev_types   = DATA(lv_types)
            ev_query    = DATA(lv_query)
            ev_mutation = DATA(lv_mutation)
        ).
      CATCH zcx_gql_error INTO lx_error.
        mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    mif_response = NEW zcl_gql_response_schema_sdl(
                     iv_types    = lv_types
                     iv_query    = lv_query
                     iv_mutation = lv_mutation
                   ).
  ENDMETHOD.

  METHOD set_result_type.
    DATA lv_type_name TYPE string.

    CASE iv_type.
      WHEN cl_abap_classdescr=>typekind_int.
        io_result->int( ).
      WHEN cl_abap_classdescr=>typekind_int1.
        io_result->int( ).
      WHEN cl_abap_classdescr=>typekind_int2.
        io_result->int( ).
      WHEN cl_abap_classdescr=>typekind_int8.
        io_result->int( ).
      WHEN cl_abap_classdescr=>typekind_char.
        io_result->string( ).
      WHEN cl_abap_classdescr=>typekind_clike.
        io_result->string( ).
      WHEN cl_abap_classdescr=>typekind_csequence.
        io_result->string( ).
      WHEN cl_abap_classdescr=>typekind_decfloat.
        io_result->float( ).
      WHEN cl_abap_classdescr=>typekind_decfloat16.
        io_result->float( ).
      WHEN cl_abap_classdescr=>typekind_decfloat34.
        io_result->float( ).
      WHEN cl_abap_classdescr=>typekind_float.
        io_result->float( ).
      WHEN cl_abap_classdescr=>typekind_struct1.
        lv_type_name = zcl_gql_schema_utils=>upper_camel_case( iv_method && |_| && iv_name ).
        DATA(lo_type) = mo_schema->type( lv_type_name ).

        DATA(lo_struc) = CAST cl_abap_structdescr( mo_class->get_method_parameter_type(
                                                     p_method_name       = iv_method
                                                     p_parameter_name    = iv_name
                                                 ) ).

        LOOP AT lo_struc->components REFERENCE INTO DATA(lr_component).
          DATA(lo_field) = lo_type->field( zcl_gql_schema_utils=>camel_case( CONV #( lr_component->name ) ) ).

          TRY.
              set_input_type(
                iv_name  = lr_component->name
                iv_type  = lr_component->type_kind
                io_field = lo_field
              ).
            CATCH zcx_gql_error INTO DATA(lx_error).
              mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
              RETURN.
          ENDTRY.
        ENDLOOP.

        io_result->type( lv_type_name ).
      WHEN cl_abap_classdescr=>typekind_struct2.
        lv_type_name = zcl_gql_schema_utils=>upper_camel_case( iv_method && |_| && iv_name ).

        lo_type = mo_schema->type( lv_type_name ).

        lo_struc = CAST cl_abap_structdescr( mo_class->get_method_parameter_type(
                                               p_method_name       = iv_method
                                               p_parameter_name    = iv_name
                                             ) ).

        LOOP AT lo_struc->components REFERENCE INTO lr_component.
          lo_field = lo_type->field( zcl_gql_schema_utils=>camel_case( CONV #( lr_component->name ) ) ).

          TRY.
              set_input_type(
                iv_name  = lr_component->name
                iv_type  = lr_component->type_kind
                io_field = lo_field
              ).
            CATCH zcx_gql_error INTO lx_error.
              mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
              RETURN.
          ENDTRY.
        ENDLOOP.

        io_result->type( lv_type_name ).
      WHEN cl_abap_classdescr=>typekind_table.
        lv_type_name = zcl_gql_schema_utils=>upper_camel_case( iv_method && |_| && iv_name ).

        io_result->list( ).

        lo_type = mo_schema->type( lv_type_name ).

        DATA(lo_table) = CAST cl_abap_tabledescr( mo_class->get_method_parameter_type(
                                                    p_method_name    = iv_method
                                                    p_parameter_name = iv_name
                                                  ) ).

        lo_struc = CAST cl_abap_structdescr( lo_table->get_table_line_type( ) ).

        LOOP AT lo_struc->components REFERENCE INTO lr_component.
          lo_field = lo_type->field( zcl_gql_schema_utils=>camel_case( CONV #( lr_component->name ) ) ).

          TRY.
              set_input_type(
                iv_name  = lr_component->name
                iv_type  = lr_component->type_kind
                io_field = lo_field
              ).
            CATCH zcx_gql_error INTO lx_error.
              mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
              RETURN.
          ENDTRY.
        ENDLOOP.

        io_result->type( lv_type_name ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_gql_error
          MESSAGE e004 WITH iv_name.
    ENDCASE.

    io_result->required( ).
  ENDMETHOD.

  METHOD set_input_type.
    CASE iv_type.
      WHEN cl_abap_classdescr=>typekind_int.
        io_field->int( ).
      WHEN cl_abap_classdescr=>typekind_int1.
        io_field->int( ).
      WHEN cl_abap_classdescr=>typekind_int2.
        io_field->int( ).
      WHEN cl_abap_classdescr=>typekind_int8.
        io_field->int( ).
      WHEN cl_abap_classdescr=>typekind_char.
        io_field->string( ).
      WHEN cl_abap_classdescr=>typekind_clike.
        io_field->string( ).
      WHEN cl_abap_classdescr=>typekind_csequence.
        io_field->string( ).
      WHEN cl_abap_classdescr=>typekind_decfloat.
        io_field->float( ).
      WHEN cl_abap_classdescr=>typekind_decfloat16.
        io_field->float( ).
      WHEN cl_abap_classdescr=>typekind_decfloat34.
        io_field->float( ).
      WHEN cl_abap_classdescr=>typekind_float.
        io_field->float( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_gql_error
          MESSAGE e004 WITH iv_name.
    ENDCASE.
  ENDMETHOD.

  METHOD search_class.
    DATA lt_objects TYPE zcl_gql_response_search_object=>tt_object.

    TRY.
        DATA(lv_search) = get_json_field( 'SEARCH' ).
      CATCH zcx_gql_error INTO DATA(lx_error).
        mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    TRANSLATE lv_search TO UPPER CASE.

    DATA lt_search TYPE RANGE OF seoclsname.

    APPEND VALUE #( sign   = 'I'
                    option = 'CP'
                    low    = |*| && lv_search && |*| ) TO lt_search.

    SELECT clsname, descript FROM vseoclass
      INTO TABLE @lt_objects UP TO 50 ROWS
      WHERE clsname IN @lt_search
        AND langu = @sy-langu.

    mif_response = NEW zcl_gql_response_search_object( lt_objects ).
  ENDMETHOD.

  METHOD search_function.
    DATA lt_objects TYPE zcl_gql_response_search_object=>tt_object.

    TRY.
        DATA(lv_search) = get_json_field( 'SEARCH' ).
      CATCH zcx_gql_error INTO DATA(lx_error).
        mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    TRANSLATE lv_search TO UPPER CASE.

    DATA lt_search TYPE RANGE OF rs38l_fnam.

    APPEND VALUE #( sign   = 'I'
                    option = 'CP'
                    low    = |*| && lv_search && |*| ) TO lt_search.

    SELECT tfdir~funcname, tftit~stext FROM tfdir
      LEFT OUTER JOIN tftit ON tfdir~funcname = tftit~funcname
                           AND tftit~spras = @sy-langu
      UP TO 50 ROWS
      INTO TABLE @lt_objects WHERE tfdir~funcname IN @lt_search.

    mif_response = NEW zcl_gql_response_search_object( lt_objects ).
  ENDMETHOD.

  METHOD save_schema.
    DATA: lv_component TYPE string,
          lt_db        TYPE TABLE OF zgql_schema.

    FIELD-SYMBOLS: <schema> TYPE REF TO data,
                   <data>   TYPE string.

    DATA(lv_endpoint) = get_endpoint( ).

    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).

    ASSIGN COMPONENT 'TYPES' OF STRUCTURE <json> TO <schema>.

    ASSIGN <schema>->* TO <data>.

    IF <data> IS ASSIGNED.
      TRY.
          LOOP AT zcl_gql_sdl_parser=>parse_types( zcl_gql_schema_utils=>clean_string( <data> ) ) REFERENCE INTO DATA(lr_type).
            APPEND INITIAL LINE TO lt_db REFERENCE INTO DATA(lr_db).
            lr_db->client      = sy-mandt.
            lr_db->endpoint    = lv_endpoint.
            lr_db->type_name   = zcl_gql_schema_generator=>get_type_name( lr_type->* ).
            lr_db->schema_type = 'T'.
            lr_db->data        = zcl_gql_schema_utils=>compress( zcl_gql_schema_utils=>serialize( lr_type->* ) ).
          ENDLOOP.
        CATCH zcx_gql_parser_error.
      ENDTRY.

      UNASSIGN <data>.
    ENDIF.

    ASSIGN COMPONENT 'QUERY' OF STRUCTURE <json> TO <schema>.

    ASSIGN <schema>->* TO <data>.

    IF <data> IS ASSIGNED.
      TRY.
          LOOP AT zcl_gql_sdl_parser=>parse_queries( zcl_gql_schema_utils=>clean_string( <data> ) ) REFERENCE INTO DATA(lr_query).
            APPEND INITIAL LINE TO lt_db REFERENCE INTO lr_db.
            lr_db->client      = sy-mandt.
            lr_db->endpoint    = lv_endpoint.
            lr_db->type_name   = zcl_gql_schema_generator=>get_query_name( lr_query->* ).
            lr_db->schema_type = 'Q'.
            lr_db->data        = zcl_gql_schema_utils=>compress( zcl_gql_schema_utils=>serialize( lr_query->* ) ).
          ENDLOOP.
        CATCH zcx_gql_parser_error.
      ENDTRY.

      UNASSIGN <data>.
    ENDIF.

    ASSIGN COMPONENT 'MUTATION' OF STRUCTURE <json> TO <schema>.

    ASSIGN <schema>->* TO <data>.

    IF <data> IS ASSIGNED.
      TRY.
          LOOP AT zcl_gql_sdl_parser=>parse_mutations( zcl_gql_schema_utils=>clean_string( <data> ) ) REFERENCE INTO DATA(lr_mutation).
            APPEND INITIAL LINE TO lt_db REFERENCE INTO lr_db.
            lr_db->client      = sy-mandt.
            lr_db->endpoint    = lv_endpoint.
            lr_db->type_name   = zcl_gql_schema_generator=>get_mutation_name( lr_mutation->* ).
            lr_db->schema_type = 'M'.
            lr_db->data        = zcl_gql_schema_utils=>compress( zcl_gql_schema_utils=>serialize( lr_mutation->* ) ).
          ENDLOOP.
        CATCH zcx_gql_parser_error.
      ENDTRY.
    ENDIF.

    MODIFY zgql_schema FROM TABLE lt_db.
    IF sy-subrc = 0.
      mif_response = NEW zcl_gql_response_200( 'SUCCESS' ).
    ENDIF.
  ENDMETHOD.

  METHOD allow_cors.
    DATA: lt_response_header TYPE tihttpnvp.

    mo_server->response->get_header_fields(
      CHANGING
        fields = lt_response_header
    ).

    APPEND VALUE #( name  = 'Access-Control-Allow-Origin'
                    value = '*' ) TO lt_response_header.

    APPEND VALUE #( name  = 'Access-Control-Allow-Headers'
                    value = 'Content-Type' ) TO lt_response_header.

    mo_server->response->set_header_fields( lt_response_header ).
  ENDMETHOD.

  METHOD assert_operation.
    rv_result = abap_true.

    mv_operation = mo_server->request->get_header_field( '~path_info' ).

    CASE mv_operation.
      WHEN mc_operations-import_class.
      WHEN mc_operations-import_function.
      WHEN mc_operations-search_class.
      WHEN mc_operations-search_function.
      WHEN mc_operations-load_schema.
      WHEN mc_operations-save_schema.
      WHEN OTHERS.
        CLEAR mv_operation.
        rv_result = abap_false.
    ENDCASE.

*    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).
*
*    DATA(lo_type) = cl_abap_typedescr=>describe_by_data_ref( mo_json ).
*
*    CASE lo_type->kind.
*      WHEN lo_type->kind_struct.
*        DATA(lo_structure) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( mo_json ) ).
*
*        DATA(lt_components) = lo_structure->get_components( ).
*
*        IF line_exists( lt_components[ name = 'OPERATION' ] ).
*          rv_result = abap_true.
*        ENDIF.
*    ENDCASE.
  ENDMETHOD.

  METHOD handle_post.
    read_json( ).

    IF assert_operation( ) = abap_true.
      handle_operation( ).
    ELSE.
      TRY.
          handle_graphql( ).
        CATCH zcx_gql_error INTO DATA(lx_error).
          mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD get_json_field.
    FIELD-SYMBOLS: <ref>    TYPE REF TO data,
                   <string> TYPE string.

    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).

    ASSIGN COMPONENT iv_field OF STRUCTURE <json> TO <ref>.

    IF <ref> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_gql_error
        MESSAGE e000 WITH iv_field.
    ENDIF.

    ASSIGN <ref>->* TO <string>.

    CHECK <string> IS ASSIGNED.

    rv_result = <string>.
  ENDMETHOD.

  METHOD get_json_field_ref.
    FIELD-SYMBOLS: <ref>    TYPE REF TO data,
                   <string> TYPE string.

    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).

    ASSIGN COMPONENT iv_field OF STRUCTURE <json> TO <ref>.

    IF <ref> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_gql_error
        MESSAGE e000 WITH iv_field.
    ENDIF.

    rr_result = <ref>.
  ENDMETHOD.

  METHOD read_graphql_data.
    TRY.
        DATA(lv_operation_name) = get_json_field( mc_gql_fields-operation_name ).
        DATA(lv_query) = get_json_field( mc_gql_fields-query ).
        DATA(lr_variables) = get_json_field_ref( mc_gql_fields-variables ).

        DATA(lo_parser) = NEW zcl_gql_query_parser(
          iv_operation_name = lv_operation_name
          iv_query          = lv_query
          ir_variables      = lr_variables
        ).

        mif_response = lo_parser->execute( ).
      CATCH zcx_gql_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_graphql.
    TRY.
        read_graphql_data( ).
      CATCH zcx_gql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_operation.
    CASE mv_operation.
      WHEN mc_operations-import_class.
        TRY.
            import_class( ).
          CATCH zcx_gql_error INTO DATA(lx_error).
            mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        ENDTRY.
      WHEN mc_operations-import_function.
        TRY.
            import_function( ).
          CATCH zcx_gql_error INTO lx_error.
            mif_response = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        ENDTRY.
      WHEN mc_operations-search_class.
        search_class( ).
      WHEN mc_operations-search_function.
        search_function( ).
      WHEN mc_operations-save_schema.
        save_schema( ).
      WHEN mc_operations-load_schema.
        load_schema( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_options.
    mif_response = NEW zcl_gql_response_200( 'OK' ).
  ENDMETHOD.

  METHOD handle_get.
    read_json( ).
  ENDMETHOD.

  METHOD read_json.
    mo_json = /ui2/cl_json=>generate(
                json        = mo_server->request->get_cdata( )
                pretty_name = /ui2/cl_json=>pretty_mode-camel_case
              ).
  ENDMETHOD.

  METHOD get_endpoint.
*    DATA lt_header TYPE tihttpnvp.
*
*    mo_server->request->get_header_fields(
*      CHANGING
*        fields = lt_header                 " Header fields
*    ).

    rv_result = mo_server->request->get_header_field( name = '~script_name' ).
  ENDMETHOD.

  METHOD get_query_string.
    rv_result = mo_server->request->get_header_field( name = '~query_string' ).
  ENDMETHOD.

  METHOD get_method.
    rv_result = mo_server->request->get_method( ).
  ENDMETHOD.

  METHOD get_operation_value.
    FIELD-SYMBOLS: <operation> TYPE REF TO data,
                   <string>    TYPE string.

    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).

    CHECK <json> IS ASSIGNED.

    ASSIGN COMPONENT 'OPERATION' OF STRUCTURE <json> TO <operation>.

    CHECK <operation> IS ASSIGNED.

    ASSIGN <operation>->* TO <string>.

    rv_result = <string>.
  ENDMETHOD.

  METHOD get_content_type.
    rv_result = mo_server->request->get_content_type( ).
  ENDMETHOD.

  METHOD handle_request.
    CASE get_content_type( ).
      WHEN mc_content_types-application_json.
        CASE get_method( ).
          WHEN mc_methods-get.
            handle_get( ).
          WHEN mc_methods-post.
            TRY.
                handle_post( ).
              CATCH zcx_gql_error.
            ENDTRY.
          WHEN OTHERS.
        ENDCASE.
      WHEN OTHERS.
        CASE get_method( ).
          WHEN mc_methods-options.
            handle_options( ).
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_response.
    allow_cors( ).

    mo_server->response->set_content_type( if_rest_media_type=>gc_appl_json ).

    IF mif_response IS BOUND.
      IF mif_response->is_error( ) = abap_true.
        mo_server->response->set_status(
          code = cl_rest_status_code=>gc_client_error_bad_request
          reason = mif_response->get_json( )
        ).

        RETURN.
      ENDIF.

      mo_server->response->set_status(
        code   = cl_rest_status_code=>gc_success_ok
        reason = 'OK'
      ).

      mo_server->response->set_cdata( mif_response->get_json( ) ).
    ELSE.
      mif_response = NEW zcl_gql_response_400( 'Unhandled request' ).

      mo_server->response->set_status(
        code = cl_rest_status_code=>gc_client_error_bad_request
        reason = mif_response->get_json( )
      ).
    ENDIF.

*    IF mx_error IS NOT BOUND.
*      mo_server->response->set_status(
*        code   = cl_rest_status_code=>gc_success_ok
*        reason = 'OK'
*      ).
*
*      mo_server->response->set_content_type( if_rest_media_type=>gc_appl_json ).
*
*      mo_server->response->set_cdata( /ui2/cl_json=>serialize(
**                                        expand_includes = abap_true
*                                        data             = mif_response->get_data( )
*                                        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*                                        name_mappings    = mif_response->get_mapping( )
*                                      ) ).
*    ELSE.
*      mo_server->response->set_status(
*        code   = cl_rest_status_code=>gc_client_error_bad_request
*        reason = mx_error->get_text( )
*      ).
*
*      mo_server->response->set_content_type( if_rest_media_type=>gc_appl_json ).
*
*      mo_server->response->set_cdata( /ui2/cl_json=>serialize(
*                                        data = NEW zcl_gql_response_400( mx_error->get_text( ) )
*                                        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                                      ) ).
*    ENDIF.
  ENDMETHOD.

  METHOD if_http_extension~handle_request.
    mo_server = server.

    handle_request( ).
    handle_response( ).
  ENDMETHOD.
ENDCLASS.
