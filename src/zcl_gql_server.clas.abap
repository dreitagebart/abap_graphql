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
                 save_schema TYPE string VALUE '/saveSchema',
                 load_schema TYPE string VALUE '/loadSchema',
               END OF mc_operations.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_response,
             data TYPE string,
           END OF ts_response.

    DATA: mx_error     TYPE REF TO zcx_gql_error,
          mo_server    TYPE REF TO if_http_server,
          mo_json      TYPE REF TO data,
          mv_operation TYPE string,
          mv_query     TYPE string,
          mv_variables TYPE string,
          mv_response  TYPE string.

    METHODS:
      allow_cors,
      assert_operation
        RETURNING VALUE(rv_result) TYPE string,
      get_operation_value
        RETURNING VALUE(rv_result) TYPE string,
      get_query_string
        RETURNING VALUE(rv_result) TYPE string,
      get_method
        RETURNING VALUE(rv_result) TYPE string,
      get_content_type
        RETURNING VALUE(rv_result) TYPE string,
      get_json_field
        IMPORTING
                  iv_field         TYPE string
        RETURNING VALUE(rv_result) TYPE string
        RAISING   zcx_gql_error,
      handle_graphql
        RAISING zcx_gql_error,
      handle_operation,
      handle_options,
      handle_get,
      handle_post
        RAISING zcx_gql_error,
      handle_response,
      read_json,
      load_schema,
      read_graphql_data
        RAISING zcx_gql_error,
      save_schema.
ENDCLASS.



CLASS zcl_gql_server IMPLEMENTATION.
  METHOD load_schema.
    SELECT SINGLE data FROM zgql_schema INTO @DATA(lv_data) WHERE schema_name = 'DEFAULT'.
    IF sy-subrc = 0.
      mv_response = lv_data.
    ENDIF.
  ENDMETHOD.

  METHOD save_schema.
    DATA ls_db TYPE zgql_schema.

    FIELD-SYMBOLS: <schema> TYPE REF TO data.

    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).

    ASSIGN COMPONENT 'SCHEMA' OF STRUCTURE <json> TO <schema>.

    ASSIGN <schema>->* TO FIELD-SYMBOL(<data>).

    ls_db-client      = sy-mandt.
    ls_db-schema_name = 'DEFAULT'.
    ls_db-data        = <data>.

    MODIFY zgql_schema FROM ls_db.
    IF sy-subrc = 0.
      mv_response = 'SUCCESS'.
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
          RAISE EXCEPTION lx_error.
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

  METHOD read_graphql_data.
    TRY.
        mv_operation = get_json_field( mc_gql_fields-operation_name ).
        mv_query = get_json_field( mc_gql_fields-query ).
*        mt_variables = get_json_field( mc_gql_fields-variables )->*.
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
      WHEN mc_operations-save_schema.
        save_schema( ).
      WHEN mc_operations-load_schema.
        load_schema( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_options.
    mv_response = 'Ok'.
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

  METHOD handle_response.
    IF mx_error IS NOT BOUND.
      allow_cors( ).

      mo_server->response->set_status(
        code   = cl_rest_status_code=>gc_success_ok
        reason = 'OK'
      ).

      mo_server->response->set_content_type( if_rest_media_type=>gc_appl_json ).

      mo_server->response->set_cdata( /ui2/cl_json=>serialize(
                                        data             = VALUE ts_response( data = mv_response )
                                        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*                                       type_descr       =
*                                       assoc_arrays     =
*                                       ts_as_iso8601    =
*                                       expand_includes  =
*                                       assoc_arrays_opt =
*                                       numc_as_string   =
*                                       name_mappings    =
*                                       conversion_exits =
                                      ) ).
    ELSE.
      mo_server->response->set_status(
        code   = cl_rest_status_code=>gc_client_error_bad_request
        reason = mx_error->get_text( )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD if_http_extension~handle_request.
    mo_server = server.

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

    handle_response( ).
  ENDMETHOD.
ENDCLASS.
