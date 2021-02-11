CLASS zcl_gql_server DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_http_extension,
      if_rest_media_type,
      if_rest_message.

    ALIASES:
      mc_method_post FOR if_rest_message~gc_method_post,
      mc_method_get FOR if_rest_message~gc_method_get,
      mc_json FOR if_rest_media_type~gc_appl_json.

    CONSTANTS: BEGIN OF mc_operations,
                 save_schema TYPE string VALUE 'saveSchema',
                 load_schema TYPE string VALUE 'loadSchema',
               END OF mc_operations.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_response,
             data TYPE string,
           END OF ts_response.

    DATA: mo_server   TYPE REF TO if_http_server,
          mo_json     TYPE REF TO data,
          mv_response TYPE string.

    METHODS:
      assert_operation
        RETURNING VALUE(rv_result) TYPE abap_bool,
      get_operation_value
        RETURNING VALUE(rv_result) TYPE string,
      get_query_string
        RETURNING VALUE(rv_result) TYPE string,
      get_method
        RETURNING VALUE(rv_result) TYPE string,
      get_content_type
        RETURNING VALUE(rv_result) TYPE string,
      handle_operation,
      handle_get,
      handle_post,
      handle_response,
      read_json,
      load_schema,
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

  METHOD assert_operation.
    rv_result = abap_false.

    ASSIGN mo_json->* TO FIELD-SYMBOL(<json>).

    DATA(lo_type) = cl_abap_typedescr=>describe_by_data_ref( mo_json ).

    CASE lo_type->kind.
      WHEN lo_type->kind_struct.
        DATA(lo_structure) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( mo_json ) ).

        DATA(lt_components) = lo_structure->get_components( ).

        IF line_exists( lt_components[ name = 'OPERATION' ] ).
          rv_result = abap_true.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_post.
    read_json( ).

    IF assert_operation( ) = abap_true.
      handle_operation( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_operation.
    CASE get_operation_value( ).
      WHEN mc_operations-save_schema.
        save_schema( ).
      WHEN mc_operations-load_schema.
        load_schema( ).
    ENDCASE.
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
    IF mv_response IS NOT INITIAL.
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
        reason = 'Bad Request'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD if_http_extension~handle_request.
    mo_server = server.

    CASE get_content_type( ).
      WHEN mc_json.
        CASE get_method( ).
          WHEN mc_method_get.
            handle_get( ).
          WHEN mc_method_post.
            handle_post( ).
          WHEN OTHERS.
        ENDCASE.
    ENDCASE.

    handle_response( ).
  ENDMETHOD.
ENDCLASS.
