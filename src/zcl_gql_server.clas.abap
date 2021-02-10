CLASS zcl_gql_server DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_response,
             type TYPE string,
             msg  TYPE string,
           END OF ts_response.

    DATA: ms_response TYPE ts_response.
ENDCLASS.



CLASS zcl_gql_server IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    CASE server->request->get_content_type( ).
      WHEN if_rest_media_type=>gc_appl_json.
*      data(lv_request_data) = server->request->get_cdata( ).

*      try.
*      /ui2/cl_json=>deserialize(
*        EXPORTING
*          json             = lv_request_data                 " JSON string
**          jsonx            =                  " JSON XString
*          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names
**          assoc_arrays     =                  " Deserialize associative array as tables with unique keys
**          assoc_arrays_opt =                  " Optimize rendering of name value maps
**          name_mappings    =                  " ABAP<->JSON Name Mapping Table
**          conversion_exits =                  " Use DDIC conversion exits on deserialize of values
*        CHANGING
*          data             = ms_response                 " Data to serialize
*      ).
*      catch cx_root into data(lx_error).
*      ENDTRY.
        DATA(lv_request_method) = server->request->get_method( ).

        DATA(lv_query_string) = server->request->get_header_field( name = '~query_string' ).

        CASE lv_request_method.
          WHEN 'POST'.
            ms_response-msg = lv_query_string.
          WHEN 'GET'.
            ms_response-msg = lv_query_string.
        ENDCASE.

        ms_response-type = lv_request_method.


        server->response->set_status(
          code   = cl_rest_status_code=>gc_success_ok
          reason = 'OK'
        ).

        server->response->set_content_type( if_rest_media_type=>gc_appl_json ).

        server->response->set_cdata( /ui2/cl_json=>serialize(
                                       data             = ms_response
*                                     compress         =
*                                     name             =
                                       pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*                                     type_descr       =
*                                     assoc_arrays     =
*                                     ts_as_iso8601    =
*                                     expand_includes  =
*                                     assoc_arrays_opt =
*                                     numc_as_string   =
*                                     name_mappings    =
*                                     conversion_exits =
                                     ) ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
