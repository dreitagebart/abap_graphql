CLASS zcl_gql_server DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gql_server IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    server->response->set_cdata(
      EXPORTING
        data   = 'Hello'                 " Character data
*    offset = 0                " Offset into character data
*    length = -1               " Length of character data
    ).
  ENDMETHOD.
ENDCLASS.
