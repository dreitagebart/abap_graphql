CLASS zcl_gql_schema_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      clean_string
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE string,
      camel_case
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE string,
      upper_camel_case
        IMPORTING
                  iv_string        TYPE string
        RETURNING VALUE(rv_result) TYPE string,
      compress
        IMPORTING
                  iv_data          TYPE string
        RETURNING VALUE(rv_result) TYPE xstring,
      deserialize
        IMPORTING
                  iv_data          TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO object,
      serialize
        IMPORTING
                  io_object        TYPE REF TO object
        RETURNING VALUE(rv_result) TYPE string,
      uncompress
        IMPORTING
                  iv_data          TYPE xstring
        RETURNING VALUE(rv_result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_gql_schema_utils IMPLEMENTATION.
  METHOD clean_string.
    DATA(lv_string) = iv_string.

    REPLACE ALL OCCURRENCES OF |\n| IN lv_string WITH | |.
    REPLACE ALL OCCURRENCES OF |\r| IN lv_string WITH | |.
    REPLACE ALL OCCURRENCES OF |\t| IN lv_string WITH | |.

    rv_result = lv_string.
  ENDMETHOD.

  METHOD compress.
    cl_abap_gzip=>compress_text(
      EXPORTING
        text_in  = iv_data
      IMPORTING
        gzip_out = rv_result
    ).
  ENDMETHOD.

  METHOD uncompress.
    TRY.
        cl_abap_gzip=>decompress_text(
          EXPORTING
            gzip_in      = iv_data
          IMPORTING
            text_out     = rv_result
        ).
      CATCH cx_parameter_invalid_range. " Parameter with invalid value range
      CATCH cx_sy_buffer_overflow.      " System Exception: Buffer too Short
      CATCH cx_sy_conversion_codepage.  " System exception in character set conversion
      CATCH cx_sy_compression_error.    " System Exception: Compression Error
    ENDTRY.
  ENDMETHOD.

  METHOD serialize.
    CALL TRANSFORMATION id
      SOURCE model = io_object
      RESULT XML rv_result.
  ENDMETHOD.

  METHOD deserialize.
    CALL TRANSFORMATION id
      SOURCE XML iv_data
      RESULT model = ro_result.
  ENDMETHOD.

  METHOD camel_case.
    DATA(lv_name) = iv_string.

    rv_result = /ui2/cl_abap2json=>convert_to_camel_case( lv_name ).
  ENDMETHOD.

  METHOD upper_camel_case.
    DATA(lv_name) = iv_string.

    rv_result = /ui2/cl_abap2json=>convert_to_camel_case( lv_name ).

    DATA(lv_length) = strlen( rv_result ) - 1.

    DATA(lv_capital) = to_upper( rv_result(1) ).

    IF lv_length > 0.
      DATA(lv_rest) = rv_result+1(lv_length).

      rv_result = lv_capital && lv_rest.
    ELSE.
      rv_result = lv_capital.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
