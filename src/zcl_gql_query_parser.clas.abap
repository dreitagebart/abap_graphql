CLASS zcl_gql_query_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_operation_name TYPE string
          iv_query          TYPE string
          ir_variables      TYPE REF TO data,
      execute
        RETURNING VALUE(ro_result) TYPE REF TO zif_gql_response.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_operation_name TYPE string,
          mv_query          TYPE string,
          mv_query_name     TYPE string,
          mv_query_type     TYPE char1,
          mr_variables      TYPE REF TO data,
          mo_type           TYPE REF TO zcl_gql_schema_type,
          mo_mutation       TYPE REF TO zcl_gql_schema_mutation,
          mo_query          TYPE REF TO zcl_gql_schema_query,
          mif_response      TYPE REF TO zif_gql_response.

    METHODS:
      determine_query_type,
      determine_query_name,
      load_data
        RAISING zcx_gql_error,
      load_type
        IMPORTING
                  io_object        TYPE REF TO object
        RETURNING VALUE(ro_result) TYPE REF TO zcl_gql_schema_type,
      resolve.

ENDCLASS.



CLASS zcl_gql_query_parser IMPLEMENTATION.
  METHOD constructor.
    mv_operation_name = iv_operation_name.
    mv_query = iv_query.
    mr_variables = ir_variables.
  ENDMETHOD.

  METHOD execute.
    determine_query_type( ).
    determine_query_name( ).

    TRY.
        load_data( ).
      CATCH zcx_gql_error INTO DATA(lx_error).
        ro_result = NEW zcl_gql_response_400( lx_error->get_text( ) ).
        RETURN.
    ENDTRY.

    resolve( ).

    ro_result = mif_response.
  ENDMETHOD.

  METHOD resolve.
    DATA: lv_directive TYPE string,
          lv_name      TYPE string,
          lt_params    TYPE abap_parmbind_tab,
          lo_param     TYPE REF TO data,
          lo_instance  TYPE REF TO object,
          lif_instance TYPE REF TO zif_gql_resolver.

    FIELD-SYMBOLS: <param>  TYPE any,
                   <source> TYPE REF TO data,
                   <target> TYPE any.

    CASE mv_query_type.
      WHEN zif_gql_resolver=>mc_types-query.
        lv_directive = mo_query->mv_directive.
        lv_name = mo_query->mv_name.
      WHEN zif_gql_resolver=>mc_types-mutation.
        lv_directive = mo_mutation->mv_directive.
        lv_name = mo_mutation->mv_name.
    ENDCASE.

    CREATE OBJECT lo_instance TYPE (lv_directive).
    CREATE OBJECT lif_instance TYPE (lv_directive).

    DATA(lo_class) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_name( lv_directive ) ).

    LOOP AT lif_instance->get_methods( ) REFERENCE INTO DATA(lr_meth) WHERE type = mv_query_type.
      IF zcl_gql_schema_utils=>camel_case( lr_meth->name ) = lv_name.
        TRY.
            DATA(lr_method) = REF #( lo_class->methods[ name = lr_meth->name ] ).

            LOOP AT lr_method->parameters REFERENCE INTO DATA(lr_param) WHERE parm_kind = cl_abap_classdescr=>importing.
              DATA(lo_descr) = lo_class->get_method_parameter_type(
                                 p_method_name       = lr_meth->name
                                 p_parameter_name    = lr_param->name
                               ).

              CREATE DATA lo_param TYPE HANDLE lo_descr.

              ASSIGN lo_param->* TO <target>.
              ASSIGN mr_variables->(lr_param->name) TO <source>.

              IF <source> IS ASSIGNED.
                ASSIGN <source>->* TO FIELD-SYMBOL(<s>).

                <target> = <s>.
              ENDIF.

              INSERT VALUE abap_parmbind(
                name  = lr_param->name
                kind  = cl_abap_classdescr=>exporting
                value = lo_param
              ) INTO TABLE lt_params.

              IF <source> IS ASSIGNED.
                UNASSIGN <source>.
              ENDIF.
            ENDLOOP.

            LOOP AT lr_method->parameters REFERENCE INTO lr_param WHERE parm_kind = cl_abap_classdescr=>returning.
              lo_descr = lo_class->get_method_parameter_type(
                           p_method_name       = lr_meth->name
                           p_parameter_name    = lr_param->name
                         ).

              CREATE DATA lo_param TYPE HANDLE lo_descr.

              INSERT VALUE abap_parmbind(
                name  = lr_param->name
                kind  = cl_abap_classdescr=>returning
                value = lo_param
              ) INTO TABLE lt_params.
            ENDLOOP.

            TRY.
                CALL METHOD lo_instance->(lr_meth->name)
                  PARAMETER-TABLE lt_params.
              CATCH cx_root INTO DATA(lx_error).
                DATA(lv_text) = lx_error->get_longtext( ).
                DATA(lv_msg) = lx_error->get_text( ).
            ENDTRY.

            mif_response = NEW zcl_gql_response_query(
                             iv_name = mv_query_name
                             io_data = lt_params[ kind = cl_abap_classdescr=>returning ]-value
                           ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD load_type.
    CASE mv_query_type.
      WHEN zif_gql_resolver=>mc_types-query.
        mo_query = CAST zcl_gql_schema_query( io_object ).

        CASE mo_query->mo_result->mv_type.
          WHEN zif_gql_schema=>mc_basic_types-boolean.
          WHEN zif_gql_schema=>mc_basic_types-float.
          WHEN zif_gql_schema=>mc_basic_types-int.
          WHEN zif_gql_schema=>mc_basic_types-string.
          WHEN OTHERS.
            SELECT SINGLE data INTO @DATA(lv_data) FROM zgql_schema WHERE schema_type = 'T'
                                                                      AND type_name   = @mo_query->mo_result->mv_type.

            IF sy-subrc = 0.
              ro_result = CAST zcl_gql_schema_type( zcl_gql_schema_utils=>deserialize( zcl_gql_schema_utils=>uncompress( lv_data ) ) ).
            ENDIF.
        ENDCASE.
      WHEN zif_gql_resolver=>mc_types-mutation.
        mo_mutation = CAST zcl_gql_schema_mutation( io_object ).

        CASE mo_mutation->mo_result->mv_type.
          WHEN zif_gql_schema=>mc_basic_types-boolean.
          WHEN zif_gql_schema=>mc_basic_types-float.
          WHEN zif_gql_schema=>mc_basic_types-int.
          WHEN zif_gql_schema=>mc_basic_types-string.
          WHEN OTHERS.
            SELECT SINGLE data INTO @lv_data FROM zgql_schema WHERE schema_type = 'T'
                                                                AND type_name   = @mo_mutation->mo_result->mv_type.

            IF sy-subrc = 0.
              ro_result = CAST zcl_gql_schema_type( zcl_gql_schema_utils=>deserialize( zcl_gql_schema_utils=>uncompress( lv_data ) ) ).
            ENDIF.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD load_data.
    DATA: lo_value TYPE REF TO cl_abap_datadescr,
          lo_data  TYPE REF TO data.

    SELECT SINGLE data INTO @DATA(lv_data) FROM zgql_schema WHERE schema_type = @mv_query_type
                                                              AND type_name   = @mv_query_name.

    IF sy-subrc = 0.
      DATA(lo_object) = zcl_gql_schema_utils=>deserialize( zcl_gql_schema_utils=>uncompress( lv_data ) ).

      DATA(lo_type) = load_type( lo_object ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_gql_error
        MESSAGE e009.
    ENDIF.
  ENDMETHOD.

  METHOD determine_query_name.
    SPLIT mv_query AT |\{| INTO TABLE DATA(lt_flakes).

    DELETE lt_flakes INDEX 1.

    DATA(lv_query) = lt_flakes[ 1 ].

    SPLIT lv_query AT '(' INTO TABLE DATA(lt_fragments).

    lv_query = lt_fragments[ 1 ].

    lv_query = zcl_gql_schema_utils=>clean_string( lv_query ).

    CONDENSE lv_query.

    mv_query_name = lv_query.
  ENDMETHOD.

  METHOD determine_query_type.
    IF mv_query CP 'mutation *'.
      mv_query_type = zif_gql_resolver=>mc_types-mutation.
    ELSEIF mv_query CP 'query *'.
      mv_query_type = zif_gql_resolver=>mc_types-query.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
