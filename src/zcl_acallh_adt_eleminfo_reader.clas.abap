"! <p class="shorttext synchronized" lang="en">Reads element information via ADT RFC</p>
CLASS zcl_acallh_adt_eleminfo_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_acallh_adt_eleminfo_reader.

    CLASS-METHODS:
      get_reader
        IMPORTING
          include       TYPE progname
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_adt_eleminfo_reader
        RAISING
          zcx_acallh_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_reader_cache,
        include TYPE progname,
        ref     TYPE REF TO zif_acallh_adt_eleminfo_reader,
      END OF ty_reader_cache.

    DATA:
      include TYPE progname,
      source  TYPE xstring.

    METHODS:
      constructor
        IMPORTING
          include TYPE progname
        RAISING
          zcx_acallh_exception,
      read_source
        RAISING
          zcx_acallh_exception,
      build_request
        IMPORTING
          uri           TYPE string
        RETURNING
          VALUE(result) TYPE sadt_rest_request,
      call_rfc_endpoint
        IMPORTING
          req           TYPE sadt_rest_request
        RETURNING
          VALUE(result) TYPE cl_cc_adt_res_code_elementinfo=>ty_element_info
        RAISING
          zcx_acallh_exception,

      get_boolean_meth_prop
        IMPORTING
          elem_info     TYPE cl_cc_adt_res_code_elementinfo=>ty_element_info
          name          TYPE string
        RETURNING
          VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS zcl_acallh_adt_eleminfo_reader IMPLEMENTATION.

  METHOD get_reader.
    result = NEW zcl_acallh_adt_eleminfo_reader( include ).
  ENDMETHOD.


  METHOD zif_acallh_adt_eleminfo_reader~read_method_properties.
    DATA(elem_info) = call_rfc_endpoint( req = build_request( uri ) ).

    IF elem_info-properties IS NOT INITIAL.
      result-visibility = VALUE #(
        elem_info-properties[ key = if_cc_adt_res_code_eleminfo_co=>co_visibility ]-value
          DEFAULT zif_acallh_c_method_visibility=>unknown ).

      DATA(level) = VALUE #( elem_info-properties[ key = if_cc_adt_res_code_eleminfo_co=>co_level ]-value OPTIONAL ).
      result-is_static = xsdbool( level = 'class' ).

      result-is_test_method = get_boolean_meth_prop( elem_info = elem_info
                                                     name      = if_cc_adt_res_code_eleminfo_co=>co_test_method ).
      result-is_abstract = get_boolean_meth_prop( elem_info = elem_info
                                                  name      = if_cc_adt_res_code_eleminfo_co=>co_abstract ).
      result-is_final = get_boolean_meth_prop( elem_info = elem_info
                                               name      = if_cc_adt_res_code_eleminfo_co=>co_final ).
      result-is_constructor = get_boolean_meth_prop( elem_info = elem_info
                                                     name      = if_cc_adt_res_code_eleminfo_co=>co_constructor ).
      result-is_handler = get_boolean_meth_prop( elem_info = elem_info
                                                 name      = if_cc_adt_res_code_eleminfo_co=>co_event_handler ).
      result-is_redefined = get_boolean_meth_prop( elem_info = elem_info
                                                   name      = if_cc_adt_res_code_eleminfo_co=>co_redefinition ).
      " [7.40-CompatIssue] 'if_cc_adt_res_code_eleminfo_co=>co_alias_referenced_intf' Not available
      result-is_alias = get_boolean_meth_prop( elem_info = elem_info
                                               name      = 'refInterface' ). "if_cc_adt_res_code_eleminfo_co=>co_alias_referenced_intf ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    me->include = include.
    read_source( ).
  ENDMETHOD.


  METHOD get_boolean_meth_prop.
    DATA(prop_value) = VALUE #( elem_info-properties[ key = name ]-value OPTIONAL ).
    " [7.40-CompatIssue] 'if_cc_adt_res_code_eleminfo_co=>co_true' Not available
    result = xsdbool( prop_value = 'true' ). " if_cc_adt_res_code_eleminfo_co=>co_true ).
  ENDMETHOD.


  METHOD read_source.
    DATA: include_code     TYPE string_table.

    READ REPORT include INTO include_code.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Include { include } could not be read or does not exist|.
    ENDIF.
    DATA(include_code_str) = concat_lines_of( table = include_code sep = cl_abap_char_utilities=>cr_lf ).

    DATA(out_converter) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    TRY.
        out_converter->convert(
          EXPORTING
            data   = include_code_str
          IMPORTING
            buffer = source ).
      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO DATA(conv_error).
        RAISE EXCEPTION TYPE zcx_acallh_exception
          EXPORTING
            previous = conv_error.
    ENDTRY.
  ENDMETHOD.


  METHOD call_rfc_endpoint.
    DATA: res              TYPE sadt_rest_response,
          hierarchy_result TYPE cl_cc_adt_res_code_elementinfo=>ty_element_info,
          adt_exception    TYPE sadt_exception,
          exc_langu        TYPE string.

    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING
        request  = req
      IMPORTING
        response = res.

    IF res IS NOT INITIAL.
      IF res-status_line-status_code >= cl_rest_status_code=>gc_client_error_bad_request.
        DATA(content_type) = REF #( res-header_fields[ name = if_http_header_fields=>content_type ] OPTIONAL ).
        IF content_type IS NOT INITIAL AND content_type->value = if_rest_media_type=>gc_appl_xml.
          CALL TRANSFORMATION sadt_exception
            SOURCE XML res-message_body
            RESULT exception_data = adt_exception
                   langu = exc_langu.
          RAISE EXCEPTION TYPE zcx_acallh_exception
            EXPORTING
              text = adt_exception-localized_message.
        ENDIF.
      ELSEIF res-message_body IS NOT INITIAL.
        TRY.
            CALL TRANSFORMATION st_cc_adt_code_element
                  SOURCE XML res-message_body
                  RESULT code_element_data = result.
          CATCH cx_transformation_error INTO DATA(transformation_error).
            RAISE EXCEPTION TYPE zcx_acallh_exception
              EXPORTING
                previous = transformation_error.
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD build_request.
    DATA(adt_uri) = `/sap/bc/adt/abapsource/codecompletion/elementinfo` &&
      |?uri={ cl_http_utility=>escape_url( uri ) }|.

    result = VALUE #(
      request_line = VALUE #(
        method = 'POST'
        uri = adt_uri )
      header_fields = VALUE #( ( name = 'accept' value = 'application/vnd.sap.adt.elementinfo+xml' )
                               ( name = 'Content-Type' value = 'text/plain' ) )
      message_body = source ).
  ENDMETHOD.

ENDCLASS.
