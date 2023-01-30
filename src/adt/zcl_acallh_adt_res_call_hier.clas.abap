"! <p class="shorttext synchronized" lang="en">Resource for ABAP Call Hierarchy</p>
CLASS zcl_acallh_adt_res_call_hier DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      create_callees_result
        IMPORTING
          root_element       TYPE REF TO zif_acallh_abap_element
          hierarchy_settings TYPE zif_acallh_ty_global=>ty_hierarchy_api_settings
        RETURNING
          VALUE(result)      TYPE zif_acallh_ty_adt=>ty_call_hierarchy_result,
      create_callers_result
        IMPORTING
          root_element  TYPE REF TO zif_acallh_abap_element
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_adt=>ty_call_hierarchy_result,
      get_path_type
        IMPORTING
          request       TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(result) TYPE string
        RAISING
          cx_adt_rest,
      convert_call_position
        IMPORTING
          call_position TYPE zif_acallh_ty_global=>ty_source_position
          uri           TYPE string
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_adt=>ty_call_position,
      get_root_element
        IMPORTING
          path          TYPE string
          path_type     TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_abap_element
        RAISING
          zcx_acallh_exception,
      get_object_identifier
        IMPORTING
          abap_element_info TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result)     TYPE string,
      create_result
        IMPORTING
          root_element  TYPE REF TO zif_acallh_abap_element
          elements      TYPE zif_acallh_abap_element=>ty_ref_tab
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_adt=>ty_call_hierarchy_result.
ENDCLASS.



CLASS zcl_acallh_adt_res_call_hier IMPLEMENTATION.

  METHOD get.
    DATA: hierarchy_result   TYPE zif_acallh_ty_adt=>ty_call_hierarchy_result,
          hierarchy_settings TYPE zif_acallh_ty_global=>ty_hierarchy_api_settings.

    DATA(path) = zcl_acallh_adt_request_util=>get_query_parameter(
      param_name = zif_acallh_c_global=>c_call_hierarchy_params-path
      mandatory  = abap_true
      request    = request ).

    DATA(path_type) = get_path_type( request ).

    hierarchy_settings-use_first_intf_impl = zcl_acallh_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_acallh_c_global=>c_call_hierarchy_params-auto_resolve_intf_method
      request    = request ).

    TRY.
        DATA(root_element) = get_root_element( path      = path
                                               path_type = path_type ).
      CATCH zcx_acallh_exception.
        response->set_status( cl_rest_status_code=>gc_success_no_content ).
        RETURN.
    ENDTRY.

    IF root_element IS NOT INITIAL.
*      hierarchy_result = create_callees_result(
*        root_element       = root_element
*        hierarchy_settings = hierarchy_settings ).
      hierarchy_result = create_callers_result(
        root_element = root_element ).
      response->set_body_data(
        content_handler = zcl_acallh_ch_factory=>create_call_hier_result_ch( )
        data            = hierarchy_result ).
    ELSE.
      " raise exception???
    ENDIF.

  ENDMETHOD.


  METHOD get_path_type.
    result = zcl_acallh_adt_request_util=>get_query_parameter(
      param_name = zif_acallh_c_global=>c_call_hierarchy_params-path_type
      mandatory  = abap_true
      request    = request ).

    IF result <> zif_acallh_c_global=>c_path_types-full_name AND
        result <> zif_acallh_c_global=>c_path_types-uri.
      RAISE EXCEPTION TYPE zcx_acallh_adt_rest
        EXPORTING
          text = |Value { result } of query parameter 'pathType' is not valid | &&
                 |[{ zif_acallh_c_global=>c_path_types-full_name },{ zif_acallh_c_global=>c_path_types-uri }]|.
    ENDIF.
  ENDMETHOD.


  METHOD create_callees_result.
    result = create_result(
      root_element = root_element
      elements     = root_element->get_called_elements( settings = hierarchy_settings ) ).
  ENDMETHOD.


  METHOD create_callers_result.
    result = create_result(
      root_element = root_element
      elements     = root_element->get_calling_elements( ) ).
  ENDMETHOD.


  METHOD create_result.
    DATA(root_uri) = root_element->get_call_position_uri( ).
    DATA(root_object_identifier) = get_object_identifier( root_element->element_info ).

    result = VALUE #(
      origin_type              = root_element->element_info-adt_type
      origin_object_name       = root_element->element_info-object_name
      origin_encl_object_name  = root_element->element_info-encl_obj_display_name
      origin_object_identifier = root_object_identifier
      entries                  = VALUE #(
        ( object_ref            = VALUE zif_acallh_ty_adt=>ty_adt_obj_ref(
            uri          = root_uri
            name         = root_element->element_info-object_name
            description  = root_element->element_info-description
            type         = root_element->element_info-adt_type )
          object_identifier     = root_object_identifier
          encl_obj_name         = root_element->element_info-encl_object_name
          encl_obj_display_name = root_element->element_info-encl_obj_display_name
          method_props          = root_element->element_info-method_props ) ) ).

    LOOP AT elements INTO DATA(called_element).
      DATA(call_positions) = VALUE zif_acallh_ty_adt=>ty_call_positions(
        FOR <callpos> IN called_element->element_info-call_positions
        ( convert_call_position(
            call_position = <callpos>
            uri           = called_element->get_call_position_uri( <callpos> ) ) ) ).

      result-entries = VALUE #( BASE result-entries
        ( object_ref            = VALUE zif_acallh_ty_adt=>ty_adt_obj_ref(
            " fallback to start of include if no call positions could be found
            uri          = VALUE #( call_positions[ 1 ]-uri DEFAULT called_element->get_call_position_uri( ) )
            parent_uri   = root_uri
            name         = called_element->element_info-object_name
            description  = called_element->element_info-description
            type         = called_element->element_info-adt_type )
          object_identifier     = get_object_identifier( called_element->element_info )
          encl_obj_name         = called_element->element_info-encl_object_name
          encl_obj_display_name = called_element->element_info-encl_obj_display_name
          method_props          = called_element->element_info-method_props
          call_positions        = call_positions ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_call_position.
    DATA(pos_in_uri) = zcl_acallh_adt_uri_util=>get_uri_source_start_pos( uri ).
    result = CORRESPONDING #( call_position ).
    result-uri = uri.
    result-line = pos_in_uri-line.
  ENDMETHOD.


  METHOD get_root_element.
    IF path_type = zif_acallh_c_global=>c_path_types-uri.
      result = zcl_acallh_call_hierarchy=>get_abap_element_from_uri( path ).
    ELSE.
      result = zcl_acallh_call_hierarchy=>get_abap_elem_from_full_name( path ).
    ENDIF.
  ENDMETHOD.


  METHOD get_object_identifier.
    result = abap_element_info-full_name.
  ENDMETHOD.

ENDCLASS.
