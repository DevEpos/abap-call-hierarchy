*"* use this source file for your ABAP unit test classes
CLASS ltcl_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA:
      uri               TYPE string,
      source_code       TYPE string_table,
      fragment          TYPE cl_adt_text_plain_fragmnt_hndl=>ty_fragment_parsed,
      expected_fullname TYPE string,
      is_error_ok       TYPE abap_bool.

    METHODS:
      assert_equals RAISING cx_static_check,

      uri_without_fragment FOR TESTING RAISING cx_static_check,
      normal_method FOR TESTING RAISING cx_static_check,
      interface_method_impl FOR TESTING RAISING cx_static_check,
      function_call FOR TESTING RAISING cx_static_check,
      form_call_inside_function FOR TESTING RAISING cx_static_check,

      "! Call of interface method with pattern [class->interface~method]
      class_intf_method_call FOR TESTING RAISING cx_static_check,
      interface_method_call FOR TESTING RAISING cx_static_check,
      form_call FOR TESTING RAISING cx_static_check,
      local_class_alias_method_call FOR TESTING RAISING cx_static_check,
      redef_intf_meth_definition for testing raising cx_static_check.
ENDCLASS.

CLASS ltcl_unit IMPLEMENTATION.

  METHOD assert_equals.
    data(pos_mapper) = zcl_acallh_adt_pos_mapper=>create( ).
    TRY.
        DATA(map_result) = pos_mapper->map_uri_to_abap_element( uri = uri ).
      CATCH zcx_acallh_exception INTO DATA(error).
    ENDTRY.

    IF is_error_ok = abap_true.
      cl_abap_unit_assert=>assert_bound( error ).
      RETURN.
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( error ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals( act = map_result-full_name
                                        exp = expected_fullname ).
  ENDMETHOD.


  METHOD uri_without_fragment.
    uri = `/sap/bc/adt/oo/classes/zcl_acallh_test1/source/main`.
    is_error_ok = abap_true.
    assert_equals( ).
  ENDMETHOD.


  METHOD normal_method.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 24 offset = 11 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    expected_fullname = '\TY:ZCL_ACALLH_TEST1\ME:TEST1'.

    assert_equals( ).
  ENDMETHOD.


  METHOD interface_method_impl.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 32 offset = 28 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    expected_fullname = '\TY:ZCL_ACALLH_TEST1\IN:ZIF_ACALLH_TEST1\ME:RUN'.

    assert_equals( ).
  ENDMETHOD.


  METHOD function_call.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 58 offset = 31 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    expected_fullname = '\FU:REPOSITORY_ENVIRONMENT_ALL'.

    assert_equals( ).
  ENDMETHOD.


  METHOD form_call_inside_function.
    uri = `/sap/bc/adt/functions/groups/seua/fmodules/repository_environment_all/source/main#start=27,21`.

    expected_fullname = '\PR:SAPLSEUA\FO:SAVE_FOR_RECURRENCE'.

    assert_equals( ).
  ENDMETHOD.


  METHOD interface_method_call.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 62 offset = 34 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    " TODO: not yet correct -> should have the single class implementing the interface in front
    expected_fullname = '\TY:ZCL_ACALLH_TEST2\IN:ZIF_ACALLH_TEST2\ME:EXECUTE'.

    assert_equals( ).
  ENDMETHOD.


  METHOD class_intf_method_call.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 64 offset = 41 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    expected_fullname = '\TY:ZCL_ACALLH_TEST2\IN:ZIF_ACALLH_TEST2\ME:EXECUTE'.

    assert_equals( ).
  ENDMETHOD.


  METHOD form_call.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 69 offset = 25 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    expected_fullname = '\PR:SAPLSEUA\FO:SAVE_FOR_RECURRENCE'.

    assert_equals( ).
  ENDMETHOD.


  METHOD local_class_alias_method_call.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST1' ).

    fragment-start = VALUE #( line = 27 offset = 25 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST1'
                                                                     fragment   = fragment ).

    expected_fullname = |\\PR:{ cl_oo_classname_service=>get_classpool_name( 'ZCL_ACALLH_TEST1' ) }| &&
                        |\\TY:LCL_LOCAL\\IN:ZIF_ACALLH_TEST1\\ME:RUN|.

    assert_equals( ).
  ENDMETHOD.


  METHOD redef_intf_meth_definition.
    DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = 'ZCL_ACALLH_TEST3' ).

    fragment-start = VALUE #( line = 9 offset = 32 ).
    uri = cl_oo_adt_uri_builder_class=>create_uri_for_class_include( class_name = 'ZCL_ACALLH_TEST3'
                                                                     fragment   = fragment ).

    expected_fullname = `\TY:ZCL_ACALLH_TEST3\IN:ZIF_ACALLH_TEST1\ME:RUN`.

    assert_equals( ).
  ENDMETHOD.

ENDCLASS.
