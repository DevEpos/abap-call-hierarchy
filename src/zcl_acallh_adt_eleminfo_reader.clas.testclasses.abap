*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      include_for_test    TYPE progname,
      uri_for_test        TYPE string,
      exp_meth_properties TYPE zif_acallh_ty_global=>ty_method_properties.

    METHODS:
      assert_equals RAISING cx_static_check,
      test_method_def FOR TESTING RAISING cx_static_check,
      abstract_method_def FOR TESTING RAISING cx_static_check,
      constructor_def FOR TESTING RAISING cx_static_check,
      redef_intf_meth_impl FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD assert_equals.
    DATA(cut) = zcl_acallh_adt_eleminfo_reader=>get_reader( include_for_test ).
    DATA(actual_meth_properties) = cut->read_method_properties( uri_for_test ).

    cl_abap_unit_assert=>assert_equals( act = actual_meth_properties
                                        exp = exp_meth_properties ).
  ENDMETHOD.


  METHOD test_method_def.
    uri_for_test = `/sap/bc/adt/oo/classes/zcl_acallh_uri_to_src_mapper/includes/testclasses#start=13,10`.
    include_for_test = cl_oo_classname_service=>get_ccau_name( 'ZCL_ACALLH_URI_TO_SRC_MAPPER' ).

    exp_meth_properties = VALUE #(
      is_test_method = abap_true
      visibility     = zif_acallh_c_method_visibility=>private
      is_static      = abap_false ).

    assert_equals( ).
  ENDMETHOD.


  METHOD abstract_method_def.
    uri_for_test = `/sap/bc/adt/oo/classes/zcl_acallh_test3/source/main#start=11,15`.

    include_for_test = cl_oo_classname_service=>get_cs_name( 'ZCL_ACALLH_TEST3' ).

    exp_meth_properties = VALUE #(
      visibility     = zif_acallh_c_method_visibility=>protected
      is_abstract    = abap_true ).

    assert_equals( ).
  ENDMETHOD.


  METHOD constructor_def.
    uri_for_test = `/sap/bc/adt/oo/classes/zcl_acallh_test3/source/main#start=13,17`.

    include_for_test = cl_oo_classname_service=>get_cs_name( 'ZCL_ACALLH_TEST3' ).

    exp_meth_properties = VALUE #(
      visibility     = zif_acallh_c_method_visibility=>private
      is_constructor = abap_true ).

    assert_equals( ).

  ENDMETHOD.

  METHOD redef_intf_meth_impl.
* Redefinition is not recognized here
    uri_for_test = `/sap/bc/adt/oo/classes/zcl_acallh_test3/source/main#start=30,27`.
    include_for_test = cl_oo_classname_service=>get_cs_name( 'ZCL_ACALLH_TEST3' ).

    exp_meth_properties = VALUE #(
      visibility     = zif_acallh_c_method_visibility=>public ).

    assert_equals( ).
  ENDMETHOD.

ENDCLASS.
