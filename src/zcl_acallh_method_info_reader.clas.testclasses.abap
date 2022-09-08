*"* use this source file for your ABAP unit test classes
CLASS ltcl_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      fullname    TYPE string,
      is_error_ok TYPE abap_bool,
      exp_props   TYPE zif_acallh_ty_global=>ty_method_properties.

    METHODS:
      assert_equals RAISING cx_static_check,
      local_class_glob_intf FOR TESTING RAISING cx_static_check,
      non_existing_meth_in_path FOR TESTING RAISING cx_static_check,
      local_class_intf_alias_meth FOR TESTING RAISING cx_static_check,
      normal_abstract_method FOR TESTING RAISING cx_static_check,
      redefined_intf_method FOR TESTING RAISING cx_static_check,
      static_constructor FOR TESTING RAISING cx_static_check,
      private_constructor FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unit IMPLEMENTATION.

  METHOD assert_equals.
    DATA(method_reader) = zcl_acallh_method_info_reader=>get_instance( ).

    TRY.
        DATA(props) = method_reader->read_properties( fullname ).
      CATCH zcx_acallh_exception INTO DATA(error).
    ENDTRY.

    IF is_error_ok = abap_true.
      cl_abap_unit_assert=>assert_bound( error ).
      RETURN.
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( error ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals( act = props exp = exp_props ).

  ENDMETHOD.

  METHOD local_class_glob_intf.
    fullname = |\\PR:{ cl_oo_classname_service=>get_classpool_name( 'CL_ADT_URI_MAPPER' ) }| &&
               |\\TY:LCL_BROKER_TO_OBJREF_SAFE\\IN:LIF_MAP_TO_OBJREF_BROKER\\ME:GET_MAPPER|.

    exp_props = VALUE #(
      name       = 'LIF_MAP_TO_OBJREF_BROKER~GET_MAPPER'
      visibility = zif_acallh_c_method_visibility=>public ).

    assert_equals( ).
  ENDMETHOD.


  METHOD non_existing_meth_in_path.
    fullname = '\TY:CL_ADT_URI_MAPPER\IN:IF_ADT_URI_MAPPER_VIT\ME:NOT_THERE'.
    is_error_ok = abap_true.

    assert_equals( ).
  ENDMETHOD.


  METHOD local_class_intf_alias_meth.
    fullname = |\\PR:{ cl_oo_classname_service=>get_classpool_name( 'ZCL_ACALLH_TEST1' ) }| &&
               |\\TY:LCL_LOCAL\\ME:ALIAS_FOR_RUN|.

    exp_props = VALUE #(
      name       = 'ALIAS_FOR_RUN'
      is_alias   = abap_true
      alias_for  = 'ZIF_ACALLH_TEST1~RUN'
      visibility = zif_acallh_c_method_visibility=>public ).

    assert_equals( ).
  ENDMETHOD.


  METHOD normal_abstract_method.
    fullname = `\TY:ZCL_ACALLH_TEST3\ME:ABSTRACT1`.

    exp_props = VALUE #(
      name        = 'ABSTRACT1'
      is_abstract = abap_true
      visibility  = zif_acallh_c_method_visibility=>protected ).

    assert_equals( ).
  ENDMETHOD.

  METHOD redefined_intf_method.
    fullname = `\TY:ZCL_ACALLH_TEST3\IN:ZIF_ACALLH_TEST1\ME:RUN`.

    exp_props = VALUE #(
      name          = 'ZIF_ACALLH_TEST1~RUN'
      is_redefined  = abap_true
      visibility    = zif_acallh_c_method_visibility=>public ).

    assert_equals( ).
  ENDMETHOD.


  METHOD private_constructor.
    fullname = `\TY:ZCL_ACALLH_TEST3\ME:CONSTRUCTOR`.

    exp_props = VALUE #(
      name           = 'CONSTRUCTOR'
      is_constructor = abap_true
      visibility     = zif_acallh_c_method_visibility=>private ).

    assert_equals( ).
  ENDMETHOD.


  METHOD static_constructor.
    fullname = `\TY:ZCL_ACALLH_TEST3\ME:CLASS_CONSTRUCTOR`.

    exp_props = VALUE #(
      name           = 'CLASS_CONSTRUCTOR'
      is_constructor = abap_true
      is_static      = abap_true
      visibility     = zif_acallh_c_method_visibility=>public ).

    assert_equals( ).
  ENDMETHOD.

ENDCLASS.
