"! <p class="shorttext synchronized" lang="en">Reads method information</p>
CLASS zcl_acallh_method_info_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_acallh_method_info_reader.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zif_acallh_method_info_reader.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_class_constructor_name TYPE string VALUE 'CLASS_CONSTRUCTOR' ##NO_TEXT,
      c_constructor_name       TYPE string VALUE 'CONSTRUCTOR' ##NO_TEXT.

    CLASS-DATA:
      instance TYPE REF TO zif_acallh_method_info_reader.

    METHODS:
      get_method_name
        IMPORTING
          full_name_info TYPE REF TO if_ris_abap_fullname
        RETURNING
          VALUE(result)  TYPE abap_methname,
      get_type_descr
        IMPORTING
          full_name_info TYPE REF TO if_ris_abap_fullname
        RETURNING
          VALUE(result)  TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_acallh_exception,
      fill_method_properties
        IMPORTING
          method_name   TYPE abap_methname
          object_descr  TYPE REF TO cl_abap_objectdescr
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_global=>ty_method_properties
        RAISING
          zcx_acallh_exception.
ENDCLASS.



CLASS zcl_acallh_method_info_reader IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW zcl_acallh_method_info_reader( ).
    ENDIF.

    result = instance.
  ENDMETHOD.


  METHOD zif_acallh_method_info_reader~read_properties.
    DATA(full_name_info) = zcl_acallh_fullname_util=>get_info_obj( full_name ).
    IF full_name_info->get_abap_fullname_tag( ) <> cl_abap_compiler=>tag_method.
      RETURN.
    ENDIF.

    result = fill_method_properties(
      method_name  = get_method_name( full_name_info )
      object_descr = get_type_descr( full_name_info ) ).
  ENDMETHOD.


  METHOD get_method_name.
    full_name_info->get_all_parts( IMPORTING et_parts = DATA(name_parts) ).

    DATA(is_intf_in_path) = xsdbool( line_exists( name_parts[ key = cl_abap_compiler=>tag_interface ] ) ).

    LOOP AT name_parts ASSIGNING FIELD-SYMBOL(<part>).

      CASE <part>-key.

        WHEN cl_abap_compiler=>tag_interface.
          result = <part>-value.

        WHEN cl_abap_compiler=>tag_method.
          IF is_intf_in_path = abap_true.
            result = |{ result }~{ <part>-value }|.
          ELSE.
            result = <part>-value.
          ENDIF.
          EXIT.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_type_descr.
    DATA type_descr TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = full_name_info->get_rtti_class_type( )
      RECEIVING
        p_descr_ref    = type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = ||.
    ENDIF.
    IF type_descr->kind <> cl_abap_typedescr=>kind_intf AND type_descr->kind <> cl_abap_typedescr=>kind_class.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = ||.
    ENDIF.

    result = CAST #( type_descr ).

  ENDMETHOD.


  METHOD fill_method_properties.
    DATA rtti_visibility TYPE abap_visibility.

    LOOP AT object_descr->methods ASSIGNING FIELD-SYMBOL(<method>) WHERE name = method_name.
      result = VALUE #(
        name         = <method>-name
        alias_for    = <method>-alias_for
        encl_type    = COND #(
          WHEN object_descr->kind = cl_abap_typedescr=>kind_class THEN zif_acallh_c_tadir_type=>class
          ELSE                                                         zif_acallh_c_tadir_type=>interface )
        is_abstract  = <method>-is_abstract
        is_redefined = <method>-is_redefined
        is_final     = <method>-is_final
        is_alias     = xsdbool( <method>-alias_for IS NOT INITIAL )
        is_handler   = xsdbool( <method>-for_event IS NOT INITIAL )
        is_static    = <method>-is_class
        is_constructor = xsdbool( <method>-name = c_constructor_name OR <method>-name = c_class_constructor_name ) ).

      IF <method>-name = c_constructor_name.
        " custom logic for CONSTRUCTOR, as RTTI does not return the correct visibility in the method
        rtti_visibility = CAST cl_abap_classdescr( object_descr )->create_visibility.
      ELSE.
        rtti_visibility = <method>-visibility.
      ENDIF.

      result-visibility = SWITCH #( rtti_visibility
        WHEN cl_abap_objectdescr=>public THEN zif_acallh_c_method_visibility=>public
        WHEN cl_abap_objectdescr=>protected THEN zif_acallh_c_method_visibility=>protected
        WHEN cl_abap_objectdescr=>private THEN zif_acallh_c_method_visibility=>private ).
      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Method { method_name } not found in type { object_descr->absolute_name }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
