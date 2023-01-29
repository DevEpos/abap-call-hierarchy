"! <p class="shorttext synchronized" lang="en">Factory for creating ABAP element's</p>
CLASS zcl_acallh_abap_element_fac DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_acallh_abap_element_fac.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves factory instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_abap_element_fac.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO zif_acallh_abap_element_fac.

    METHODS:
      constructor,
      get_adt_type
        IMPORTING
          element_data  TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result) TYPE string,
      fill_missing_information
        CHANGING
          elem_info TYPE zif_acallh_ty_global=>ty_abap_element.
ENDCLASS.



CLASS zcl_acallh_abap_element_fac IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD fill_missing_information.

    DATA(ref_stack) = zcl_acallh_fullname_util=>get_parts( elem_info-full_name ).

    CHECK lines( ref_stack ) >= 1.

    DATA(first_ref_entry) = ref_stack[ 1 ].
    DATA(second_ref_entry) = VALUE #( ref_stack[ 2 ] OPTIONAL ).

    IF first_ref_entry-tag = cl_abap_compiler=>tag_type.
      elem_info-type-legacy_type = swbm_c_type_cls_mtd_impl.

      elem_info-encl_object_name =
        elem_info-encl_obj_display_name = first_ref_entry-name.
    ELSEIF first_ref_entry-tag = cl_abap_compiler=>tag_program.
      elem_info-encl_object_name = first_ref_entry-name.

      IF second_ref_entry-tag = cl_abap_compiler=>tag_type.
        elem_info-type-legacy_type = swbm_c_type_prg_class_method.

        DATA(encl_class) = translate( val = CONV seoclsname( first_ref_entry-name ) from = '=' to = '' ).
        elem_info-encl_obj_display_name = |{ encl_class }=>{ second_ref_entry-name }|.
      ELSE.
        elem_info-type-legacy_type = swbm_c_type_prg_subroutine.
        elem_info-encl_obj_display_name = elem_info-encl_object_name.
      ENDIF.
    ELSEIF first_ref_entry-tag = cl_abap_compiler=>tag_form.
      elem_info-type-legacy_type = swbm_c_type_prg_subroutine.
    ELSEIF first_ref_entry-tag = cl_abap_compiler=>tag_function.
      elem_info-type-legacy_type = swbm_c_type_function.
    ENDIF.

    IF elem_info-object_name CS '->'.
      DATA object_name_parts TYPE string_table.
      SPLIT elem_info-object_name AT '->' INTO TABLE object_name_parts.
      elem_info-object_name = object_name_parts[ 2 ].
    ENDIF.

  ENDMETHOD.


  METHOD get_adt_type.
    DATA tadir_type TYPE trobjtype.

    CASE element_data-type-legacy_type.
      WHEN zif_acallh_c_euobj_type=>form OR
           zif_acallh_c_euobj_type=>local_impl_method.
        tadir_type = zif_acallh_c_tadir_type=>program.

      WHEN zif_acallh_c_euobj_type=>function.
        tadir_type = zif_acallh_c_tadir_type=>function_group.

      WHEN zif_acallh_c_euobj_type=>method.
        tadir_type = zif_acallh_c_tadir_type=>class.
    ENDCASE.

    result = |{ tadir_type }/{ element_data-type-legacy_type }|.
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW zcl_acallh_abap_element_fac( ).
    ENDIF.

    result = instance.
  ENDMETHOD.


  METHOD zif_acallh_abap_element_fac~create_abap_element.
    DATA(l_element_info) = element_info.
    fill_missing_information( CHANGING elem_info = l_element_info ).

    IF l_element_info-main_program IS INITIAL.
      zcl_acallh_mainprog_resolver=>resolve_main_prog( REF #( l_element_info ) ).
    ENDIF.

    l_element_info-description = zcl_acallh_elem_descr_reader=>get_instance( )->get_description( l_element_info ).
    l_element_info-adt_type = get_adt_type( l_element_info ).

    result = NEW zcl_acallh_abap_element(
      data                = l_element_info
      hierarchy_service   = zcl_acallh_call_hierarchy=>get_call_hierarchy_srv( )
      where_used_hier_srv = zcl_acallh_call_hierarchy=>get_where_used_hierarchy_srv( ) ).
  ENDMETHOD.

ENDCLASS.
