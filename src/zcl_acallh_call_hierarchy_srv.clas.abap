"! <p class="shorttext synchronized" lang="en">Call Hierarchy Service</p>
CLASS zcl_acallh_call_hierarchy_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_acallh_call_hierarchy.

  PUBLIC SECTION.
    INTERFACES zif_acallh_call_hierarchy_srv.

    METHODS:
      constructor
        IMPORTING
          abap_elem_factory TYPE REF TO zif_acallh_abap_element_fac.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_abap_element_info_by_line,
        line TYPE i,
        col  TYPE i,
        ref  TYPE REF TO zif_acallh_abap_element,
      END OF ty_abap_element_info_by_line.

    CLASS-DATA:
      instance TYPE REF TO zif_acallh_call_hierarchy_srv.

    DATA:
      factory           TYPE REF TO zif_acallh_abap_element_fac,
      abap_element_info TYPE zif_acallh_ty_global=>ty_abap_element,
      refs_for_range    TYPE scr_names_tags_grades,
      called_include    TYPE program,
      compiler          TYPE REF TO zif_acallh_abap_compiler,
      descr_reader      TYPE REF TO zif_acallh_elem_descr_reader,
      current_element   TYPE REF TO zif_acallh_abap_element.

    METHODS:
      get_full_names_in_range
        IMPORTING
          settings TYPE zif_acallh_ty_global=>ty_hierarchy_api_settings OPTIONAL,
      create_abap_element
        IMPORTING
          direct_ref        TYPE scr_ref
          full_name         TYPE string
          line_of_first_occ TYPE i
          call_positions    TYPE zif_acallh_ty_global=>ty_call_positions
        RETURNING
          VALUE(result)     TYPE REF TO zif_acallh_abap_element
        RAISING
          zcx_acallh_exception,
      create_abap_elements_from_refs
        RETURNING
          VALUE(result) TYPE zif_acallh_abap_element=>ty_ref_tab,
      adjust_meth_full_name
        CHANGING
          full_name TYPE string,
      get_direct_references
        RETURNING
          VALUE(result) TYPE scr_refs,
      get_call_positions
        IMPORTING
          refs          TYPE scr_refs
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_global=>ty_call_positions,
      filter_refs_by_include
        IMPORTING
          include       TYPE progname
          refs          TYPE scr_refs
        RETURNING
          VALUE(result) TYPE scr_refs,
      fill_legacy_type
        IMPORTING
          full_name         TYPE string
        CHANGING
          abap_element_info TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result)     TYPE seu_stype,
      determine_correct_src_pos
        IMPORTING
          settings TYPE zif_acallh_ty_global=>ty_hierarchy_api_settings OPTIONAL
        RAISING
          zcx_acallh_exception.
ENDCLASS.



CLASS zcl_acallh_call_hierarchy_srv IMPLEMENTATION.

  METHOD constructor.
    ASSERT abap_elem_factory IS BOUND.
    me->factory = abap_elem_factory.
    descr_reader = zcl_acallh_elem_descr_reader=>get_instance( ).
  ENDMETHOD.


  METHOD zif_acallh_call_hierarchy_srv~determine_called_elements.
    CHECK abap_element->element_info-main_program IS NOT INITIAL.

    me->current_element = abap_element.
    abap_element_info = abap_element->element_info.

    get_full_names_in_range( settings ).
    IF refs_for_range IS INITIAL.
      RETURN.
    ENDIF.

    result = create_abap_elements_from_refs( ).
  ENDMETHOD.


  METHOD get_full_names_in_range.
    compiler = zcl_acallh_abap_compiler=>get( abap_element_info-main_program ).

    IF abap_element_info-source_pos_start IS INITIAL.
      TRY.
          DATA(old_main_prog) = abap_element_info-main_program.
          determine_correct_src_pos( settings ).
          IF abap_element_info-main_program <> old_main_prog.
            compiler = zcl_acallh_abap_compiler=>get( main_prog = abap_element_info-main_program ).
          ENDIF.
        CATCH zcx_acallh_exception.
          RETURN.
      ENDTRY.
    ENDIF.

    refs_for_range = compiler->get_refs_in_range(
      include    = abap_element_info-include
      start_line = abap_element_info-source_pos_start-line + 1
      end_line   = abap_element_info-source_pos_end-line ).
  ENDMETHOD.


  METHOD create_abap_elements_from_refs.

    DATA(direct_refs) = get_direct_references( ).

    LOOP AT direct_refs ASSIGNING FIELD-SYMBOL(<direct_ref>) GROUP BY <direct_ref>-full_name.
      DATA(direct_refs_for_fullname) = VALUE scr_refs( FOR <ref> IN GROUP <direct_ref> ( <ref> ) ).
      DATA(call_positions) = get_call_positions( direct_refs_for_fullname ).
      DATA(first_call_pos) = call_positions[ 1 ].

      DATA(original_full_name) = <direct_ref>-full_name.
      IF <direct_ref>-tag = cl_abap_compiler=>tag_method.
        adjust_meth_full_name( CHANGING full_name = original_full_name ).
      ENDIF.

      TRY.
          result = VALUE #( BASE result
          ( create_abap_element(
              direct_ref           = direct_refs_for_fullname[ 1 ]
              full_name            = original_full_name
              line_of_first_occ    = first_call_pos-line
              call_positions       = call_positions ) ) ).
        CATCH zcx_acallh_exception.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_abap_element.

    DATA(new_elem_info) = VALUE zif_acallh_ty_global=>ty_abap_element(
      tag                 = direct_ref-tag
      object_name         = zcl_acallh_fullname_util=>get_info_obj( full_name )->get_last_part( )-value
      full_name           = full_name
      include             = direct_ref-statement->source_info->name
      call_positions      = call_positions
      parent_main_program = abap_element_info-main_program ).

    IF direct_ref-tag = cl_abap_compiler=>tag_method.
      new_elem_info-method_props = zcl_acallh_method_info_reader=>get_instance( )->read_properties( full_name = full_name ).
      new_elem_info-encl_object_type = new_elem_info-method_props-encl_type.
    ENDIF.

    result = factory->create_abap_element( new_elem_info ).

  ENDMETHOD.


  METHOD adjust_meth_full_name.
    DATA(symbol) = compiler->get_symbol_entry( full_name ).
    IF symbol IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(method_symbol) = CAST cl_abap_comp_method( symbol ).
        IF method_symbol->compkind = cl_abap_comp_symbol=>compkind_alias AND
            method_symbol->super_method IS NOT INITIAL.
          full_name = method_symbol->super_method->full_name.
        ENDIF.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.


  METHOD get_direct_references.
    result = compiler->get_direct_references(
      full_names = VALUE #( FOR <ref> IN refs_for_range ( <ref>-full_name ) )
      start_line = abap_element_info-source_pos_start-line + 1
      end_line   = abap_element_info-source_pos_end-line ).

    result = filter_refs_by_include(
      include = abap_element_info-include
      refs    = result ).
  ENDMETHOD.


  METHOD filter_refs_by_include.

    LOOP AT refs ASSIGNING FIELD-SYMBOL(<ref>).
      TRY.
          DATA(include_of_source) = <ref>-statement->source_info->name.
          " include of occurence must match include of caller
          IF include <> include_of_source.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_ref_is_initial.
          CONTINUE.
      ENDTRY.

      result = VALUE #( BASE result ( <ref> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_call_positions.
    result = VALUE #( FOR <ref> IN refs ( line = <ref>-line column = <ref>-column ) ).
  ENDMETHOD.


  METHOD fill_legacy_type.
    DATA(ref_stack) = zcl_acallh_fullname_util=>get_parts( full_name ).

    CHECK lines( ref_stack ) >= 1.

    DATA(first_ref_entry) = ref_stack[ 1 ].
    DATA(second_ref_entry) = VALUE #( ref_stack[ 2 ] OPTIONAL ).

    IF first_ref_entry-tag = cl_abap_compiler=>tag_type.
      abap_element_info-legacy_type = swbm_c_type_cls_mtd_impl.

      abap_element_info-encl_object_name =
        abap_element_info-encl_obj_display_name = first_ref_entry-name.
    ELSEIF first_ref_entry-tag = cl_abap_compiler=>tag_program.
      abap_element_info-encl_object_name = first_ref_entry-name.

      IF second_ref_entry-tag = cl_abap_compiler=>tag_type.
        abap_element_info-legacy_type = swbm_c_type_prg_class_method.

        DATA(encl_class) = translate( val = CONV seoclsname( first_ref_entry-name ) from = '=' to = '' ).
        abap_element_info-encl_obj_display_name = |{ encl_class }=>{ second_ref_entry-name }|.
      ELSE.
        abap_element_info-legacy_type = swbm_c_type_prg_subroutine.
        abap_element_info-encl_obj_display_name = abap_element_info-encl_object_name.
      ENDIF.
    ELSEIF first_ref_entry-tag = cl_abap_compiler=>tag_form.
      abap_element_info-legacy_type = swbm_c_type_prg_subroutine.
    ELSEIF first_ref_entry-tag = cl_abap_compiler=>tag_function.
      abap_element_info-legacy_type = swbm_c_type_function.
    ENDIF.
  ENDMETHOD.


  METHOD determine_correct_src_pos.
    DATA implementing_classes TYPE seor_implementing_keys.

    IF abap_element_info-tag = cl_abap_compiler=>tag_method AND
        abap_element_info-method_props-encl_type = zif_acallh_c_tadir_type=>interface.

      IF settings-use_first_intf_impl = abap_true.
        " TODO: move logic to new class

        CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
          EXPORTING
            intkey       = VALUE seoclskey( clsname = abap_element_info-encl_object_name )
          IMPORTING
            impkeys      = implementing_classes
          EXCEPTIONS
            not_existing = 1
            OTHERS       = 2.
        IF sy-subrc = 0.
          IF implementing_classes IS INITIAL.
            abap_element_info-method_props-impl_state = zif_acallh_c_meth_impl_state=>no_implementations.
            RETURN.
          ELSEIF lines( implementing_classes ) > 1.
            abap_element_info-method_props-impl_state = zif_acallh_c_meth_impl_state=>multiple_implementations.
            RETURN.
          ENDIF.

          " determine the correct method include for the interface method
          cl_oo_classname_service=>get_method_include(
            EXPORTING
              mtdkey              = VALUE #( clsname = implementing_classes[ 1 ]-clsname
                                             cpdname = |{ abap_element_info-encl_object_name }~{ abap_element_info-object_name }| )
            RECEIVING
              result              = abap_element_info-include
            EXCEPTIONS
              class_not_existing  = 1
              method_not_existing = 2
              OTHERS              = 3
          ).
          IF sy-subrc <> 0.
            " method could be implemented not at all (default ignore) or only in a subclass
            RETURN.
          ENDIF.
          abap_element_info-source_pos_start = VALUE #( line = 1 ).
          abap_element_info-source_pos_end = VALUE #( line = 1000000 ).
          abap_element_info-main_program = cl_oo_classname_service=>get_classpool_name( implementing_classes[ 1 ]-clsname ).
        ENDIF.
      ELSEIF settings-intf_impl IS NOT INITIAL.
        " TODO: set main program to given implementation - could be local or global class
      ENDIF.
    ELSE.
      DATA(source_info) = compiler->get_src_by_start_end_refs( abap_element_info-full_name ).
      IF source_info IS NOT INITIAL.
        abap_element_info-source_pos_start = source_info-start_pos.
        abap_element_info-source_pos_end = source_info-end_pos.
        abap_element_info-include = source_info-include.
      ENDIF.
    ENDIF.

    IF abap_element_info-include IS INITIAL OR abap_element_info-source_pos_start IS INITIAL.
      RAISE EXCEPTION TYPE zcx_acallh_exception.
    ENDIF.

    IF current_element->element_info-include IS INITIAL.
      current_element->set_include( abap_element_info-include ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
