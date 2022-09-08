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
        ref  TYPE REF TO zif_acallh_abap_element,
      END OF ty_abap_element_info_by_line.

    CLASS-DATA:
      instance TYPE REF TO zif_acallh_call_hierarchy_srv.

    DATA:
      factory           TYPE REF TO zif_acallh_abap_element_fac,
      abap_element_info TYPE zif_acallh_ty_global=>ty_abap_element,
      refs_for_range    TYPE scr_names_tags_grades,
      called_include    TYPE program,
      compiler          TYPE REF TO zif_acallh_abap_compiler.

    METHODS:
      get_full_names_in_range,
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
        IMPORTING
          full_name     TYPE string
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
      get_method_description
        IMPORTING
          elem_info     TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result) TYPE string,
      get_description
        IMPORTING
          elem_info     TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result) TYPE string,
      get_function_description
        IMPORTING
          elem_info     TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_acallh_call_hierarchy_srv IMPLEMENTATION.

  METHOD constructor.
    ASSERT abap_elem_factory IS BOUND.
    me->factory = abap_elem_factory.
  ENDMETHOD.


  METHOD zif_acallh_call_hierarchy_srv~determine_called_units.
    CHECK abap_element->element_info-main_program IS NOT INITIAL.

    abap_element_info = abap_element->element_info.
    compiler = zcl_acallh_abap_compiler=>get( abap_element_info-main_program ).

    get_full_names_in_range( ).
    IF refs_for_range IS INITIAL.
      RETURN.
    ENDIF.

    result = create_abap_elements_from_refs( ).
  ENDMETHOD.


  METHOD get_full_names_in_range.
    IF abap_element_info-source_pos_start IS INITIAL.
      DATA(source_info) = compiler->get_src_by_start_end_refs( full_name = abap_element_info-full_name ).
      abap_element_info-include = source_info-include.
      abap_element_info-source_pos_start = source_info-start_pos.
      abap_element_info-source_pos_end = source_info-end_pos.
    ENDIF.

    refs_for_range = compiler->get_refs_in_range(
      include    = abap_element_info-include
      start_line = abap_element_info-source_pos_start-line + 1
      end_line   = abap_element_info-source_pos_end-line ).
  ENDMETHOD.


  METHOD create_abap_elements_from_refs.

    DATA sorted_comp_units TYPE SORTED TABLE OF ty_abap_element_info_by_line WITH NON-UNIQUE KEY line.

    LOOP AT refs_for_range ASSIGNING FIELD-SYMBOL(<ref>).

      DATA(direct_refs) = get_direct_references( <ref>-full_name ).
      CHECK direct_refs IS NOT INITIAL.

      DATA(call_positions) = get_call_positions( direct_refs ).
      DATA(line_of_first_occ) = call_positions[ 1 ]-line.

      DATA(original_full_name) = <ref>-full_name.
      IF <ref>-tag = cl_abap_compiler=>tag_method.
        adjust_meth_full_name( CHANGING full_name = original_full_name ).
      ENDIF.

      TRY.
          INSERT VALUE #(
            line = line_of_first_occ
            ref  = create_abap_element(
              direct_ref           = direct_refs[ 1 ]
              full_name            = original_full_name
              line_of_first_occ    = line_of_first_occ
              call_positions       = call_positions ) ) INTO TABLE sorted_comp_units.
        CATCH zcx_acallh_exception.
      ENDTRY.

      DELETE refs_for_range.

    ENDLOOP.

    result = VALUE #( FOR <comp_unit> IN sorted_comp_units ( <comp_unit>-ref ) ).

  ENDMETHOD.


  METHOD create_abap_element.

    DATA(new_elem_info) = VALUE zif_acallh_ty_global=>ty_abap_element(
      tag                 = direct_ref-tag
      object_name         = zcl_acallh_fullname_util=>get_info_obj( full_name )->get_last_part( )-value
      full_name           = full_name
      include             = direct_ref-statement->source_info->name
      call_positions      = call_positions
      parent_main_program = abap_element_info-main_program ).

    fill_legacy_type(
      EXPORTING
        full_name         = full_name "direct_ref_elem_info->fullname
      CHANGING
        abap_element_info = new_elem_info ).

    IF new_elem_info-legacy_type IS INITIAL.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Legacy type could not be determined for { full_name }|.
    ENDIF.

    IF direct_ref-tag = cl_abap_compiler=>tag_method.
      new_elem_info-method_props = zcl_acallh_method_info_reader=>get_instance( )->read_properties( full_name = full_name ).
      new_elem_info-encl_object_type = new_elem_info-method_props-encl_type.
    ENDIF.

    new_elem_info-description = get_description( new_elem_info ).

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
      include    = abap_element_info-include
      full_name  = full_name
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
      abap_element_info-legacy_type = swbm_c_type_function.
    ENDIF.
  ENDMETHOD.


  METHOD get_description.

    CASE elem_info-tag.

      WHEN cl_abap_compiler=>tag_method.
        result = get_method_description( elem_info ).

      WHEN cl_abap_compiler=>tag_function.
        result = get_function_description( elem_info ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_method_description.
    DATA: class_name   TYPE classname,
          method_parts TYPE string_table,
          method_name  TYPE seocmpname.

    IF elem_info-legacy_type = swbm_c_type_cls_mtd_impl.
      IF elem_info-method_props-name CS '~'.
        SPLIT elem_info-method_props-name AT '~' INTO TABLE method_parts.
        class_name = method_parts[ 1 ].
        method_name = method_parts[ 2 ].
      ELSE.
        class_name = elem_info-encl_object_name.
        method_name = elem_info-method_props-name.
      ENDIF.
      SELECT SINGLE descript
        FROM seocompotx
        WHERE clsname = @class_name
          AND cmpname = @method_name
          AND langu = @sy-langu
        INTO @result.
    ENDIF.
  ENDMETHOD.


  METHOD get_function_description.
    DATA(func_name) = CONV funcname( elem_info-object_name ).

    SELECT SINGLE stext
      FROM tftit
      WHERE funcname = @func_name
        AND spras = @sy-langu
      INTO @result.
  ENDMETHOD.

ENDCLASS.
