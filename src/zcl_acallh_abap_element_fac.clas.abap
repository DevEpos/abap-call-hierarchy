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

    DATA:
      relevant_legacy_types TYPE RANGE OF seu_stype.

    METHODS:
      constructor,
      get_adt_type
        IMPORTING
          element_data  TYPE zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result) TYPE string,
      determine_source_info
        IMPORTING
          uri               TYPE string
          source_pos_of_uri TYPE zif_acallh_ty_global=>ty_source_position
          element_data      TYPE REF TO zif_acallh_ty_global=>ty_abap_element,
      determine_src_info_by_uri
        IMPORTING
          uri               TYPE string
          source_pos_of_uri TYPE zif_acallh_ty_global=>ty_source_position
          element_data      TYPE REF TO zif_acallh_ty_global=>ty_abap_element
        RETURNING
          VALUE(result)     TYPE zif_acallh_ty_global=>ty_ae_src_info.
ENDCLASS.



CLASS zcl_acallh_abap_element_fac IMPLEMENTATION.

  METHOD constructor.
    relevant_legacy_types = VALUE #( sign = 'I' option = 'EQ'
      ( low = zif_acallh_c_euobj_type=>form )
      ( low = zif_acallh_c_euobj_type=>function )
      ( low = zif_acallh_c_euobj_type=>method )
      ( low = zif_acallh_c_euobj_type=>local_impl_method ) ).
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW zcl_acallh_abap_element_fac( ).
    ENDIF.

    result = instance.
  ENDMETHOD.


  METHOD zif_acallh_abap_element_fac~create_abap_element.
    DATA(l_unit_data) = element_info.
    IF l_unit_data-main_program IS INITIAL.
      zcl_acallh_mainprog_resolver=>resolve_main_prog( REF #( l_unit_data ) ).
    ENDIF.

    l_unit_data-adt_type = get_adt_type( l_unit_data ).

    result = NEW zcl_acallh_abap_element(
      data              = l_unit_data
      hierarchy_service = zcl_acallh_call_hierarchy=>get_call_hierarchy_srv( ) ).
  ENDMETHOD.


  METHOD get_adt_type.
    DATA tadir_type TYPE trobjtype.

    CASE element_data-legacy_type.
      WHEN zif_acallh_c_euobj_type=>form OR
           zif_acallh_c_euobj_type=>local_impl_method.
        tadir_type = zif_acallh_c_tadir_type=>program.

      WHEN zif_acallh_c_euobj_type=>function.
        tadir_type = zif_acallh_c_tadir_type=>function_group.

      WHEN zif_acallh_c_euobj_type=>method.
        tadir_type = zif_acallh_c_tadir_type=>class.
    ENDCASE.

    result = |{ tadir_type }/{ element_data-legacy_type }|.
  ENDMETHOD.


  METHOD determine_source_info.
    " TODO: check if unnecessary compiler call can be be prevented if alias call or interface method detected
    DATA(compiler) = zcl_acallh_abap_compiler=>get( main_prog = element_data->main_program ).

    DATA(source_info) = compiler->get_src_by_start_end_refs( element_data->full_name ).

    IF source_info IS INITIAL AND element_data->tag = cl_abap_compiler=>tag_method.
      IF element_data->method_props-is_alias = abap_true.
        DATA(refs) = compiler->get_refs_by_fullname( element_data->full_name ).

        LOOP AT refs ASSIGNING FIELD-SYMBOL(<ref>) WHERE full_name = element_data->full_name.
          DATA(method_symbol) = CAST cl_abap_comp_method( <ref>-symbol ).
          " handle alias method
          IF method_symbol->super_method IS NOT INITIAL.
            DATA(super_full_name) = method_symbol->super_method->full_name.
            source_info = compiler->get_src_by_start_end_refs( super_full_name ).
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSEIF element_data->encl_object_type = zif_acallh_c_tadir_type=>interface.
        " handle interface method call
        source_info = determine_src_info_by_uri(
          uri               = uri
          source_pos_of_uri = source_pos_of_uri
          element_data      = element_data ).
      ENDIF.
    ENDIF.

    IF source_info-main_prog IS NOT INITIAL.
      element_data->main_program = source_info-main_prog.
    ENDIF.
    element_data->include = source_info-include.
    element_data->source_pos_start = source_info-start_pos.
    element_data->source_pos_end = source_info-end_pos.
  ENDMETHOD.


  METHOD determine_src_info_by_uri.
    DATA source_code TYPE string_table.

    DATA(source_reader) = lcl_intfm_src_pos_read_fac=>create_reader_by_uri(
      uri             = uri
      method_name     = |{ element_data->encl_object_name }~{ element_data->object_name }|
      full_name       = element_data->full_name_from_parser
      source_position = source_pos_of_uri ).
    IF source_reader IS INITIAL.
      RETURN.
    ENDIF.

    result = source_reader->determine_source_pos( ).
  ENDMETHOD.

ENDCLASS.
