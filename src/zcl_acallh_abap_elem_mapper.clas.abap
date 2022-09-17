"! <p class="shorttext synchronized" lang="en">Mapper for ABAP Elements</p>
CLASS zcl_acallh_abap_elem_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_acallh_abap_elem_mapper.

    CLASS-METHODS:
      class_constructor,
      create
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_abap_elem_mapper.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      relevant_legacy_types TYPE RANGE OF seu_stype.

    DATA:
      compiler TYPE REF TO zif_acallh_abap_compiler.

    METHODS:
      create_fullname_from_src
        IMPORTING
          uri_include_info TYPE zif_acallh_ty_global=>ty_adt_uri_info
        RETURNING
          VALUE(result)    TYPE string,
      fill_elem_info_via_crossref
        IMPORTING
          fullname      TYPE string
        RETURNING
          VALUE(result) TYPE ris_s_adt_data_request
        RAISING
          zcx_acallh_exception,
      fill_method_properties
        IMPORTING
          element_info  TYPE REF TO zif_acallh_ty_global=>ty_abap_element
        CHANGING
          fullname_info TYPE REF TO if_ris_abap_fullname
        RAISING
          zcx_acallh_exception,
      convert_fullname_to_abap_elem
        IMPORTING
          main_prog     TYPE progname
          fullname      TYPE string
          include       TYPE progname OPTIONAL
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_global=>ty_abap_element
        RAISING
          zcx_acallh_exception.
ENDCLASS.



CLASS zcl_acallh_abap_elem_mapper IMPLEMENTATION.

  METHOD class_constructor.
    relevant_legacy_types = VALUE #( sign = 'I' option = 'EQ'
      ( low = zif_acallh_c_euobj_type=>form )
      ( low = zif_acallh_c_euobj_type=>function )
      ( low = zif_acallh_c_euobj_type=>method )
      ( low = zif_acallh_c_euobj_type=>local_impl_method ) ).
  ENDMETHOD.


  METHOD create.
    result = NEW zcl_acallh_abap_elem_mapper( ).
  ENDMETHOD.


  METHOD zif_acallh_abap_elem_mapper~map_uri_to_abap_element.
    CALL FUNCTION 'RS_WORKING_AREA_INIT'.

    DATA(uri_include_info) = zcl_acallh_uri_to_src_mapper=>create( )->map_adt_uri_to_src( uri ).
    IF uri_include_info-source_position IS INITIAL.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |URI without positional fragment cannot be mapped|.
    ENDIF.

    DATA(fullname) = create_fullname_from_src( uri_include_info = uri_include_info ).

    result = convert_fullname_to_abap_elem(
      main_prog = uri_include_info-main_prog
      include   = uri_include_info-include
      fullname  = fullname ).
  ENDMETHOD.


  METHOD zif_acallh_abap_elem_mapper~map_full_name_to_abap_element.
    CALL FUNCTION 'RS_WORKING_AREA_INIT'.

    result = convert_fullname_to_abap_elem(
      main_prog = main_prog
      fullname  = full_name ).
  ENDMETHOD.


  METHOD create_fullname_from_src.
    DATA: include_source TYPE string_table.

    compiler = zcl_acallh_abap_compiler=>get( main_prog = uri_include_info-main_prog ).

    result = compiler->get_full_name_for_position(
      include = uri_include_info-include
      line    = uri_include_info-source_position-line
      column  = uri_include_info-source_position-column )-full_name.

    IF result IS INITIAL AND uri_include_info-source_position-column > 1.
      result = compiler->get_full_name_for_position(
        include = uri_include_info-include
        line    = uri_include_info-source_position-line
        column  = uri_include_info-source_position-column - 1 )-full_name.
    ENDIF.

    " sometimes the method name is not in the first line.
    IF result IS INITIAL AND uri_include_info-include+30(2) = 'CM'.
      READ REPORT uri_include_info-include INTO include_source.

      LOOP AT include_source ASSIGNING FIELD-SYMBOL(<source_line>) WHERE table_line CP '*method *.'.
        DATA(corrected_line) = sy-tabix.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        result = compiler->get_full_name_for_position(
          include = uri_include_info-include
          line    = corrected_line
          column  = uri_include_info-source_position-column )-full_name.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD fill_elem_info_via_crossref.

    DATA: findstrings   TYPE rinfoobj,
          findstring    TYPE rsfind,
          findtype      TYPE seu_obj,
          scope_object  TYPE rsfind,
          scope_objects TYPE rinfoobj.

    CALL FUNCTION 'RS_CONV_FULLNAME_TO_CROSSREF'
      EXPORTING
        full_name              = fullname
      IMPORTING
        i_find_obj_cls         = findtype
        i_findstrings          = findstrings
      CHANGING
        i_scope_objects        = scope_objects
      EXCEPTIONS
        full_name_syntax_error = 1
        unknown                = 2
        OTHERS                 = 3.

    IF findtype IS INITIAL OR sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |ABAP full name { fullname } cannot be converted|.
    ENDIF.

    result-full_name = fullname.

    IF findtype IS NOT INITIAL.
      IF strlen( findtype ) > 3.
        " new WB object types
        result-trobjtype = findtype(4).
        result-subtype   = findtype+4.
      ELSE.
        " legacy types
        result-legacy_type = findtype.
        cl_wb_object_type=>get_r3tr_from_internal_type( EXPORTING  p_internal_type = result-legacy_type
                                                        RECEIVING  p_tadir_type    = result-trobjtype
                                                        EXCEPTIONS OTHERS          = 0 ).
      ENDIF.
    ENDIF.

    IF result-legacy_type NOT IN relevant_legacy_types.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Unsupported legacy type { result-legacy_type } type detected|.
    ENDIF.

    IF findstrings IS NOT INITIAL.
      READ TABLE findstrings INDEX 1 INTO findstring.
      result-object_name      = findstring-object.
      result-encl_object_name = findstring-encl_obj.
    ENDIF.

    IF scope_objects IS NOT INITIAL.
      READ TABLE scope_objects INDEX 1 INTO scope_object.
      result-scope_object_name      = scope_object-object.
      result-scope_encl_object_name = scope_object-encl_obj.
    ENDIF.

  ENDMETHOD.


  METHOD fill_method_properties.
    DATA(method_info_reader) = zcl_acallh_method_info_reader=>get_instance( ).
    DATA(method_props) = method_info_reader->read_properties( element_info->full_name ).

    " if alias method is found, the full name needs to be adjusted
    IF method_props-is_alias = abap_true.
      DATA(comp_separator) = find( val = method_props-alias_for sub = '~' ).
      DATA(after_sep_offset) = comp_separator + 1.

      fullname_info->get_all_parts( IMPORTING et_parts = DATA(name_parts) ).

      DATA(method_part_offset) = find( val = element_info->full_name sub = '\ME:' ).
      element_info->full_name = element_info->full_name(method_part_offset).

      " append alias name parts to full name

      element_info->full_name = |{ element_info->full_name }\\IN:{ method_props-alias_for(comp_separator) }| &&
                             |\\ME:{ method_props-alias_for+after_sep_offset }|.

      " refetch the full_name info
      fullname_info = zcl_acallh_fullname_util=>get_info_obj( element_info->full_name ).
    ENDIF.

    element_info->method_props = method_props.

  ENDMETHOD.


  METHOD convert_fullname_to_abap_elem.

    IF fullname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_acallh_exception.
    ENDIF.

    DATA(fullname_info) = zcl_acallh_fullname_util=>get_info_obj( fullname ).
    DATA(tag) = fullname_info->get_abap_fullname_tag( ).
    IF tag <> cl_abap_compiler=>tag_method AND
        tag <> cl_abap_compiler=>tag_function AND
        tag <> cl_abap_compiler=>tag_form.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Unsupported Tag { tag } detected|.
    ENDIF.

    DATA(element_info) = CORRESPONDING zif_acallh_ty_global=>ty_abap_element(
      fill_elem_info_via_crossref( fullname = fullname ) ).
    element_info-main_program = main_prog.
    element_info-include = include.
    element_info-tag = tag.

    DATA(current_main_prog) = element_info-main_program.
    zcl_acallh_mainprog_resolver=>resolve_main_prog( element_info  = REF #( element_info )
                                                     ignore_filled = abap_true ).

    IF tag = cl_abap_compiler=>tag_method.
      fill_method_properties( EXPORTING element_info  = REF #( element_info )
                              CHANGING  fullname_info = fullname_info ).
    ENDIF.

    result = element_info.
  ENDMETHOD.

ENDCLASS.
