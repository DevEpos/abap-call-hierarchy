*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_uri_mapper_factory IMPLEMENTATION.

  METHOD get_uri_mapper.
    IF matches( val = uri regex = `^/sap/bc/adt/programs/.+` ).
      result = NEW lcl_prog_uri_mapper( uri ).
    ELSEIF matches( val = uri regex = `^/sap/bc/adt/functions/groups/.+` ).
      result = NEW lcl_fugr_uri_mapper( uri ).
    ELSEIF matches( val = uri regex = `^/sap/bc/adt/oo/classes/.+` ).
      result = NEW lcl_class_uri_mapper( uri ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_class_uri_mapper IMPLEMENTATION.

  METHOD constructor.
    me->uri = uri.
  ENDMETHOD.


  METHOD lif_uri_mapper~map.
    DATA clstype TYPE seoclstype.

    FIND REGEX c_class_uri_regex IN uri
      RESULTS DATA(match).

    IF match IS INITIAL.
      RETURN.
    ENDIF.

    DATA(classname_group) = match-submatches[ 1 ].
    DATA(source_part1_group) = match-submatches[ 2 ].
    DATA(source_part2_group) = match-submatches[ 3 ].

    IF classname_group-offset <= 0 OR source_part1_group-offset <= 0.
      RETURN.
    ENDIF.

    result-uri = uri.
    result-trobjtype = zif_acallh_c_tadir_type=>class.

    DATA(classname) = CONV classname(
      to_upper(
        cl_http_utility=>unescape_url( |{ uri+classname_group-offset(classname_group-length) }| ) ) ).
    result-main_prog = cl_oo_classname_service=>get_classpool_name( classname ).

    CALL FUNCTION 'SEO_CLIF_EXISTENCE_CHECK'
      EXPORTING
        cifkey        = VALUE seoclskey( clsname = classname )
      IMPORTING
        clstype       = clstype
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        OTHERS        = 3.
    IF sy-subrc <> 0 OR clstype = 1.
      RAISE EXCEPTION TYPE zcx_acallh_exception.
    ENDIF.

    DATA(partname) = uri+source_part1_group-offset(source_part1_group-length).
    IF partname = 'source/main'.
      " map position to correct include
      DATA(clif_source) = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = classname ).
      DATA(clif_pos_converter) = cl_oo_source_pos_converter=>create( clif_key = VALUE #( clsname = classname ) source = clif_source ).
      DATA(uri_src_pos) = zcl_acallh_adt_uri_util=>get_uri_source_start_pos( uri ).
      TRY.
          DATA(include_pos) = clif_pos_converter->get_include_position( uri_src_pos ).
          result-include = include_pos-include.
          result-source_position = include_pos-source_position.
        CATCH cx_oo_clif_scan_error cx_oo_invalid_source_position INTO DATA(oo_error).
          RAISE EXCEPTION TYPE zcx_acallh_exception
            EXPORTING
              previous = oo_error.
      ENDTRY.
    ELSEIF partname = 'includes'.
      DATA(includename) = COND string( WHEN source_part2_group-offset > 0 THEN uri+source_part2_group-offset(source_part2_group-length) ).
      IF includename = 'definitions'.
        result-include = cl_oo_classname_service=>get_ccdef_name( classname ).
      ELSEIF includename = 'implementations'.
        result-include = cl_oo_classname_service=>get_ccimp_name( classname ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_fugr_uri_mapper IMPLEMENTATION.

  METHOD constructor.
    me->uri = uri.
  ENDMETHOD.


  METHOD lif_uri_mapper~map.
    FIND REGEX c_fugr_uri_regex IN uri
        RESULTS DATA(match).

    IF match IS INITIAL.
      RETURN.
    ENDIF.

    DATA(fugrname_group) = match-submatches[ 1 ].
    DATA(type_group) = match-submatches[ 2 ].
    DATA(sub_name_group) = match-submatches[ 3 ].

    IF fugrname_group-offset <= 0 OR type_group-offset <= 0 OR sub_name_group-offset <= 0.
      RETURN.
    ENDIF.

    result-uri = uri.
    result-trobjtype = zif_acallh_c_tadir_type=>function_group.

    DATA(group) = cl_http_utility=>unescape_url( to_upper( uri+fugrname_group-offset(fugrname_group-length) ) ).
    result-main_prog = zcl_acallh_func_util=>get_progname_for_group( CONV #( group ) ).
    result-include = cl_http_utility=>unescape_url( to_upper( uri+sub_name_group-offset(sub_name_group-length) ) ).

    DATA(type_name) = uri+type_group-offset(type_group-length).
    IF type_name = 'fmodules'.
      result-include = zcl_acallh_func_util=>get_function_include_by_fname( CONV #( result-include ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_prog_uri_mapper IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.


  METHOD lif_uri_mapper~map.
    FIND REGEX c_prog_uri_regex IN uri
         RESULTS DATA(match).

    IF match IS INITIAL.
      RETURN.
    ENDIF.

    DATA(type_group) = match-submatches[ 1 ].
    DATA(name_group) = match-submatches[ 2 ].

    IF type_group-offset <= 0 OR name_group-offset <= 0.
      RETURN.
    ENDIF.

    result-uri = uri.
    result-trobjtype = zif_acallh_c_tadir_type=>program.
    result-main_prog =
      result-include = cl_http_utility=>unescape_url( to_upper( uri+name_group-offset(name_group-length) ) ).

    DATA(type_name) = uri+type_group-offset(type_group-length).
    IF type_name = 'includes'.
      " TODO: differentiation necessary ??
    ENDIF.
  ENDMETHOD.

ENDCLASS.
