"! <p class="shorttext synchronized" lang="en">Where-Used-List Service</p>
CLASS zcl_acallh_where_used_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_acallh_where_used_srv.

    METHODS:
      constructor
        IMPORTING
          abap_elem_fac TYPE REF TO zif_acallh_abap_element_fac.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ris_object,
        type           TYPE ris_s_object_type_id,
        name           TYPE ris_parameter,
        enclosing_name TYPE ris_parameter,
      END OF ty_ris_object.

    TYPES BEGIN OF ty_referencing_object_type.
    INCLUDE TYPE ris_s_md_relationship.
    TYPES: data_model TYPE REF TO cl_ris_data_model.
    TYPES END OF ty_referencing_object_type.

    TYPES:
      ty_referencing_object_types TYPE STANDARD TABLE OF ty_referencing_object_type WITH NON-UNIQUE DEFAULT KEY,

      BEGIN OF ty_ris_referenced_object,
        object                   TYPE ty_ris_object,
        scope_object             TYPE ty_ris_object,
        full_name                TYPE string,
        main_program             TYPE programm,
        referencing_object_types TYPE ty_referencing_object_types,
      END OF ty_ris_referenced_object.

    DATA:
      abap_elem_fac          TYPE REF TO zif_acallh_abap_element_fac,
      current_element        TYPE REF TO zif_acallh_abap_element,
      current_element_info   TYPE zif_acallh_ty_global=>ty_abap_element,
      incomplete_types_range TYPE RANGE OF string,
      incomplete_elems       TYPE zif_acallh_ty_global=>ty_abap_elements,
      result_elems_raw       TYPE zif_acallh_ty_global=>ty_abap_elements.

    METHODS:
      get_referenced_obj_from_data
        RETURNING
          VALUE(result) TYPE ty_ris_referenced_object
        RAISING
          cx_ris_exception,
      get_results
        IMPORTING
          referenced_object TYPE ty_ris_referenced_object
        EXPORTING
          ris_results       TYPE ris_t_results
        RAISING
          cx_adt_rest,
      get_individual_request
        IMPORTING
          referenced_object TYPE ty_ris_referenced_object
        RETURNING
          VALUE(result)     TYPE ris_s_parameter_where_used_s,
      process_results
        IMPORTING
          referenced_object TYPE ty_ris_referenced_object
          ris_results       TYPE ris_t_results,
      get_main_program
        IMPORTING
          referenced_object TYPE ty_ris_referenced_object
          include           TYPE programm
        RETURNING
          VALUE(result)     TYPE programm,
      convert_to_abap_elem
        IMPORTING
          referenced_object  TYPE ty_ris_referenced_object
          generic_ris_result TYPE sris
        RETURNING
          VALUE(result)      TYPE zif_acallh_ty_global=>ty_abap_element
        RAISING
          zcx_acallh_exception,
      extract_attr_from_ris_result
        IMPORTING
          generic_ris_result TYPE sris
          referenced_object  TYPE ty_ris_referenced_object
        RETURNING
          VALUE(result)      TYPE zif_acallh_ty_global=>ty_abap_element
        RAISING
          zcx_acallh_exception,
      set_full_name
        IMPORTING
          generic_ris_result TYPE sris
          referenced_object  TYPE ty_ris_referenced_object
        CHANGING
          abap_elem          TYPE zif_acallh_ty_global=>ty_abap_element,
      create_result_elements
        RETURNING
          VALUE(result) TYPE zif_acallh_abap_element=>ty_ref_tab,
      complete_elements.
ENDCLASS.



CLASS zcl_acallh_where_used_srv IMPLEMENTATION.

  METHOD constructor.
    me->abap_elem_fac = abap_elem_fac.

    incomplete_types_range = VALUE #( sign = 'I' option = 'EQ'
      ( low = zif_acallh_c_adt_type=>class_include )
      ( low = zif_acallh_c_adt_type=>include )
      ( low = zif_acallh_c_adt_type=>function_group_include ) ).
  ENDMETHOD.


  METHOD zif_acallh_where_used_srv~get_where_used_elements.
    CHECK abap_element->element_info-main_program IS NOT INITIAL.

    current_element = abap_element.
    current_element_info = abap_element->element_info.

    CLEAR: incomplete_elems,
           result_elems_raw.

    " start determination
    TRY.
        DATA(referenced_object) = get_referenced_obj_from_data( ).
        get_results( EXPORTING referenced_object = referenced_object
                     IMPORTING ris_results       = DATA(ris_results) ).
        process_results( referenced_object = referenced_object
                         ris_results       = ris_results ).
        complete_elements( ).
        result = create_result_elements( ).
      CATCH cx_adt_rest.
        "handle exception
      CATCH cx_ris_exception.
        "handle exception
    ENDTRY.

  ENDMETHOD.


  METHOD get_referenced_obj_from_data.
    DATA: name_parts TYPE string_table.

    result = VALUE #(
      full_name    = COND #( WHEN current_element_info-alias_full_name IS NOT INITIAL THEN
                               current_element_info-alias_full_name
                             ELSE
                               current_element_info-full_name )
      main_program = current_element_info-main_program
      object       = VALUE #(
        name           = current_element_info-object_name
        enclosing_name = current_element_info-encl_object_name
        type           = CORRESPONDING #( current_element_info-type ) )
      scope_object = VALUE #(
        name           = current_element_info-scope_object-object_name
        enclosing_name = current_element_info-scope_object-encl_object_name
        type           = VALUE #(
          trobjtype   = current_element_info-scope_object-trobjtype
          subtype     = current_element_info-scope_object-subtype
          legacy_type = current_element_info-scope_object-legacy_type ) ) ).


    " adjust enclosing/object if interface component detected
    IF result-object-name CS '~'.
      SPLIT result-object-name AT '~' INTO TABLE name_parts.
      result-object-name = name_parts[ 2 ].
      result-object-enclosing_name = name_parts[ 1 ].
    ENDIF.

    DATA(ris_metadata) = cl_ris_metadata_factory=>get_instance( ).
    ris_metadata->get_where_used( EXPORTING iv_trobjtype   = result-object-type-trobjtype
                                            iv_subtype     = result-object-type-subtype
                                            iv_legacy_type = result-object-type-legacy_type
                                  IMPORTING et_where_used  = DATA(raw_object_types) ).

    LOOP AT raw_object_types INTO DATA(raw_object_type) WHERE trobjtype = zif_acallh_c_tadir_type=>class OR
                                                              trobjtype = zif_acallh_c_tadir_type=>interface OR
                                                              trobjtype = zif_acallh_c_tadir_type=>program.
      DATA(object_type) = CORRESPONDING ty_referencing_object_type( raw_object_type ).
      DATA(meta_model) = ris_metadata->get_meta_model( iv_trobjtype   = object_type-trobjtype
                                                       iv_subtype     = object_type-subtype
                                                       iv_legacy_type = object_type-legacy_type ).
      CHECK meta_model IS BOUND.
      object_type-data_model = meta_model->get_data_model( ).
      INSERT object_type INTO TABLE result-referencing_object_types.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_results.
    DATA: referencing_object_type TYPE ty_referencing_object_type,
          individual_results      TYPE ris_t_results.

    DATA(individual_request) = get_individual_request( referenced_object ).

    LOOP AT referenced_object-referencing_object_types INTO referencing_object_type.

      CHECK referencing_object_type-data_model IS BOUND.

      CLEAR individual_results.

      TRY.
          referencing_object_type-data_model->execute_where_used( EXPORTING is_parameter_where_used = individual_request
                                                                  IMPORTING et_results              = individual_results ).
          LOOP AT individual_results INTO DATA(individual_result).
            CHECK individual_result->mo_data IS BOUND.
            individual_result->mo_requested_data_model = referencing_object_type-data_model.
            INSERT individual_result INTO TABLE ris_results.
          ENDLOOP.

        CATCH cx_ris_exception INTO DATA(ris_exception).
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_individual_request.

    result-find_objects = VALUE #( ( object      = referenced_object-object-name
                                     encl_object = referenced_object-object-enclosing_name ) ).
    result-find_type = CORRESPONDING #( referenced_object-object-type ).

    IF referenced_object-scope_object-enclosing_name IS NOT INITIAL
        OR referenced_object-scope_object-name IS NOT INITIAL.
      result-scope_objects = VALUE #( ( object      = referenced_object-scope_object-name
                                        encl_object = referenced_object-scope_object-enclosing_name ) ).
    ENDIF.

    result-scope_type = CORRESPONDING #( referenced_object-scope_object-type ).
    result-full_name = referenced_object-full_name.

  ENDMETHOD.


  METHOD process_results.
    DATA: generic_result TYPE sris.

    FIELD-SYMBOLS: <result_table> TYPE ANY TABLE.

    LOOP AT ris_results INTO DATA(ris_result).
      ASSIGN ris_result->mo_data->* TO <result_table>.
      IF sy-subrc = 0.

        LOOP AT <result_table> ASSIGNING FIELD-SYMBOL(<result_line>).
          generic_result = CORRESPONDING #( <result_line> ).

          CHECK generic_result-cntnd = abap_false.

          " filter out method implementation lines??, i.e.: METHOD some_method.
          IF generic_result-source CP '*method *'.
            CONTINUE.
          ENDIF.

          TRY.
              DATA(abap_elem) = convert_to_abap_elem( generic_ris_result = generic_result
                                                      referenced_object  = referenced_object ).
              IF abap_elem-adt_type IN incomplete_types_range.
                incomplete_elems = VALUE #( BASE incomplete_elems ( abap_elem ) ).
              ELSE.
                result_elems_raw = VALUE #( BASE result_elems_raw ( abap_elem ) ).
              ENDIF.
            CATCH zcx_acallh_exception.
              "handle exception
          ENDTRY.
        ENDLOOP.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_main_program.

*    IF referenced_object-main_program IS NOT INITIAL.
*      result = referenced_object-main_program.
*    ELSE.
    result = zcl_acallh_mainprog_resolver=>get_main_prog_by_include( include ).
*    ENDIF.

  ENDMETHOD.


  METHOD convert_to_abap_elem.

    result = extract_attr_from_ris_result(
      generic_ris_result = generic_ris_result
      referenced_object  = referenced_object ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = 'No valid result'.
    ELSEIF result-object_name = current_element_info-object_name AND
        result-encl_object_name = current_element_info-encl_object_name.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = 'No valid result'.
    ENDIF.

    set_full_name( EXPORTING generic_ris_result = generic_ris_result
                             referenced_object  = referenced_object
                   CHANGING  abap_elem          = result ).

    IF result-full_name IS NOT INITIAL AND
        result-tag = cl_abap_compiler=>tag_method.
      result-method_props = zcl_acallh_method_info_reader=>get_instance( )->read_properties( result-full_name ).
    ENDIF.
  ENDMETHOD.


  METHOD extract_attr_from_ris_result.

    result-parent_main_program = current_element_info-main_program.

    IF generic_ris_result-object IS NOT INITIAL.
      result-include = generic_ris_result-object.
      result-main_program = get_main_program( include           = result-include
                                              referenced_object = referenced_object ).
    ENDIF.

    IF result-main_program IS INITIAL AND generic_ris_result-program IS NOT INITIAL.
      result-include = generic_ris_result-program.
      result-main_program = get_main_program( include           = result-include
                                              referenced_object = referenced_object ).
    ENDIF.

    IF generic_ris_result-full_name IS NOT INITIAL.
      result-full_name = generic_ris_result-full_name.
    ELSEIF generic_ris_result-used_obj IS NOT INITIAL.
      result-full_name = generic_ris_result-used_obj.
    ENDIF.

    " filter out some irrelevant includes like definitions in classes
    IF result-include+30(1) = seop_inctype_class AND
        ( result-include+31(1) = seop_inccode_public OR
          result-include+31(1) = seop_inccode_protected OR
          result-include+31(1) = seop_inccode_private ).
      CLEAR result.
      RETURN.
    ENDIF.

    IF generic_ris_result-object_row > 0.
      result-call_positions = VALUE #( ( line = generic_ris_result-object_row ) ).
    ENDIF.

    IF generic_ris_result-object_cls = 'OM'.
      result-encl_object_name = cl_oo_classname_service=>get_clsname_by_include( result-main_program ).
      result-type = VALUE #( trobjtype   = zif_acallh_c_tadir_type=>class
                             legacy_type = zif_acallh_c_euobj_type=>method ).
      IF result-include+30(2) = 'CM'. " include is method
        result-object_name = cl_oo_classname_service=>get_method_by_include( result-include )-cpdname.
        result-adt_type = zif_acallh_c_adt_type=>method.
      ELSE.
        result-adt_type = zif_acallh_c_adt_type=>class_include.
      ENDIF.

    ELSEIF generic_ris_result-object_cls = 'P'.
      DATA(is_fugr_include) = VALUE abap_bool( ).
      DATA(is_function) = VALUE abap_bool( ).
      DATA(fugr_group) = VALUE  rs38l_area( ).

      CALL FUNCTION 'RS_PROGNAME_SPLIT'
        EXPORTING
          progname_with_namespace     = result-include
        IMPORTING
          fugr_is_include_name        = is_fugr_include
          fugr_is_functionmodule_name = is_function
          fugr_group                  = fugr_group
        EXCEPTIONS
          OTHERS                      = 1.
      IF sy-subrc = 0.
        IF is_function = abap_true.
          DATA(fugr_include) = result-include.
          DATA(function_name) = VALUE rs38l_fnam( ).
          CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
            CHANGING
              funcname = function_name
              include  = result-include
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc = 0.
            result-type = VALUE #( trobjtype   = zif_acallh_c_tadir_type=>function_group
                                   legacy_type = zif_acallh_c_euobj_type=>function ).
            result-object_name = function_name.
            result-adt_type = zif_acallh_c_adt_type=>function.
          ENDIF.
        ELSE.
          result-object_name = result-include.

          IF is_fugr_include = abap_true.
            result-adt_type = zif_acallh_c_adt_type=>function_group_include.
            result-encl_object_name = fugr_group.
          ELSEIF result-main_program = result-include.
            result-adt_type = zif_acallh_c_adt_type=>program.
          ELSE.
            result-adt_type = zif_acallh_c_adt_type=>include.
            result-encl_object_name = result-main_program.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      result-adt_type = zif_acallh_c_adt_type=>program.
      result-object_name = result-include.
    ENDIF.

  ENDMETHOD.


  METHOD set_full_name.
    DATA object_name_parts TYPE string_table.

    CASE abap_elem-adt_type.

      WHEN zif_acallh_c_adt_type=>function.
        abap_elem-tag = cl_abap_compiler=>tag_function.
        abap_elem-full_name = |\\{ abap_elem-tag }:{ abap_elem-object_name }|.

      WHEN zif_acallh_c_adt_type=>method.
        abap_elem-tag = cl_abap_compiler=>tag_method.

        SPLIT abap_elem-object_name AT '~' INTO TABLE object_name_parts.
        IF lines( object_name_parts ) = 2.
          abap_elem-full_name = |\\{ cl_abap_compiler=>tag_type }:| &&
            |{ abap_elem-encl_object_name }\\{ cl_abap_compiler=>tag_interface }:{ object_name_parts[ 1 ] }| &&
            |\\{ abap_elem-tag }:{ object_name_parts[ 2 ] }|.
        ELSE.
          abap_elem-full_name = |\\{ cl_abap_compiler=>tag_type }:| &&
                                |{ abap_elem-encl_object_name }\\{ abap_elem-tag }:{ abap_elem-object_name }|.
        ENDIF.

      WHEN OTHERS.
        " needs further determination
    ENDCASE.

  ENDMETHOD.


  METHOD complete_elements.
    " TODO: complete elements in table
    DATA: source_code TYPE string_table.

    LOOP AT incomplete_elems ASSIGNING FIELD-SYMBOL(<incomplete_elem_raw>) GROUP BY <incomplete_elem_raw>-main_program.
      CHECK <incomplete_elem_raw>-main_program IS NOT INITIAL.

      zcl_acallh_abap_compiler=>get( main_prog = <incomplete_elem_raw>-main_program ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_result_elements.

    LOOP AT result_elems_raw ASSIGNING FIELD-SYMBOL(<result_elem_raw>) GROUP BY <result_elem_raw>-full_name.
      DATA(call_positions) = VALUE zif_acallh_ty_global=>ty_call_positions( ).
      LOOP AT GROUP <result_elem_raw> ASSIGNING FIELD-SYMBOL(<group_elem>).
        call_positions = VALUE #( BASE call_positions ( LINES OF <group_elem>-call_positions ) ).
      ENDLOOP.

      SORT call_positions BY line.

      <result_elem_raw>-call_positions = call_positions.

      TRY.
          result = VALUE #( BASE result ( abap_elem_fac->create_abap_element( <result_elem_raw> ) ) ).
        CATCH zcx_acallh_exception.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
