"! <p class="shorttext synchronized" lang="en">Determines main program from object</p>
CLASS zcl_acallh_mainprog_resolver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Fills main program if still empty</p>
      "!
      "! @parameter element_info | Compilation unit data
      "! @parameter ignore_filled | if 'X' the main program will always be filled even if not empty. <br/>
      "!   This is currently only of relevance for method types (OM)
      resolve_main_prog
        IMPORTING
          element_info  TYPE REF TO zif_acallh_ty_global=>ty_abap_element
          ignore_filled TYPE abap_bool OPTIONAL
        RAISING
          zcx_acallh_exception,

      "! <p class="shorttext synchronized" lang="en">Resolves main program from ABAP full name</p>
      get_from_full_name
        IMPORTING
          full_name     TYPE string
        RETURNING
          VALUE(result) TYPE progname
        RAISING
          zcx_acallh_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_function_main_prog
        IMPORTING
          function_name TYPE rs38l_fnam
        RETURNING
          VALUE(result) TYPE progname,
      resolve_main_prog_om
        IMPORTING
          ignore_filled TYPE abap_bool
          element_info  TYPE REF TO zif_acallh_ty_global=>ty_abap_element
        RAISING
          zcx_acallh_exception.
ENDCLASS.



CLASS zcl_acallh_mainprog_resolver IMPLEMENTATION.

  METHOD resolve_main_prog.
    CASE element_info->legacy_type.

      WHEN swbm_c_type_function.
        element_info->main_program = get_function_main_prog( CONV #( element_info->object_name ) ).
        IF element_info->full_name IS INITIAL.
          element_info->full_name = |\\{ cl_abap_compiler=>tag_function }:{ element_info->object_name }|.
        ENDIF.

      WHEN swbm_c_type_cls_mtd_impl.
        resolve_main_prog_om( element_info  = element_info
                              ignore_filled = ignore_filled ).

      WHEN swbm_c_type_prg_subroutine.
        element_info->main_program = element_info->encl_object_name.

      WHEN swbm_c_type_prg_class_method.
        IF element_info->main_program IS INITIAL AND element_info->encl_object_type <> 'INTF'.
          element_info->main_program = element_info->encl_object_name.
        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD get_function_main_prog.
    DATA(funcname) = function_name.
    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      IMPORTING
        pname    = result
      CHANGING
        funcname = funcname
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.


  METHOD resolve_main_prog_om.
    CHECK ignore_filled = abap_true OR element_info->main_program IS INITIAL.

    IF element_info->encl_object_name+30(2) = 'CP'.
      element_info->main_program = element_info->encl_object_name.
      RETURN.
    ENDIF.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name      = element_info->encl_object_name
                                         RECEIVING  p_descr_ref = DATA(typedescr)
                                         EXCEPTIONS OTHERS      = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Type { element_info->encl_object_name } not found!|.
    ENDIF.

    DATA(class_typedescr) = CAST cl_abap_objectdescr( typedescr ).
    IF class_typedescr->kind = cl_abap_typedescr=>kind_class.
      element_info->main_program = cl_oo_classname_service=>get_classpool_name( CONV #( element_info->encl_object_name ) ).
    ELSE.
      " check if full name has the class name in the front
      DATA(name_parts) = zcl_acallh_fullname_util=>get_parts( element_info->full_name ).
      IF lines( name_parts ) >= 2 AND name_parts[ 2 ]-name = element_info->encl_object_name.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name      = name_parts[ 1 ]-name
                                             RECEIVING  p_descr_ref = DATA(encl_class_descr)
                                             EXCEPTIONS OTHERS      = 1 ).
        IF sy-subrc = 0 AND encl_class_descr->kind = cl_abap_typedescr=>kind_class.

          DATA(interfaces) = CAST cl_abap_classdescr( encl_class_descr )->interfaces.
          IF interfaces IS NOT INITIAL AND
              line_exists( interfaces[ name = element_info->encl_object_name ] ).
            element_info->main_program = cl_oo_classname_service=>get_classpool_name(
              CONV #( CAST cl_abap_objectdescr( encl_class_descr )->get_relative_name( ) ) ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_from_full_name.
    DATA class_type TYPE seoclstype.

    DATA(full_name_info) = zcl_acallh_fullname_util=>get_info_obj( full_name ).

    CASE full_name_info->get_abap_fullname_tag( ).

      WHEN cl_abap_compiler=>tag_method.
        full_name_info->get_all_parts( IMPORTING et_parts = DATA(all_parts) ).

        DATA(first_part) = all_parts[ 1 ].
        IF first_part-key = cl_abap_compiler=>tag_program.
          result = first_part-value.
        ELSE.
          DATA(classname) = CONV classname( first_part-value ).

          " check if type is class or interface
          CALL FUNCTION 'SEO_CLIF_EXISTENCE_CHECK'
            EXPORTING
              cifkey  = VALUE seoclskey( clsname = classname )
            IMPORTING
              clstype = class_type
            EXCEPTIONS
              OTHERS  = 1.
          IF sy-subrc = 0.
            IF class_type = seoc_clstype_class.
              result = cl_oo_classname_service=>get_classpool_name( classname ).
            ELSE.
              result = cl_oo_classname_service=>get_interfacepool_name( classname ).
            ENDIF.
          ENDIF.

        ENDIF.

      WHEN cl_abap_compiler=>tag_form.
        result = full_name_info->get_first_part( )-value.

      WHEN cl_abap_compiler=>tag_function.
        result = get_function_main_prog( CONV #( full_name_info->get_first_part( )-value ) ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
