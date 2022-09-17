"! <p class="shorttext synchronized" lang="en">Compilation unit</p>
CLASS zcl_acallh_abap_element DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_acallh_abap_element_fac.

  PUBLIC SECTION.
    INTERFACES zif_acallh_abap_element.

    METHODS:
      constructor
        IMPORTING
          hierarchy_service TYPE REF TO zif_acallh_call_hierarchy_srv
          data              TYPE zif_acallh_ty_global=>ty_abap_element
        RAISING
          zcx_acallh_exception,

      set_hierarchy_possible
        IMPORTING
          value TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
      element_info FOR zif_acallh_abap_element~element_info.

    TYPES:
      BEGIN OF ty_comp_unit_by_line,
        line TYPE i,
        ref  TYPE REF TO zif_acallh_abap_element,
      END OF ty_comp_unit_by_line,

      BEGIN OF ty_ref_entry,
        type TYPE scr_tag,
        name TYPE string,
      END OF ty_ref_entry,

      ty_ref_stack TYPE STANDARD TABLE OF ty_ref_entry WITH EMPTY KEY.

    DATA:
      hierarchy_service          TYPE REF TO zif_acallh_call_hierarchy_srv,

      is_hierarchy_possible      TYPE abap_bool,
      is_called_units_determined TYPE abap_bool,
      called_units               TYPE zif_acallh_abap_element=>ty_ref_tab.
ENDCLASS.



CLASS zcl_acallh_abap_element IMPLEMENTATION.

  METHOD constructor.
    element_info = data.
    me->hierarchy_service = hierarchy_service.
    is_hierarchy_possible = abap_true.
  ENDMETHOD.


  METHOD set_hierarchy_possible.
    is_hierarchy_possible = value.
  ENDMETHOD.

  METHOD zif_acallh_abap_element~get_called_elements.
    IF force_reset = abap_true.
      is_called_units_determined = abap_false.
    ENDIF.

    IF is_called_units_determined = abap_false.
      IF is_hierarchy_possible = abap_true.
        called_units = hierarchy_service->determine_called_Elements( abap_element = me
                                                                  settings     = settings ).
      ENDIF.
      is_called_units_determined = abap_true.
    ENDIF.

    result = called_units.
  ENDMETHOD.

  METHOD zif_acallh_abap_element~get_call_position_uri.
**********************************************************************
    " 1) direct jump to definition of form

**cl_wb_request=>create_from_encl_name(
**  EXPORTING
**    p_object_type      = 'PU'
**    p_encl_object_name = 'SAPLSEUA' " include name
**    p_object_name      = 'INFOSYSTEM_OBJECT_PROGRAM_GET' " Form name
**    p_operation        = 'DISPLAY'
**  RECEIVING
**    p_wb_request       = wb_request
**  EXCEPTIONS
**    OTHERS             = 1
**).

**********************************************************************
    IF element_info-include IS INITIAL.
      RETURN.
    ENDIF.

    DATA(adt_tools_factory) = cl_adt_tools_core_factory=>get_instance( ).
    DATA(uri_mapper) = adt_tools_factory->get_uri_mapper( ).

    IF element_info-parent_main_program IS INITIAL AND
        element_info-main_program IS NOT INITIAL AND
        element_info-source_pos_start IS NOT INITIAL.

      DATA(line) = element_info-source_pos_start-line.
      DATA(col) = element_info-source_pos_start-column.
      DATA(prog) = element_info-main_program.
    ELSEIF element_info-parent_main_program IS NOT INITIAL AND
         element_info-call_positions IS NOT INITIAL.

      IF position IS NOT INITIAL.
        line = position-line.
        col = position-column.
      ELSE.
        line = element_info-call_positions[ 1 ]-line.
        col = element_info-call_positions[ 1 ]-column.
      ENDIF.

      prog = element_info-parent_main_program.
    ENDIF.

    DATA(mapping_options) = adt_tools_factory->create_mapping_options( ).

    TRY.
        DATA(obj_ref) = uri_mapper->map_include_to_objref(
          program     = prog
          include     = element_info-include
          line        = line
          line_offset = col ).
        result = obj_ref->ref_data-uri.
      CATCH cx_adt_uri_mapping.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
