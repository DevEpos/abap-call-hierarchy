"! <p class="shorttext synchronized" lang="en">Global Type Definitions</p>
INTERFACE zif_acallh_ty_global
  PUBLIC .

  TYPES:
    ty_visibility        TYPE c LENGTH 15,
    ty_method_impl_state TYPE c LENGTH 1,

    BEGIN OF ty_hierarchy_api_settings,
      use_first_intf_impl TYPE abap_bool,
      intf_impl           TYPE string,
    END OF ty_hierarchy_api_settings,

    "! Workbench object name
    BEGIN OF ty_wb_object_name,
      "! Name in TADIR
      name         TYPE sobj_name,
      display_name TYPE sobj_name,
    END OF ty_wb_object_name,

    "! Information about function module
    BEGIN OF ty_function_info,
      name    TYPE tfdir-funcname,
      group   TYPE rs38l_area,
      include TYPE tfdir-pname,
    END OF ty_function_info,

    BEGIN OF ty_source_position,
      line   TYPE i,
      column TYPE i,
    END OF ty_source_position,

    BEGIN OF ty_adt_uri_info,
      uri             TYPE string,
      main_prog       TYPE program,
      include         TYPE program,
      source_position TYPE ty_source_position,
      trobjtype       TYPE trobjtype,
    END OF ty_adt_uri_info,

    "! Source information of ABAP element
    BEGIN OF ty_ae_src_info,
      main_prog TYPE progname,
      include   TYPE progname,
      start_pos TYPE ty_source_position,
      end_pos   TYPE ty_source_position,
    END OF ty_ae_src_info,

    BEGIN OF ty_ris_data_request,
      "! Original RIS data request
      orig_request      TYPE ris_s_adt_data_request,
      uri               TYPE string,
      source_pos_of_uri TYPE ty_source_position,
    END OF ty_ris_data_request,

    BEGIN OF ty_fullname_part,
      tag  TYPE scr_tag,
      name TYPE string,
    END OF ty_fullname_part,

    ty_fullname_parts TYPE STANDARD TABLE OF ty_fullname_part WITH EMPTY KEY,

    ty_call_positions TYPE STANDARD TABLE OF ty_source_position WITH EMPTY KEY,

    BEGIN OF ty_method_properties,
      name           TYPE abap_methname,
      encl_type      TYPE trobjtype,
      alias_for      TYPE abap_methname,
      is_final       TYPE abap_bool,
      is_abstract    TYPE abap_bool,
      is_alias       TYPE abap_bool,
      is_redefined   TYPE abap_bool,
      is_handler     TYPE abap_bool,
      is_constructor TYPE abap_bool,
      is_static      TYPE abap_bool,
      is_test_method TYPE abap_bool,
      visibility     TYPE ty_visibility,
      impl_state     TYPE ty_method_impl_state,
    END OF ty_method_properties,

    BEGIN OF ty_abap_element,
      BEGIN OF type,
        legacy_type TYPE seu_stype,
        trobjtype   TYPE trobjtype,
        subtype     TYPE seu_objtyp,
      END OF type,
      adt_type              TYPE string,
      tag                   TYPE scr_tag,
      object_name           TYPE ris_parameter,
      encl_object_name      TYPE ris_parameter,
      encl_object_type      TYPE trobjtype,
      encl_obj_display_name TYPE ris_parameter,
      BEGIN OF scope_object,
        trobjtype        TYPE trobjtype,
        subtype          TYPE seu_objtyp,
        legacy_type      TYPE seu_stype,
        object_name      TYPE ris_parameter,
        encl_object_name TYPE ris_parameter,
      END OF scope_object,
      source_pos_start      TYPE ty_source_position,
      source_pos_end        TYPE ty_source_position,
      full_name             TYPE string,
      alias_full_name       TYPE string,
      description           TYPE string,
      method_props          TYPE ty_method_properties,
      include               TYPE progname,
      parent_main_program   TYPE progname,
      main_program          TYPE progname,
      call_positions        TYPE ty_call_positions,
    END OF ty_abap_element.

ENDINTERFACE.
