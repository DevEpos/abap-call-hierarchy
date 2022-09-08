"! <p class="shorttext synchronized" lang="en">Types for ADT communication</p>
INTERFACE zif_acallh_ty_adt
  PUBLIC.

  TYPES BEGIN OF ty_method_properties.
  INCLUDE TYPE zif_acallh_ty_global=>ty_method_properties.
  TYPES END OF ty_method_properties.

  TYPES:
    BEGIN OF ty_adt_obj_ref,
      "! URI - mandatory client response, optional for client request
      uri          TYPE string,
      "! URI to parent object
      parent_uri   TYPE string,
      "! Name of the referenced entity - optional
      name         TYPE string,
      "! Description of the referenced entity - optional
      description  TYPE string,
      "! ADT Type of the referenced entity - optional
      type         TYPE string,
      "! Package name of the referenced entity- optional
      package_name TYPE string,
      "! Owner of the referenced entity - optional
      owner        TYPE string,
    END OF ty_adt_obj_ref,


    BEGIN OF ty_call_position,
      line   TYPE i,
      column TYPE i,
      uri    TYPE string,
    END OF ty_call_position,

    ty_call_positions TYPE STANDARD TABLE OF ty_call_position WITH EMPTY KEY,

    BEGIN OF ty_abap_element,
      object_ref            TYPE ty_adt_obj_ref,
      "! Enclosing object name (e.g. Interface, Class, Function Group)
      encl_obj_name         TYPE string,
      encl_obj_display_name TYPE string,
      method_props          TYPE ty_method_properties,
      call_positions        TYPE ty_call_positions,
    END OF ty_abap_element,

    ty_abap_elements TYPE STANDARD TABLE OF ty_abap_element WITH EMPTY KEY,

    BEGIN OF ty_call_hierarchy_result,
      origin_type             TYPE string,
      origin_object_name      TYPE string,
      origin_encl_object_name TYPE string,
      entries TYPE ty_abap_elements,
    END OF ty_call_hierarchy_result.

ENDINTERFACE.
