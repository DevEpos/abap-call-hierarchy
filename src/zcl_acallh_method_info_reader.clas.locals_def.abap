*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
"! Info reader that uses RTTI
CLASS lcl_rtti_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_acallh_method_info_reader.

    METHODS:
      constructor
        IMPORTING
          fallback_reader TYPE REF TO zif_acallh_method_info_reader OPTIONAL.
  PRIVATE SECTION.
    CONSTANTS:
      c_class_constructor_name TYPE string VALUE 'CLASS_CONSTRUCTOR' ##NO_TEXT,
      c_constructor_name       TYPE string VALUE 'CONSTRUCTOR' ##NO_TEXT,
      c_rtti_intf_type         TYPE string VALUE '\INTERFACE=' ##NO_TEXT,
      c_rtti_class_type        TYPE string VALUE '\CLASS=' ##NO_TEXT,
      c_rtti_progr_type        TYPE string VALUE '\PROGRAM=' ##NO_TEXT.

    DATA: fallback_reader TYPE REF TO zif_acallh_method_info_reader.

    METHODS:
      get_method_name
        IMPORTING
          full_name_info TYPE REF TO if_ris_abap_fullname
        RETURNING
          VALUE(result)  TYPE abap_methname,
      get_type_descr
        IMPORTING
          full_name_info TYPE REF TO if_ris_abap_fullname
        RETURNING
          VALUE(result)  TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_acallh_exception,
      get_possible_rtti_type_names
        IMPORTING
          full_name_info TYPE REF TO if_ris_abap_fullname
        RETURNING
          VALUE(result)  TYPE string_table,
      fill_method_properties
        IMPORTING
          method_name   TYPE abap_methname
          object_descr  TYPE REF TO cl_abap_objectdescr
        RETURNING
          VALUE(result) TYPE zif_acallh_ty_global=>ty_method_properties
        RAISING
          zcx_acallh_exception,
    run_fallback_reader
      IMPORTING
        full_name  TYPE string
      CHANGING
          meth_props TYPE zif_acallh_ty_global=>ty_method_properties.
ENDCLASS.

"! Info reader that uses the ABAP Parser
CLASS lcl_abap_parser_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_acallh_method_info_reader.

    CONSTANTS: c_calc_method_name TYPE string VALUE 'CALCULATE_ELEMENT_INFO_BY_NAME'.

    METHODS:
      constructor.
  PRIVATE SECTION.
    DATA: abap_parser TYPE REF TO cl_abap_parser.
ENDCLASS.
