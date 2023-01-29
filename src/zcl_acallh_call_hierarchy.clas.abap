"! <p class="shorttext synchronized" lang="en">Call hierarchy for method/form/function</p>
CLASS zcl_acallh_call_hierarchy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves hierarchy service instance</p>
      get_call_hierarchy_srv
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_call_hierarchy_srv,
      get_where_used_hierarchy_srv
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_where_used_srv,
      "! <p class="shorttext synchronized" lang="en">Retrieves ABAP element at URI</p>
      get_abap_element_from_uri
        IMPORTING
          uri           TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_abap_element
        RAISING
          zcx_acallh_exception,
      "! <p class="shorttext synchronized" lang="en">Retrieves ABAP element via full name identifier</p>
      get_abap_elem_from_full_name
        IMPORTING
          full_name     TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_abap_element
        RAISING
          zcx_acallh_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      hierarchy_srv       TYPE REF TO zif_acallh_call_hierarchy_srv,
      where_used_hier_srv TYPE REF TO zif_acallh_where_used_srv.
ENDCLASS.



CLASS zcl_acallh_call_hierarchy IMPLEMENTATION.

  METHOD get_abap_element_from_uri.
    DATA(element_info) = zcl_acallh_abap_elem_mapper=>create( )->map_uri_to_abap_element( uri ).
    result = zcl_acallh_abap_element_fac=>get_instance( )->create_abap_element( element_info = element_info ).
  ENDMETHOD.


  METHOD get_abap_elem_from_full_name.
    DATA(element_info) = zcl_acallh_abap_elem_mapper=>create( )->map_full_name_to_abap_element(
      full_name = full_name
      main_prog = zcl_acallh_mainprog_resolver=>get_from_full_name( full_name ) ).

    result = zcl_acallh_abap_element_fac=>get_instance( )->create_abap_element( element_info = element_info ).
  ENDMETHOD.


  METHOD get_call_hierarchy_srv.
    IF hierarchy_srv IS INITIAL.
      hierarchy_srv = NEW zcl_acallh_call_hierarchy_srv(
        abap_elem_factory = zcl_acallh_abap_element_fac=>get_instance( ) ).
    ENDIF.

    result = hierarchy_srv.
  ENDMETHOD.


  METHOD get_where_used_hierarchy_srv.
    IF where_used_hier_srv IS INITIAL.
      where_used_hier_srv = NEW zcl_acallh_where_used_srv(
        abap_elem_fac = zcl_acallh_abap_element_fac=>get_instance( ) ).
    ENDIF.

    result = where_used_hier_srv.
  ENDMETHOD.

ENDCLASS.
