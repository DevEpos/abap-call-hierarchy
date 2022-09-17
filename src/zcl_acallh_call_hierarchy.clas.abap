"! <p class="shorttext synchronized" lang="en">Call hierarchy for method/form/function</p>
CLASS zcl_acallh_call_hierarchy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_call_hierarchy_srv
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_call_hierarchy_srv,
      "! <p class="shorttext synchronized" lang="en">Retrieves compilation unit at URI</p>
      get_abap_element_from_uri
        IMPORTING
          uri           TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_abap_element
        RAISING
          zcx_acallh_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      hierarchy_srv TYPE REF TO zif_acallh_call_hierarchy_srv.
ENDCLASS.



CLASS zcl_acallh_call_hierarchy IMPLEMENTATION.

  METHOD get_abap_element_from_uri.
    DATA(element_info) = zcl_acallh_abap_elem_Mapper=>create( )->map_uri_to_abap_element( uri ).
    result = zcl_acallh_abap_element_fac=>get_instance( )->create_abap_element( element_info = element_info ).
  ENDMETHOD.


  METHOD get_call_hierarchy_srv.
    IF hierarchy_srv IS INITIAL.
      hierarchy_srv = NEW zcl_acallh_call_hierarchy_srv(
        abap_elem_factory = zcl_acallh_abap_element_fac=>get_instance( ) ).
    ENDIF.

    result = hierarchy_srv.
  ENDMETHOD.

ENDCLASS.
