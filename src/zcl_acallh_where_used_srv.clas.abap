CLASS zcl_acallh_where_used_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieve</p>
      get_where_used_elements
        RETURNING
          VALUE(result) TYPE zif_acallh_abap_element=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_acallh_where_used_srv IMPLEMENTATION.

  METHOD get_where_used_elements.

  ENDMETHOD.

ENDCLASS.
