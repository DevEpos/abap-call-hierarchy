"! <p class="shorttext synchronized" lang="en">Service to determine Where-Used Hierarchy</p>
INTERFACE zif_acallh_where_used_srv
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieve</p>
    get_where_used_elements
      IMPORTING
        abap_element  TYPE REF TO zif_acallh_abap_element
      RETURNING
        VALUE(result) TYPE zif_acallh_abap_element=>ty_ref_tab.

ENDINTERFACE.
