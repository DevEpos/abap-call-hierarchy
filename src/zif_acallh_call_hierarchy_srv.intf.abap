"! <p class="shorttext synchronized" lang="en">Call Hierarchy service</p>
INTERFACE zif_acallh_call_hierarchy_srv
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Determines the called units of the given comp. unit</p>
    determine_called_Elements
      IMPORTING
        abap_element  TYPE REF TO zif_acallh_abap_element
        settings      TYPE zif_acallh_ty_global=>ty_hierarchy_api_settings OPTIONAL
      RETURNING
        VALUE(result) TYPE zif_acallh_abap_element=>ty_ref_tab.
ENDINTERFACE.
