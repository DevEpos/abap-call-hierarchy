"! <p class="shorttext synchronized" lang="en">Call Hierarchy service</p>
INTERFACE zif_acallh_call_hierarchy_srv
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Determines the called units of the given comp. unit</p>
    determine_called_units
      IMPORTING
        comp_unit     TYPE REF TO zif_acallh_compilation_unit
      RETURNING
        VALUE(result) TYPE zif_acallh_compilation_unit=>ty_ref_tab.
ENDINTERFACE.
