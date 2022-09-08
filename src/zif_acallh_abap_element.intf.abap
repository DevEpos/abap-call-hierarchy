"! <p class="shorttext synchronized" lang="en">Call hierarchy for a method/form/function</p>
INTERFACE zif_acallh_abap_element
  PUBLIC.

  TYPES:
    ty_ref_tab TYPE STANDARD TABLE OF REF TO zif_acallh_abap_element WITH EMPTY KEY.

  DATA:
    element_info TYPE zif_acallh_ty_global=>ty_abap_element READ-ONLY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns call position URI</p>
    get_call_position_uri
      IMPORTING
        position      TYPE zif_acallh_ty_global=>ty_source_position OPTIONAL
      RETURNING
        VALUE(result) TYPE string,
    "! <p class="shorttext synchronized" lang="en">Retrieves called ABAP elements</p>
    get_called_elements
      RETURNING
        VALUE(result) TYPE ty_ref_tab.
ENDINTERFACE.
