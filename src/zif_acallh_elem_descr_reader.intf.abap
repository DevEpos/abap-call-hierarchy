"! <p class="shorttext synchronized" lang="en">Reads descriptions for elements</p>
INTERFACE zif_acallh_elem_descr_reader
  PUBLIC.

  "! <p class="shorttext synchronized" lang="en">Retrieves description for ABAP element</p>
  METHODS get_description
    IMPORTING
      elem_info     TYPE zif_acallh_ty_global=>ty_abap_element
    RETURNING
      VALUE(result) TYPE string.
ENDINTERFACE.
