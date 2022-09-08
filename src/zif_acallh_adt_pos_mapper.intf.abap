"! <p class="shorttext synchronized" lang="en">Mapper of positions in Source Code</p>
INTERFACE zif_acallh_adt_pos_mapper
  PUBLIC.
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Maps given URI to sructure of ABAP element</p>
    "! A valid ABAP element is either a method a function module or a form. <br/>
    "! No other ABAP elements are supported at this time
    map_uri_to_abap_element
      IMPORTING
        uri           TYPE string
      RETURNING
        VALUE(result) TYPE zif_acallh_ty_global=>ty_abap_element
      RAISING
        zcx_acallh_exception.

ENDINTERFACE.
