"! <p class="shorttext synchronized" lang="en">Reads element information via ADT RFC</p>
INTERFACE zif_acallh_adt_eleminfo_reader
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Reads method properties</p>
    read_method_properties
      IMPORTING
        uri           TYPE string
      RETURNING
        VALUE(result) TYPE zif_acallh_ty_adt=>ty_method_properties
      RAISING
        zcx_acallh_exception.
ENDINTERFACE.
