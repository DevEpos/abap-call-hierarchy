"! <p class="shorttext synchronized" lang="en">Maps URI to program/include</p>
INTERFACE zif_acallh_uri_to_src_mapper
  PUBLIC.

  "! <p class="shorttext synchronized" lang="en">Maps an ADT URI to the include/program of its origin</p>
  METHODS map_adt_uri_to_src
    IMPORTING
      uri           TYPE string
    RETURNING
      VALUE(result) TYPE zif_acallh_ty_global=>ty_adt_uri_info
    RAISING
      zcx_acallh_exception.

ENDINTERFACE.
