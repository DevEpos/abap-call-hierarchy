"! <p class="shorttext synchronized" lang="en">Method info reader</p>
INTERFACE zif_acallh_method_info_reader
  PUBLIC.

  "! <p class="shorttext synchronized" lang="en">Reads method properties</p>
  METHODS read_properties
    IMPORTING
      full_name     TYPE string
    RETURNING
      VALUE(result) TYPE zif_acallh_ty_global=>ty_method_properties
    RAISING
      zcx_acallh_exception.
ENDINTERFACE.
