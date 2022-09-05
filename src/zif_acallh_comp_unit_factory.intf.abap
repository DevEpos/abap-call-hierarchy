"! <p class="shorttext synchronized" lang="en">Compilation unit factory</p>
INTERFACE zif_acallh_comp_unit_factory
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Creates compilation unit from data request</p>
    create_comp_unit_from_ext
      IMPORTING
        data_request  TYPE zif_acallh_ty_global=>ty_ris_data_request
      RETURNING
        VALUE(result) TYPE REF TO zif_acallh_compilation_unit
      RAISING
        zcx_acallh_exception,

    "! <p class="shorttext synchronized" lang="en">Creates compilation unit</p>
    create_comp_unit
      IMPORTING
        unit_data     TYPE zif_acallh_ty_global=>ty_compilation_unit
      RETURNING
        VALUE(result) TYPE REF TO zif_acallh_compilation_unit
      RAISING
        zcx_acallh_exception.
ENDINTERFACE.
