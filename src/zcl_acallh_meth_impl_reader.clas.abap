"! <p class="shorttext synchronized" lang="en">Reader for method implmentations</p>
CLASS zcl_acallh_meth_impl_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_acallh_meth_impl_reader.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves instance of method impl. reader</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_meth_impl_reader.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO zif_acallh_meth_impl_reader.
ENDCLASS.



CLASS zcl_acallh_meth_impl_reader IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW zcl_acallh_meth_impl_reader( ).
    ENDIF.

    result = instance.
  ENDMETHOD.

ENDCLASS.
