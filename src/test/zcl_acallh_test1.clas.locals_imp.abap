*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_local IMPLEMENTATION.

  METHOD zif_acallh_test1~run.
    DATA: intf_ref TYPE REF TO if_adt_rest_authorization.

    TRY.
        intf_ref->check_authority( '/sap/uri' ).
      CATCH cx_adt_res_no_authority.
    ENDTRY.
  ENDMETHOD.

  METHOD local_private.

  ENDMETHOD.

ENDCLASS.
