"! <p class="shorttext synchronized" lang="en">Method visibility</p>
INTERFACE zif_acallh_c_method_visibility
  PUBLIC.

  CONSTANTS:
    public    TYPE zif_acallh_ty_global=>ty_visibility VALUE 'public',
    protected TYPE zif_acallh_ty_global=>ty_visibility VALUE 'protected',
    private   TYPE zif_acallh_ty_global=>ty_visibility VALUE 'private'.

ENDINTERFACE.
