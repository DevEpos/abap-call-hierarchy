"! <p class="shorttext synchronized" lang="en">Object types in TADIR</p>
INTERFACE zif_acallh_c_tadir_type
  PUBLIC .

  CONSTANTS:
    function_group TYPE trobjtype VALUE 'FUGR' ##NO_TEXT,
    program        TYPE trobjtype VALUE 'PROG' ##NO_TEXT,
    class          TYPE trobjtype VALUE 'CLAS' ##NO_TEXT,
    interface      TYPE trobjtype VALUE 'INTF' ##NO_TEXT.
ENDINTERFACE.
