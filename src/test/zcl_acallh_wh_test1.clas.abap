CLASS zcl_acallh_wh_test1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS prepare_commit.
    METHODS prepare_fetch.
    METHODS commit_transaction.
    METHODS start_transaction.
ENDCLASS.



CLASS zcl_acallh_wh_test1 IMPLEMENTATION.

  METHOD commit_transaction.
    DATA: bu TYPE REF TO zcl_acallh_wh_test2.

    bu->execute_calc( ).
    bu->run( ).
    bu->execute_calc( ).
  ENDMETHOD.

  METHOD start_transaction.
    commit_transaction( ).
    commit_transaction( ).
  ENDMETHOD.

  METHOD prepare_commit.
  ENDMETHOD.

  METHOD prepare_fetch.
  ENDMETHOD.

ENDCLASS.
