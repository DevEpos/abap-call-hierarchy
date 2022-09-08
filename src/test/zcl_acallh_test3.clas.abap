CLASS zcl_acallh_test3 DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PRIVATE
  INHERITING FROM zcl_acallh_test1.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS zif_acallh_test1~run REDEFINITION.
  PROTECTED SECTION.
    METHODS abstract1 ABSTRACT.
  PRIVATE SECTION.
    METHODS constructor.
    methods sub_test1.
    methods sub_test2.

ENDCLASS.



CLASS zcl_acallh_test3 IMPLEMENTATION.

  METHOD class_constructor.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD zif_acallh_test1~run.

  ENDMETHOD.

  METHOD sub_test1.

  ENDMETHOD.

  METHOD sub_test2.

  ENDMETHOD.

ENDCLASS.
