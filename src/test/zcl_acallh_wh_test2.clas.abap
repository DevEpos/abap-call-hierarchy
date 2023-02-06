CLASS zcl_acallh_wh_test2 DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      execute_calc,
      run.
  PROTECTED SECTION.
    METHODS prepare.
    METHODS complete.
    METHODS push_to_db.
    METHODS push_to_provider ABSTRACT.
  PRIVATE SECTION.
    METHODS convert_to_number.
    METHODS convert_to_string.
    METHODS is_necessary
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS conv_1.
    METHODS conv_2.
    METHODS conv_3.
ENDCLASS.



CLASS zcl_acallh_wh_test2 IMPLEMENTATION.

  METHOD execute_calc.
    prepare( ).
    complete( ).
  ENDMETHOD.

  METHOD run.
    IF is_necessary( ).
      complete( ).
    ENDIF.
  ENDMETHOD.

  METHOD complete.
    convert_to_string( ).
    push_to_db( ).
    push_to_provider( ).
    push_to_db( ).
  ENDMETHOD.

  METHOD convert_to_number.
    CALL FUNCTION 'ZDBBR_START'.
  ENDMETHOD.

  METHOD convert_to_string.
    CALL FUNCTION 'ZDBBR_START'.
    convert_to_number( ).
    convert_to_number( ).
    convert_to_number( ).
    convert_to_number( ).
  ENDMETHOD.

  METHOD prepare.
    convert_to_string( ).
  ENDMETHOD.


  METHOD is_necessary.
  ENDMETHOD.

  METHOD push_to_db.
  ENDMETHOD.

  METHOD conv_1.
    convert_to_number( ).
  ENDMETHOD.

  METHOD conv_2.
    convert_to_number( ).
  ENDMETHOD.

  METHOD conv_3.
    convert_to_number( ).
  ENDMETHOD.

ENDCLASS.
