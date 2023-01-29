*&---------------------------------------------------------------------*
*& Report zacallh_test1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zacallh_test1.


CLASS lcl_class1 DEFINITION.

  PUBLIC SECTION.
    METHODS:
      some_pub_method.
  PROTECTED SECTION.
    METHODS:
      some_prot_method.
  PRIVATE SECTION.
    METHODS:
      some_priv_method.
ENDCLASS.

CLASS lcl_class2 DEFINITION.

  PUBLIC SECTION.
    METHODS:
      some_pub_method.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_class2 IMPLEMENTATION.

  METHOD some_pub_method.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_class1 IMPLEMENTATION.

  METHOD some_priv_method.
    some_prot_method( ).
    DATA(ref) = NEW lcl_class2( ).
    ref->some_pub_method( ).
  ENDMETHOD.

  METHOD some_prot_method.
    CALL FUNCTION 'RS_PROGNAME_SPLIT'
      EXPORTING
        progname_with_namespace = 'test'
      EXCEPTIONS
        delimiter_error         = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.

  METHOD some_pub_method.
    some_priv_method( ).
  ENDMETHOD.

ENDCLASS.

FORM my_form
              USING test.
  NEW lcl_class1( )->some_pub_method( ).
ENDFORM.

START-OF-SELECTION.
  NEW lcl_class1( )->some_pub_method( ).

end-of-SELECTION.
  NEW lcl_class1( )->some_pub_method( ).
