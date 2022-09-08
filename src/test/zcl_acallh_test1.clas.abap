CLASS zcl_acallh_test1 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_acallh_test1.
    METHODS test1.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES my_alias FOR zif_acallh_test1~run.

    METHODS test2.
    METHODS test3.
    METHODS test4.
    methods test5.
    methods test6.
    methods test7.
ENDCLASS.



CLASS zcl_acallh_test1 IMPLEMENTATION.

  METHOD test1.
    DATA(local_class) = NEW lcl_local( ).

    local_class->alias_for_run( ).

    zif_acallh_test1~run( ).
  ENDMETHOD.

  METHOD zif_acallh_test1~run.
    DATA(local_class) = NEW lcl_local( ).
    local_class->zif_acallh_test1~run( ).

    cl_wb_object=>create_from_global_type(
      p_object_type = VALUE #( objtype_tr = 'CLAS' )
    ).
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.


  method zif_acallh_test1~test.
  ENDMETHOD.


  METHOD test2.
    my_alias( ).

    my_alias( ).
  ENDMETHOD.

  METHOD test3.
    DATA intf_ref_with_one_class TYPE REF TO zif_acallh_test2.
    DATA test_class2_ref TYPE REF TO zcl_acallh_test2.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type = 'OM'.

    intf_ref_with_one_class->execute( ).

    test_class2_ref->zif_acallh_test2~execute( ).
  ENDMETHOD.


  METHOD test4.
    PERFORM save_for_recurrence IN PROGRAM saplseua.
  ENDMETHOD.


  METHOD test5.

  ENDMETHOD.


  METHOD test6.

  ENDMETHOD.


  METHOD test7.

  ENDMETHOD.

ENDCLASS.
