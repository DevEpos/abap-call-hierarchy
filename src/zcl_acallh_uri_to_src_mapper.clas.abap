"! <p class="shorttext synchronized" lang="en">Maps URI to program/include</p>
CLASS zcl_acallh_uri_to_src_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_acallh_uri_to_src_mapper.

    CLASS-METHODS:
      create
        RETURNING
          VALUE(result) TYPE REF TO zif_acallh_uri_to_src_mapper.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_acallh_uri_to_src_mapper IMPLEMENTATION.

  METHOD create.
    result = NEW zcl_acallh_uri_to_src_mapper( ).
  ENDMETHOD.


  METHOD zif_acallh_uri_to_src_mapper~map_adt_uri_to_src.
    DATA: main    TYPE progname,
          include TYPE progname.

    DATA(uri_mapper) = lcl_uri_mapper_factory=>get_uri_mapper( uri ).
    IF uri_mapper IS INITIAL.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |URI does not conform to a valid source|.
    ENDIF.

    result = uri_mapper->map( ).

    IF result-main_prog IS NOT INITIAL.
      IF result-source_position IS INITIAL.
        result-source_position = zcl_acallh_adt_uri_util=>get_uri_source_start_pos( uri ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_acallh_exception
        EXPORTING
          text = |Main program could not be determined from URI|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
