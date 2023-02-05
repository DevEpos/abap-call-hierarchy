"! <p class="shorttext synchronized" lang="en">Reads method information</p>
CLASS zcl_acallh_method_info_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_reader_type TYPE c LENGTH 1.
    CONSTANTS:
      BEGIN OF c_reader_type,
        abap_parser TYPE ty_reader_type VALUE 'p',
        rtti        TYPE ty_reader_type VALUE 'r',
      END OF c_reader_type.

    CLASS-METHODS get_instance
      IMPORTING
        type          TYPE ty_reader_type DEFAULT c_reader_type-rtti
      RETURNING
        VALUE(result) TYPE REF TO zif_acallh_method_info_reader.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      rtti_reader               TYPE REF TO zif_acallh_method_info_reader,
      parser_reader             TYPE REF TO zif_acallh_method_info_reader,
      has_parser_reader_support TYPE abap_bool VALUE abap_undefined.

    CLASS-METHODS:
      system_supports_parser
        RETURNING
          VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS zcl_acallh_method_info_reader IMPLEMENTATION.

  METHOD get_instance.

    CASE type.

      WHEN c_reader_type-rtti.
        IF rtti_reader IS INITIAL.
          rtti_reader = NEW lcl_rtti_reader(
            fallback_reader = get_instance( type = c_reader_type-abap_parser ) ).
        ENDIF.
        result = rtti_reader.

      WHEN c_reader_type-abap_parser.
        IF parser_reader IS INITIAL AND system_supports_parser( ).
          parser_reader = NEW lcl_abap_parser_reader( ).
        ENDIF.
        result = parser_reader.

    ENDCASE.

  ENDMETHOD.


  METHOD system_supports_parser.
    DATA: abap_parser_ref TYPE REF TO cl_abap_parser.

    IF has_parser_reader_support = abap_undefined.
      has_parser_reader_support = abap_false.
      DATA(ref_descr) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( abap_parser_ref ) ).
      DATA(parser_descr) = CAST cl_abap_classdescr( ref_descr->get_referenced_type( ) ).
      DATA(calc_method) = VALUE abap_methdescr( parser_descr->methods[
        name = lcl_abap_parser_reader=>c_calc_method_name ] OPTIONAL ).

      IF calc_method IS NOT INITIAL.
        " find 'fullname' parameter (necessary??)
        IF line_exists( calc_method-parameters[ name = 'FULLNAME' ] ).
          has_parser_reader_support = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    result = has_parser_reader_support.
  ENDMETHOD.

ENDCLASS.
