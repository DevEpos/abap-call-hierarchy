"! <p class="shorttext synchronized" lang="en">Content handler factory</p>
CLASS zcl_acallh_ch_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates content handler call hierarchy result</p>
      create_call_hier_result_ch
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_acallh_ch_factory IMPLEMENTATION.

  METHOD create_call_hier_result_ch.
    result = cl_adt_rest_st_handler=>create_instance(
      st_name   = 'ZACALLH_CALL_HIER_RESULT'
      root_name = 'HIERARCHY_RESULT' ).
  ENDMETHOD.

ENDCLASS.
