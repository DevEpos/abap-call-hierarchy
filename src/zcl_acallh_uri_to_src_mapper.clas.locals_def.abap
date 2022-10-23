*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_uri_mapper.
  METHODS map
    RETURNING
      VALUE(result) TYPE zif_acallh_ty_global=>ty_adt_uri_info
    RAISING
      zcx_acallh_exception.
ENDINTERFACE.

CLASS lcl_uri_mapper_factory DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_uri_mapper
      IMPORTING
        uri           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO lif_uri_mapper.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_class_uri_mapper DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_uri_mapper.
    METHODS constructor
      IMPORTING
        uri TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA uri TYPE string.
    CONSTANTS:
      c_class_uri_regex TYPE string VALUE `^/sap/bc/adt/oo/classes/([\w%]+)/(source/main|includes)/?(definitions|implementations|testclasses)?`.
ENDCLASS.


CLASS lcl_intf_uri_mapper DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_uri_mapper.
    METHODS constructor
      IMPORTING
        uri TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA uri TYPE string.
    CONSTANTS:
      c_intf_uri_regex TYPE string VALUE `^/sap/bc/adt/oo/interfaces/([\w%]+)/source/main`.
ENDCLASS.


CLASS lcl_fugr_uri_mapper DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_uri_mapper.
    METHODS constructor
      IMPORTING
        uri TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA uri TYPE string.
    CONSTANTS:
      c_fugr_uri_regex  TYPE string VALUE `^/sap/bc/adt/functions/groups/([\w%]+)/(includes|fmodules)/([\w%]+)/source/main`.
ENDCLASS.


CLASS lcl_prog_uri_mapper DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_uri_mapper.
    METHODS constructor
      IMPORTING
        uri TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA uri TYPE string.
    CONSTANTS:
      c_prog_uri_regex  TYPE string VALUE `^/sap/bc/adt/programs/(includes|programs)/([\w%]+)/source/main`.
ENDCLASS.
