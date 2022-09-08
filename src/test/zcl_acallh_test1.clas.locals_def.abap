*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_local DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_acallh_test1.

    ALIASES alias_for_run FOR zif_acallh_test1~run.
  PROTECTED SECTION.
  PRIVATE SECTION.
    methods local_private.
ENDCLASS.
