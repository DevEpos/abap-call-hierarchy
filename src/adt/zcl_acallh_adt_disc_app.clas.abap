"! <p class="shorttext synchronized" lang="en">Router for ABAP Call Hierarchy</p>
CLASS zcl_acallh_adt_disc_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS
      class_constructor.

    METHODS:
      if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      fill_router REDEFINITION,
      get_application_title REDEFINITION,
      register_resources REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      c_static_uri         TYPE string VALUE '/devepos/adt/aht',
      c_call_hierarchy_uri TYPE string VALUE '/callhierarchy',
      c_root_scheme        TYPE string VALUE 'http://www.devepos.com/adt/aht',
      c_root_rel_scheme    TYPE string VALUE 'http://www.devepos.com/adt/relations/aht',

      BEGIN OF c_handlers,
        call_hierarchy TYPE string VALUE 'ZCL_ACALLH_ADT_RES_CALL_HIER',
      END OF c_handlers.

    METHODS:
      register_call_hierarchy
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry.
ENDCLASS.



CLASS zcl_acallh_adt_disc_app IMPLEMENTATION.

  METHOD class_constructor.
  ENDMETHOD.


  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = c_static_uri.
  ENDMETHOD.


  METHOD fill_router.
    super->fill_router( CHANGING router = router ).
    router->attach(
      iv_template      = '/discovery'
      iv_handler_class = cl_adt_res_discovery=>co_class_name ).
  ENDMETHOD.


  METHOD get_application_title.
    result = 'ABAP Call Hierarchy'.
  ENDMETHOD.


  METHOD register_resources.
    register_call_hierarchy( registry ).
  ENDMETHOD.


  METHOD register_call_hierarchy.
    DATA(call_hierarchy_coll) = registry->register_discoverable_resource(
      url             = c_call_hierarchy_uri
      handler_class   = c_handlers-call_hierarchy
      description     = 'Call Hierarchy'
      category_scheme = c_root_scheme && c_call_hierarchy_uri
      category_term   = 'callHierarchy' ).

    DATA(template) = |{ c_call_hierarchy_uri }\{?{ zif_acallh_c_global=>c_call_hierarchy_params-uri }*\}|
                     .
    call_hierarchy_coll->register_disc_res_w_template(
      relation      = c_root_rel_scheme && c_call_hierarchy_uri
      template      = template
      handler_class = c_handlers-call_hierarchy ).
  ENDMETHOD.

ENDCLASS.
