"! <p class="shorttext synchronized" lang="en">Global constants for Call Hierarchy</p>
INTERFACE zif_acallh_c_global
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_path_types,
      uri       TYPE string VALUE 'uri',
      full_name TYPE string VALUE 'fullName',
    END OF c_path_types,

    BEGIN OF c_call_hierarchy_params,
      path                       TYPE string VALUE 'path',
      path_type                  TYPE string VALUE 'pathType',
      full_name                  TYPE string VALUE 'fullName',
      auto_resolve_intf_method   TYPE string VALUE 'autoResolveIntfMethod',
      intf_method_implementation TYPE string VALUE 'intfMethodImpl',
    END OF c_call_hierarchy_params.
ENDINTERFACE.
