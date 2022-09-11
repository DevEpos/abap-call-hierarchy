"! <p class="shorttext synchronized" lang="en">Global constants for Call Hierarchy</p>
INTERFACE zif_acallh_c_global
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_call_hierarchy_params,
      uri                        TYPE string VALUE 'uri',
      auto_resolve_intf_method   TYPE string VALUE 'autoResolveIntfMethod',
      intf_method_implementation TYPE string VALUE 'intfMethodImpl',
    END OF c_call_hierarchy_params.
ENDINTERFACE.
