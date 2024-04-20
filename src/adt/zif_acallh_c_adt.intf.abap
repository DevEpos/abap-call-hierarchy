"! <p class="shorttext synchronized">Constants for ADT</p>
INTERFACE zif_acallh_c_adt
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_hierarchy_mode,
      callees TYPE string VALUE 'callees',
      callers TYPE string VALUE 'callers',
    END OF c_hierarchy_mode.

  CONSTANTS:
    BEGIN OF c_path_types,
      uri       TYPE string VALUE 'uri',
      full_name TYPE string VALUE 'fullName',
    END OF c_path_types.

  CONSTANTS:
    BEGIN OF c_call_hierarchy_params,
      mode                       TYPE string VALUE 'mode',
      path                       TYPE string VALUE 'path',
      path_type                  TYPE string VALUE 'pathType',
      full_name                  TYPE string VALUE 'fullName',
      auto_resolve_intf_method   TYPE string VALUE 'autoResolveIntfMethod',
      intf_method_implementation TYPE string VALUE 'intfMethodImpl',
    END OF c_call_hierarchy_params.
ENDINTERFACE.
