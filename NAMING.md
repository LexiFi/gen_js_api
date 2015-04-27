gen_js_api: default naming convention
=====================================

Javascript names corresponding to bound components can always be
specified explicitly (with the use of attributes).  When the naming is
left implicit, a Javascript name is automatically derived from the
OCaml name by applying the following rules:

  - uppercasing every character following an underscore;

  - removing every underscore;

  - uppercasing the first character when generating object constructor names.
