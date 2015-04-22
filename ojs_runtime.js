//Provides: caml_ojs_wrap_fun_arguments
//Requires: caml_js_wrap_callback
function caml_ojs_wrap_fun_arguments(f) {
  return function() {
    return caml_js_wrap_callback(f)(arguments);
  }
}

//Provides: caml_ojs_iterate_properties
//Requires: caml_js_to_string
function caml_ojs_iterate_properties(o, f) {
  var name;
  for(name in o) {
    if(o.hasOwnProperty(name)) {
      f(caml_js_to_string(name));
    }
  }
}
