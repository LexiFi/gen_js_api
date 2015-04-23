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

//Provides: caml_js_wrap_callback_strict const
//Requires: caml_call_gen
function caml_js_wrap_callback_strict(arity, f) {
  return function () {
    var n = arguments.length;
    if(n == arity) return caml_call_gen(f, arguments);
    var args = new Array(arity);
    for (var i = 0; i < n && i < arity; i++) args[i] = arguments[i];
    return caml_call_gen(f, args);
  };
}
