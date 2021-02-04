//Provides: caml_ojs_wrap_fun_arguments
//Requires: caml_js_wrap_callback
function caml_ojs_wrap_fun_arguments(f) {
  return function() {
    return caml_js_wrap_callback(f)(arguments);
  }
}
