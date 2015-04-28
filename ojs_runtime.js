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


//Provides: caml_ojs_new_arr
function caml_ojs_new_arr(c, a) {
  switch (a.length) {
  case 0: return new c;
  case 1: return new c (a[0]);
  case 2: return new c (a[0],a[1]);
  case 3: return new c (a[0],a[1],a[2]);
  case 4: return new c (a[0],a[1],a[2],a[3]);
  case 5: return new c (a[0],a[1],a[2],a[3],a[4]);
  case 6: return new c (a[0],a[1],a[2],a[3],a[4],a[5]);
  case 7: return new c (a[0],a[1],a[2],a[3],a[4],a[5],a[6]);
  }
  function F() { return c.apply(this, a); }
  F.prototype = c.prototype;
  return new F;
}
