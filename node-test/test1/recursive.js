var Foo = /*#__PURE__*/function () {
  "use strict";

  function Foo(name) {
    this.name = name;
  }

  var _proto = Foo.prototype;

  _proto.describe = function describe() {
    return "Foo:".concat(this.name);
  };

  _proto.toBar = function toBar() {
    return new Bar(this.name);
  };

  return Foo;
}();

var Bar = /*#__PURE__*/function () {
  "use strict";

  function Bar(name) {
    this.name = name;
  }

  var _proto2 = Bar.prototype;

  _proto2.describe = function describe() {
    return "Bar:".concat(this.name);
  };

  _proto2.toFoo = function toFoo() {
    return new Foo(this.name);
  };

  return Bar;
}();

joo_global_object.Foo = Foo
joo_global_object.Bar = Bar
