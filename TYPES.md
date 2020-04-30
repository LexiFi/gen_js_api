Types supported in gen_js_api
=============================

JS-able types
-------------

A JS-able type is an OCaml type whose values can be mapped to and from
Javascript objects.

The following types are supported out-of-the-box:

 - Basic built-in types: `string`, `int`, `bool`, `float` and `Ojs.t`.

 - Tuples of JS-able types (mapped to JS arrays).

 - Sequences of JS-able types: `array` and `list`, both mapped to JS
   arrays (which are assumed to be indexed by integers 0..length-1).

 - Options on JS-able types.  They are mapped to the same type as
   their parameter: `None` is mapped to JS `null` value, and both
   `null` and `undefined` are mapped back to `None`.  This encoding
   doesn't support nested options in a faithful way.

 - Arrows (see section below).

 - Polymorphic variants with only constant variants are supported
   (see the section on enums below).

 - Polymorphic variants can also be used to encode non-discriminated
   unions on the JS side (see the section on union types below).

 - Polymorphic variants can also be used to encode discriminated
   unions on the JS side (see the section on discriminated union types
   below).

An arbitrary non-parametrized type with path `M.t` is JS-able if the
following two values are available in module `M`:

```ocaml
val t_to_js: t -> Ojs.t
val t_of_js: Ojs.t -> t
```

The name of these values is obtained by appending `_of_js` or `_to_js`
to the local name of the type.  It is thus possible to define JS-able
manually by defining these two functions.  Type and class declarations
processed by gen_js_api (see sections below) create JS-able type (by
generating those functions automatically).

Parametrized types can also be JS-able.  It is currently assumed that
such types are covariant in each of their parameter.  Mapping
functions take extra arguments corresponding to the mapper for each
parameter.  For instance, a type `'a t` would need to come with the following
functions:

```ocaml
val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
```

Arrow types
-----------

Arrow types can also be used in contexts that expect JS-able types.
All arguments must be JS-able types, and a final `unit`
pseudo-argument is allowed (and mandatory when there is no real
argument).  The function's result can be either a JS-able type or
`unit`.  Note that `unit` is not considered as a proper JS-able type:
it is only allowed in these two contexts (as the result, or the final
pseudo-argument).

Arguments can be **labelled or optional**.  Labels are simply ignored
on the JS side.  Optional arguments have different treatments:

 - When mapping an OCaml function to JS (e.g. a callback), optional
   arguments are treated as normal values of an option type (i.e.
   both `null` and `undefined` are mapped to `None`).

 - When mapping a JS function to OCaml, it is possible to specify
   a default value to fill in a missing argument:

   ```ocaml
   val f: t -> ?x:(int [@js.default 0]) -> unit -> t
   ```

   If no default value is provided and the argument is missing, the
   argument is *dropped* from the list of arguments passed to the JS
   call (this apply to function/method/constructor calls).

 - In `[@@js.builder]` values, missing optional arguments are ignored
   (they don't create any property on the object).


There is a special treatment for optional argument on a
`[@js.variadic]` argument (see below), in which case a missing value
is interpreted as an empty list (i.e. no extra arguments).


When mapping an OCaml function to JS, the **function arity** is the
number of real arguments (not counting the final `unit`) and the
semantics is the standard one for JS functions: missing arguments are
filled with `undefined` and extra arguments are dropped.  The correct
way to support a calling convention where the JS caller might not
provide all arguments to a function defined in OCaml is to use
optional arguments (or just arguments with option types) on the OCaml
side.

In order to define **functions that return functions**, one can put a
`[@js.dummy]` attribute (or any arbitrary attribute) on the resulting type :

```ocaml
t1 -> (t2 -> t3 [@js.dummy])
```

Without the attribute, such a type would be parsed as a function of
arity 2 (returning type `t3`).


**Variadic functions** are supported, by adding a `[@js.variadic]`
attribute on the last parameter (which will represent all remaining
arguments):

```ocaml
val sep: string -> (string list [@js.variadic]) -> string
```


Type declarations
-----------------

All type declarations processed by gen_js_api create JS-able types,
i.e.  associated `*_to_js` and `*_to_js` mapping functions.  A
optional "private" modifier is allowed on the type declaration (in the
interface) and dropped from the generated definition (in the
implementation).  Mutually recursive type declarations are supported.


- "Abstract" subtype of `Ojs.t`:

    ```ocaml
    type t = private Ojs.t
    ```

  This is used to bind to JS "opaque" objects, with no runtime mapping
  involved when moving between OCaml and JS (mapping functions are the
  identity).

- Abstract type

   ```ocaml
   type t
   ```

  This will generate `type t = Ojs.t` in the implementation.  This
  is very similar to the case above.

- Type abbreviation:

    ```ocaml
    type t = tyexp
    ```

  (formally, abstract types with a manifest).  This assumes that the
  abbreviated type expression is itself JS-able.  Note that the first
  kind of type declaration above (abstract subtypes of `Ojs.t`) are
  a special kind of such declaration, since `abstract` is always dropped
  and `Ojs.t` is JS-able.

- Record declaration:

    ```ocaml
    type t = { .... }
    ```

  This assumes that the type for all fields are JS-able.  Fields can
  be mutabled, but polymorphic fields are not yet supported.

  OCaml record values of this type are mapped to JS objects (one
  property per field).  By default, property names are equal to OCaml
  labels, but this can be changed manually with a `[@js]` attribute.

  ```ocaml
  type myType = { x : int; y : int [@js "Y"]}
  ```

- Sum type declaration, mapped to enums (see Enums section).

- Sum type declaration with non constant constructors, mapped to records with a discriminator field (see Sum types section).

Enums mapped to polymorphic variants or sum types
-------------------------------------------------

Either polymorphic variants or normal sum types (all with constant
constructors) can be used to bind to "enums" in Javascript.  By
default, constructors are mapped to the JS string equal to their OCaml
name, but a custom translation can be provided with a `[@js]`
attribute.  This custom translation can be a string or an integer
literal.

```ocaml
type t =
  | Foo [@js "foo"]
  | Bar [@js 42]
  | Baz
    [@@js.enum]

type t = [`foo | `bar [@js 42] | `Baz] [@@js.enum]
```


It is possible to specify constructors with one argument of
type either int or string, used to represent "all other cases" of JS values.

```ocaml
type status =
  | OK [@js 1]
  | KO [@js 2]
  | OtherS of string [@js.default]
  | OtherI of int [@js.default]
    [@@js.enum]
```

There cannot be two default constructors with the same argument type.

Sum types mapped to records with a discriminator field
------------------------------------------------------

Either polymorphic variants or sum types can be mapped to JS records
with a discriminator field.

By default, the name of the discriminator field is `kind`, but this
can be changed by specifying a field name as attribute value of the
`[@@js.sum]` attribute. The value of the discriminator field is set to
the representation of the constructor name: it is derived
automatically from the constructor name but can also be specified with
a `[@js]` attribute. In this latter case, it can be either a string or
an integer.

A constant constructor is simply mapped to a record containing the
discriminator field.

A unary constructor is mapped to a record containing two fields: the
discriminator field and an argument field representing the unique
argument of the constructor. The argument field name is by default
`arg`, but this can be changed with a `[@js.arg]` attribute.

At most one unary constructor may have the attribute `[@js.default]`
and the argument of this constructor must be of type `Ojs.t`. In this
case, this constructor is used to handle the default case when either
the discriminator field is equal to an unexpected value or even worse
when the discriminator field is absent (from JS to ML). In the other
direction (from ML to JS), the unique argument is used as JavaScript
representation.

A nary constructor is mapped to a record containing two fields: the
discriminator field and an argument field set to an array representing
the arguments of the constructor. Once again, the argument field name
is by default `arg`, but this can be changed with a `[@js.arg]`
attribute. In the case of polymorphic variant, if the argument is a
tuple, then the polymorphic variant constructor is considered to be
n-ary.

Finally, an inline record constructor is mapped to a record containing
all the field of the record in addition of the discriminator
field. The name of the fields are derived from the name of the record
fields. As usual, these names can be customized using a `[@js]`
directive. This last case only applies to sum types.

```ocaml
type t =
  | A
  | B of int
  | C of int * string
  | D of {age: int; name: string}
  | Unknown of Ojs.t [@js.default]
    [@@js.sum]
```

The following declaration is equivalent to the previous one.

```ocaml
type t =
  | A [@js "A"]
  | B of int [@js.arg "arg"]
  | C of int * string [@js.arg "arg"]
  | D of {age: int [@js "age"]; name: string}
  | Unknown of Ojs.t [@js.default]
    [@@js.sum "kind"]
```

Union types
-----------

It is common for JS functions to allow arguments of several different
types (for instance, a string or an object).  To represent this calling
convention, one can use polymorphic variants:

```
val f: t -> ([`Str of string | `Obj of t | `Nothing] [@js.union]) -> ...
```

A limited form of (polymorphic) variants is supported: only constant
constructors or unary constructors are supported.

When the `[@js.union]` attribute is used without any other option,
only the ML to JS function is generated. The ML to JS conversion
function simply maps constant constructors to the `null` value and
unary constructors to the value of the constructor argument.

For generating the converse function, one needs to have a way to
distinguish JS values in the union type. At the moment, union types
with a discriminator field argument are supported. To indicate the
name of the field, one can add extra option `on_field "kind"` (where
"kind" is the name of the field) to the `[@js.union]` attribute. In
this case, the JS to ML conversion function will inspect the value of
the field named "kind" and will map the JS value to the corresponding
unary constructor. As for sum types, the value of the discriminator
field is deduced from the name of the constructors but it can always
be overriden by using a `[@js]`attribute.

```
type close_path

type moveto_abs

type svg_path_seg =
  | Unknown of Ojs.t         [@js.default]
  | Close_path of close_path [@js 1]
  | Moveto_abs of moveto_abs [@js 2]
  [@@js.union on_field "pathSegType"]
```

As for sum types, at most one unary constructor may have the
`[@js.default]` attribute and the argument of this constructor must be
of type `Ojs.t`. In this case, this constructor is used to handle the
default case when either the discriminator field is equal to an
unexpected value or even worse when the discriminator field is absent
(from JS to ML).

Discriminated union types
-------------------------

It is common for JS functions to allow arguments of several different
types (for instance, a string or an object), whose type depends on a
preceding argument.  To represent this calling convention, one can use
polymorphic variants:

```
val f: t -> ([`Str of string | `Obj of t | `Nothing] [@js.enum]) -> ...
```

This generalisation of the `[@js.enum]` attribute can only be used on
polymorphic variant used in contravariant context (i.e. to describe
mapping from OCaml to Javascript, not the other way around).  With
this calling convention, first the representation of the constructor
(which can be either an integer or a string, which is derived
automatically if not specified with a `[@js]` attribute) is passed,
followed by the n arguments of the constructor.
