TODO list for gen_js_api
========================

- Create reasonnably complete bindings for Javascript's stdlib
  (string, regexp), for the DOM, for jQuery, etc.

- Add a safe mode, where the generated code is augmented with explicit
  checks (e.g. when casting a JS value to a string or integer, when
  accessing a property, etc).

- Support sum types / polymorphic variants with non constant constructors
  (mapped to objects with a discriminator field).

- Optimize generated code (for instance, lift calls to string_of_js on
  literals).

- Idea: to facilitate binding and calling multiple methods at once,
  provide something like (jQuery example):

    ```ocaml
    val set: ?text:string -> ?hide:unit -> ?css:(string * string) -> t -> unit
     [@@js.multicall]
    ```


  One can then write:

     ```ocaml
     set
       ~text:"Hello"
       ~hide:()
       node
     ```

  Each provided argument yields one method call (in the order where
  arguments are declared, of course).  This is mostly interesting when
  methods are used to "set" internal properties, and when the different
  calls commute.

  This could be simulated with:

  ```ocaml

    val set: ?text:string -> ?hide:unit -> ?css:(string * string) -> t -> unit
      [@@@js.custom
      val set_text: t -> string -> unit
        [@@js.meth "text"]

      let set ?text ... x =
        Option.iter (set_text x) text;
        ...
      ]
  ```


- Optional arguments on JS methods are usually at the end.  But this
  forces to add a `unit` pseudo-argument.  One could have an
  (optional) convention to push optional arguments at the end of the JS
  call even though there are not in the OCaml type.  This would also
  work for instance methods:

  ```ocaml
  val foo: ?bla:int -> t -> int
  ```

  instead of:

  ```ocaml
  val foo: t -> ?bla:int -> unit -> int
  ```

- When defining a binding to a function with `[@@js.global
  "foo.bar"]`, this is currently interpreted as calling this global
  function.  One could interpret it as calling the bar method on
  object foo, which would have the effect of assigning `this` during
  the function evaluation.


- Extend default heuristic for simplifying binding to "singleton objects", e.g.:


  ```ocaml
  module Console : sig
    [@@@js.singleton "Console"]

    val log: string -> unit
  end
  ```

  The `[@@js.singleton]` attribute would change the automatic heuristic
  (until the end of the current structure) so that the declaration
  above is interpreted as `[@@js.global "Console.log"]` (i.e. functions
  are interpreted as calling methods on the object specified
  in the `singleton` attribute).

