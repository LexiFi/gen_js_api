TODO list for gen_js_api
========================



- Support sum types / polymorphic variants with non constant constructors
  (mapped to objects with a discriminator field).


- Support really abstract types (treated as `Ojs.t` in the implementation).

- Optimize generated code (for instance, lift calls to string_of_js on
  literals).

- Idea: to facilitate binding and calling multiple methods at once,
  provide something like (jQuery example):

    val set: ?text:string -> ?hide:unit -> ?css:(string * string) -> t -> unit
     [@@js.multicall]


  One can then write:

     set
       ~text:"Hello"
       ~hide:()
       node

  Each provided argument yield one method call (in the order where
  arguments are declared, of course).  This is mostly interesting when
  methods are used to "set" internal properties, and when the different
  calls commute.

  This could be simulated with:

```ocaml

    val set: ?text:string -> ?hide:unit -> ?css:(string * string) -> t -> unit
  [@@@js.custom]
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

  ```caml
  val foo: ?bla:int -> t -> int
  ```

  instead of:

  ```caml
  val foo: t -> ?bla:int -> unit -> int

   foo ~bla x ()
   foo x ~bla ()
   foo x () ~bla
  ```
