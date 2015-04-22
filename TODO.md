TODO list for gen_js_api
========================



- Support sum types / polymorphic variants with non constant constructors
  (mapped to objects with a discriminator field).


- Support OCaml object types, to wrap JS values (less efficient than
  opaque binding, but sometimes more idiomatic).  The idea would be to
  declare:

  ````
    class foo: Ojs.t -> object
      method f: int -> unit (* method *)
      method x: int (* getter *)
      method set_x: int -> unit (* setter *)

      method to_js: Ojs.t
    end
  ````

  (DONE, to be documented and polished.)



- Support really abstract types (treated as `Ojs.t` in the implementation).

- Optimize generated code (for instance, lift calls to string_of_js on
  literals).

- Document class wrapping.

- Finish support for variadic functions.
