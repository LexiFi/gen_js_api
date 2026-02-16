# Binding Node.js Modules with Runtime Primitives

This guide shows how to use the new runtime primitive support in `gen_js_api` to bind Node.js libraries that are usually obtained with `require(...)`. The feature hinges on two additions:

- any `[@@js.global "@primitive_name"]` binding returns an `Ojs.t` pointing to a primitive exported by the JavaScript runtime;
- a scope string that starts with `@` (for example `[@@@js.scope "@node_fs.promises"]`) resolves the first path component through the runtime primitives before following regular properties.

Together, those tools let you keep your bindings declarative while delegating the actual `require` calls to a tiny JavaScript stub.

## Example layout

```
runtime_primitives/
  dune
  imports.js
  imports.wat
  bindings.mli
  example.ml
```

### Step 1 - expose the runtime primitives

Create a JavaScript file that `require`s the Node modules you need and publishes them as js_of_ocaml runtime primitives. The js_of_ocaml linker recognises `//Provides: <name>` comments and registers the value under that name at startup.

```javascript
// runtime_primitives/imports.js
'use strict';

//Provides: node_path
var node_path = require('path');

//Provides: node_fs
var node_fs = require('fs');

//Provides: node_version
var node_version = require('process').version;

//Provides: node_console
var node_console = console.log;

```

When targeting WebAssembly you also need to expose the primitives through a `.wat` shim so that `wasm_of_ocaml` can import them at runtime:

```wat
;; runtime_primitives/imports.wat
(global (export "_node_path") (import "js" "node_path") anyref)
(global (export "_node_fs") (import "js" "node_fs") anyref)
(global (export "_node_version") (import "js" "node_version") anyref)
(global (export "_node_console") (import "js" "node_console") anyref)
```

List this file in your dune stanza so that js_of_ocaml ships it with the compiled artefacts:

```
; runtime_primitives/dune
(rule
 (targets bindings.ml)
 (deps bindings.mli)
 (action (run gen_js_api %{deps})))

(executable
 (name example)
 (libraries ojs)
 (preprocess (pps gen_js_api.ppx))
 (modes js wasm)
 (js_of_ocaml (javascript_files imports.js))
 (wasm_of_ocaml (javascript_files imports.js imports.wat)))
```

Adding the file to both `js_of_ocaml` and `wasm_of_ocaml` makes the primitives available in browser and wasm builds alike.

### Step 2 - bind module functions with `[@js.scope "@..."]`

Use `module [@js.scope "@primitive"]` blocks to call methods on runtime primitives without manually threading the module objects. The interface below covers the synchronous filesystem API used in the reference JavaScript while keeping the underlying modules abstract.

```ocaml
(* runtime_primitives/bindings.mli *)
module [@js.scope "@node_fs"] Fs : sig
  val write_file_sync : string -> string -> unit [@@js.global "writeFileSync"]
  val read_file_sync : string -> encoding:string -> string [@@js.global "readFileSync"]
  val readdir_sync : string -> string array [@@js.global "readdirSync"]
  val append_file_sync : string -> string -> unit [@@js.global "appendFileSync"]
end

module [@js.scope "@node_path"] Path : sig
  val separator: string [@@js.global "sep"]
  val join : (string list [@js.variadic]) -> string [@@js.global "join"]
end
```
Each module-level scope starts with `@`, so the ppx turns calls like `Fs.write_file_sync` into direct invocations on the corresponding Node module (`node_fs.writeFileSync` in this case) without requiring you to pass the module object around.

### Step 3 - bind direct values with `@`-prefixed `[@@js.global]`

When you only need the primitive itself—such as a constant exported by a Node module—use the `@` prefix inside `[@@js.global]` to obtain it directly as an OCaml value.

```ocaml
(* runtime_primitives/primitives_bindings.mli continued *)

val node_version : string [@@js.global "@node_version"]
val log : string -> unit [@@js.global "@node_console"]
```

These expand to `Jsoo_runtime.Js.runtime_value ...` calls and convert the results to the requested OCaml types, so you can expose constants or functions alongside the scoped modules described above.

### Step 4 - port the JavaScript example

`main.ml` mirrors the original JavaScript snippet that writes, reads, appends, and re-reads a file while logging progress to the Node console. It relies on the scoped `Fs`/`Path` modules plus the direct `log`, `path_separator`, and `node_version` values.

```ocaml
open Bindings

let initial_content = "Hello, Node.js!"
let appended_line = "\nAppending a new line."
let encoding = "utf-8"
let filename = "example.txt"

let run () =
  let file = Path.join ["."; filename] in

  Fs.write_file_sync file initial_content;

  let content = Fs.read_file_sync file ~encoding in
  if content <> initial_content then
    failwith "Unexpected initial content";
  log ("File content: " ^ content);

  let files = Fs.readdir_sync "." |> Array.to_list in
  if not (List.mem filename files) then
    failwith "example.txt missing from directory listing";
  log ("Files in current directory: " ^ String.concat ", " files);

  Fs.append_file_sync file appended_line;

  let updated = Fs.read_file_sync file ~encoding in
  if updated <> initial_content ^ appended_line then
    failwith "Append failed";
  log ("Updated content: " ^ updated);
  log ("Path separator reported by Node: " ^ Path.separator);
  log ("Node.js version: " ^ node_version)


let () = run ()
```

### Putting it together

1. Declare each required Node module once in `imports.js` (and mirror them in `imports.wat` for wasm) using the js_of_ocaml `//Provides:` convention.
2. Export the files through dune so that the js_of_ocaml toolchain registers those primitives at runtime.
3. Map node modules in OCaml with `module [@js.scope "@primitive"]` blocks, and use `@`-prefixed `[@@js.global]` bindings for direct values.
4. Consume the generated modules from OCaml exactly as you would in JavaScript, as shown in `example.ml`.

With these pieces in place you can keep writing high-level `gen_js_api` bindings while relying on the new runtime primitive support to bridge your OCaml code to Node-specific libraries provided via `require`.
