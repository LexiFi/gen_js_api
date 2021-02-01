[@@@js.scope Imports.path]

val sep: string [@@js.global]
val dirname: string -> string [@@js.global]
val extname: string -> string [@@js.global]
val is_absolute: string -> bool [@@js.global]
val join: (string list [@js.variadic]) -> string [@@js.global]
val normalize: string -> string [@@js.global]

type parse_result =
  {
    dir: string;
    root: string;
    base: string;
    name: string;
    ext: string
  }

val parse: string -> parse_result [@@js.global]
