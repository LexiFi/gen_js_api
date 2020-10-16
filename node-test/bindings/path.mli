[@@@js.scope (Ojs.require "path")]

val sep: string
val dirname: string -> string
val extname: string -> string
val is_absolute: string -> bool
val join: string list -> string
val normalize: string -> string

type parse_result =
  {
      dir: string;
      root: string;
      base: string;
      name: string;
      ext: string
  }

val parse: string -> parse_result
