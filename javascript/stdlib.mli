class js_string: Ojs.t ->
  object
    inherit Ojs.obj
    method char_at: int -> js_string
    method char_code_at: int -> int
    method concat: (js_string list [@js.variadic]) -> js_string
    method from_char_code: (int list [@js.variadic]) -> js_string
    method index_of: js_string -> ?start:int -> unit -> int
    method last_index_of: js_string -> ?start:int -> unit -> int
    method length: int
    method locale_compare: js_string -> int
    method match_: js_regexp -> int array
    method replace: js_regexp -> js_string -> js_string
  end
and js_regexp: Ojs.t ->
  object
    inherit Ojs.obj
  end
