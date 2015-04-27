class js_object: Ojs.t ->
  object
    inherit Ojs.obj
    method to_string: string [@@js.call]
  end

class js_string: Ojs.t ->
  object
    inherit js_object
    method char_at: int -> js_string
    method char_code_at: int -> int
    method concat: (js_string list [@js.variadic]) -> js_string
    method index_of: js_string -> ?start:int -> unit -> int
    method last_index_of: js_string -> ?start:int -> unit -> int
    method length: int
    method locale_compare: js_string -> int
    method match_: js_regexp -> js_string array
    method replace: js_regexp -> js_string -> js_string
    method search: js_regexp -> int
    method slice: start:int -> ?end_:int -> unit -> js_string
    method split: ?separator:js_string -> ?limit:int -> unit -> js_string array
    method substr: start:int -> ?length:int -> unit -> js_string
    method substring: start:int -> ?end_:int -> unit -> js_string
    method to_locale_lower_case: js_string [@@js.call]
    method to_locale_upper_case: js_string [@@js.call]
    method to_lower_case: js_string [@@js.call]
    method to_upper_case: js_string [@@js.call]
    method trim: js_string [@@js.call]
    method value_of: string [@@js.call]
  end
and js_regexp: Ojs.t ->
  object
    inherit js_object
    method global: bool
    method ignore_case: bool
    method multiline: bool
    method source: string
    method last_index: int
    method exec: js_string -> js_string option
    method test: js_string -> bool
  end

module String: sig
  class t: string -> js_string [@@js.new "String"]

  val create: string -> js_string [@@js.new "String"]

  val from_char_code: (int list [@js.variadic]) -> js_string [@@js.global "String.fromCharCode"]
end

module RegExp: sig
  class t: string -> ?flags:string -> unit -> js_regexp [@@js.new "RegExp"]

  val create: string -> ?flags:string -> unit -> js_string [@@js.new "RegExp"]
end

