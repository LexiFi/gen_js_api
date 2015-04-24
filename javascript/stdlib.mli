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
  end
and js_regexp: Ojs.t ->
  object
    inherit js_object
  end

module String: sig
  class t: string -> js_string [@@js.new "String"]

  val from_char_code: (int list [@js.variadic]) -> js_string [@@js.global "String.fromCharCode"]
end

module RegExp: sig
  class t: string -> ?flags:string -> unit -> js_regexp [@@js.new "RegExp"]
end

