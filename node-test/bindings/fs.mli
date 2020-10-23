[@@@js.scope Imports.fs_promises]

module[@js.prototype] Dirent : sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val js_of_t: Ojs.t -> t

  val name: t -> string
  val is_file: t -> bool
  val is_directory: t -> bool
end

module[@js.prototype] Dir : sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val js_of_t: Ojs.t -> t

  val path: t -> string
  val close: t -> unit Promise.t
  val read:t -> Dirent.t option Promise.t
end

module[@js.prototype] FileHandle : sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val js_of_t: Ojs.t -> t

  type read = {
    bytes_read: int;
    buffer: Buffer.t;
  }

  val append_file: t -> Buffer.t -> unit Promise.t
  val read: t -> Buffer.t -> int -> int -> int -> read Promise.t
  val chmod: t -> int -> unit Promise.t
  val chmown: t -> uid:int -> gid:int -> unit Promise.t
  val close: t -> unit Promise.t
  val datasync: t -> unit Promise.t
  val fd: t -> int
end

[@@@js.static]
val readdir: string -> string list Promise.t


val open_: string -> flag:string -> FileHandle.t Promise.t
val rmdir: string -> unit Promise.t
val rename: string -> string -> unit Promise.t
val unlink: string -> unit Promise.t