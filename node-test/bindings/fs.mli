[@@@js.scope Imports.fs_promises]

module Dirent : sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val name: t -> string [@@js.get]
  val is_file: t -> bool [@@js.call]
  val is_directory: t -> bool [@@js.call]
end

module Dir : sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t


  val path: t -> string [@@js.get]
  val close: t -> unit Promise.t [@@js.call]
  val read:t -> Dirent.t option Promise.t [@@js.call]
end

module FileHandle : sig
  type t = Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  type read = {
    bytes_read: int;
    buffer: Buffer.t;
  }

  val append_file: t -> Buffer.t -> unit Promise.t [@@js.call]
  val read: t -> Buffer.t -> int -> int -> int -> read Promise.t [@@js.call]
  val chmod: t -> int -> unit Promise.t [@@js.call]
  val chmown: t -> uid:int -> gid:int -> unit Promise.t [@@js.call]
  val close: t -> unit Promise.t [@@js.call]
  val datasync: t -> unit Promise.t [@@js.call]
  val fd: t -> int [@@js.get]
end

val readdir: string -> string list Promise.t [@@js.global]
val open_: string -> flag:string -> FileHandle.t Promise.t [@@js.global]
val rmdir: string -> unit Promise.t [@@js.global]
val rename: string -> string -> unit Promise.t [@@js.global]
val unlink: string -> unit Promise.t [@@js.global]
