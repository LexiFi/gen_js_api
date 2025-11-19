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

val node_version : string [@@js.runtime "node_version"]
val log : string -> unit [@@js.runtime "node_console"]
