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
