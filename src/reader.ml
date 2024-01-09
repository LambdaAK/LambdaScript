exception InvalidDir

(** * [read dir] reads the contents of the file at [dir] and returns it as a
    string. * Raises: [InvalidDir] if [dir] does not exist. *)
let read dir =
  if not (Sys.file_exists dir) then raise InvalidDir
  else
    let channel = open_in_bin dir in
    let contents = really_input_string channel (in_channel_length channel) in
    close_in channel;
    contents
