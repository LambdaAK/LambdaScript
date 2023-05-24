exception InvalidDir

let read dir =
  if not (Sys.file_exists dir) then raise InvalidDir
  else
    let channel = open_in_bin dir in
    let contents = really_input_string channel (in_channel_length channel) in
    close_in channel;
    contents