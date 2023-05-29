(** [filereader.ml] is a module that is responsible for opening a given file and
    running its contents. It includes a function that returns the contents of a
    file, given its file path. *)

    val read : string -> string
    (** [read dir] is the contents of the file with path [dir] raises: InvalidDir if
        d is not a valid directory [dir] is interpreted as a directory relative to
        the root directory of the project. *)
    
    exception InvalidDir
    (** Exception raised if an invalid file path is read. *)