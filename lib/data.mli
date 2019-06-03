(** [in_dir filename] expands [filename] into the full path, where the root is the
    "result directory" passed as a [-d dir] command line argument. *)
val in_dir : string -> string

(** {1 Hdf5 files to store large arrays} *)

type h5

val h5 : ?reuse:bool -> string -> h5

(** [Mat.mat] operations *)
module Mat : sig
  val save : h5:h5 -> string -> Owl.Mat.mat -> unit
  val load : h5:h5 -> string -> Owl.Mat.mat
end

(** [Owl.Arr] operations *)
module Arr : sig
  val save : h5:h5 -> string -> Owl.Arr.arr -> unit
  val load : h5:h5 -> string -> Owl.Arr.arr
end

(** {1 Json files to log simulation parameters} *)

type json

val json : ?reuse:bool -> string -> json
val bool : json:json -> string -> bool -> bool
val int : json:json -> string -> int -> int
val float : json:json -> string -> float -> float
val string : json:json -> string -> string -> string
val any : json:json -> string -> ('a -> Yojson.Safe.json) -> 'a -> 'a
