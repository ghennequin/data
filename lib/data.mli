(** [in_dir filename] expands [filename] into the full path, where the root is the
    "result directory" passed as a [-d dir] command line argument. *)
val in_dir : string -> string

(** {1 Hdf5 files to store large arrays} *)

type h5

val h5 : ?reuse:bool -> string -> h5

module H5Attr : sig
  val exists : h5:h5 -> string -> bool
  val delete : h5:h5 -> string -> unit
  val get_int64 : h5:h5 -> string -> int64
  val get_int : h5:h5 -> string -> int
  val get_float : h5:h5 -> string -> float
  val get_float_array : h5:h5 -> string -> float array
  val get_string : h5:h5 -> string -> string
  val get_string_array : h5:h5 -> string -> string array
  val write_int64 : h5:h5 -> string -> int64 -> unit
  val write_int : h5:h5 -> string -> int -> unit
  val write_float : h5:h5 -> string -> float -> unit
  val write_float_array : h5:h5 -> string -> float array -> unit
  val write_string : h5:h5 -> string -> string -> unit
  val write_string_array : h5:h5 -> string -> string array -> unit
end

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
val any : json:json -> string -> ('a -> Yojson.Safe.t) -> 'a -> 'a
