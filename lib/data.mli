(** [in_dir filename] expands [filename] into the full path, where the root is the
    "result directory" passed as a [-d dir] command line argument. *)
val in_dir : string -> string

(** Abstract HDF5 data handler *)
type data

(** Create a data handler from the specification of a filename; at this stage, if the
    file doesn't exist, it is created. *)
val file : [ `replace of string | `reuse of string ] -> data

(** [Mat.mat] operations *)
module Mat : sig
  val save : data:data -> string -> Owl.Mat.mat -> unit
  val load : data:data -> string -> Owl.Mat.mat
end

(** [Owl.Arr] operations *)
module Arr : sig
  val save : data:data -> string -> Owl.Arr.arr -> unit
  val load : data:data -> string -> Owl.Arr.arr
end

module type Param_type = sig
  type t

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

(** Handle basic simulation parameter types *)
module Param : sig
  val t : data:data -> (module Param_type with type t = 'a) -> string -> 'a -> 'a
  val int : data:data -> string -> int -> int
  val float : data:data -> string -> float -> float
  val string : data:data -> string -> string -> string
end
