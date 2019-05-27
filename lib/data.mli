module type T = sig

  type t_prms

  module Mat : sig
    val load : string -> Owl.Mat.mat
    val save : string -> Owl.Mat.mat -> unit
  end

  module Arr : sig
    val load : string -> Owl.Arr.arr
    val save : string -> Owl.Arr.arr -> unit
  end

  module Prms: sig
    val t : string -> t_prms -> t_prms
    val load : string -> t_prms
    val int : string -> int -> int
    val float : string -> float -> float
    val string : string -> string -> string
  end
end

val in_dir : string -> string

module Make
(F : sig
  type t_prms
  val sexp_of_t_prms: t_prms -> Sexplib0.Sexp.t
  val t_prms_of_sexp: Sexplib0.Sexp.t -> t_prms
  val file : [ `replace of string | `reuse of string ]
end) : T with type t_prms = F.t_prms 


