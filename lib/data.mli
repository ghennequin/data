module type T = sig
  module Mat : sig
    val load : string -> Owl.Mat.mat
    val save : string -> Owl.Mat.mat -> unit
  end

  module Arr : sig
    val load : string -> Owl.Arr.arr
    val save : string -> Owl.Arr.arr -> unit
  end
end

val in_dir: string -> string

module Make (F : sig
  val file : [ `replace of string | `reuse of string]
end) : T
