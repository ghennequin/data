module type T = sig
  val close : unit -> unit

  module Mat : sig
    val load : string -> Owl.Mat.mat
    val save : string -> Owl.Mat.mat -> unit
  end

  module Arr : sig
    val load : string -> Owl.Arr.arr
    val save : string -> Owl.Arr.arr -> unit
  end
end

module Make (F : sig
  val file : [`replace of string | `reuse of string ]
end) : T
