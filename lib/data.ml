open Hdf5_caml

let parse_group h name =
  let path = String.split_on_char '/' name |> List.filter (( <> ) "") in
  let rec traverse cur_h accu =
    match accu with
    | [] ->
        assert false
    | [hd] ->
        (cur_h, hd)
    | g :: rest ->
        traverse H5.(open_group cur_h g) rest
  in
  traverse h path

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
  val file : [`replace of string | `reuse of string]
end) : T = struct
  let h =
    match F.file with `replace f -> Sys.remove f ; H5.open_rdwr f | `reuse f -> H5.open_rdwr f

  let close () = H5.flush h ; H5.close h
  let _ = at_exit (fun () -> try H5.close h ; Printf.printf "closed\n%!" with _ -> ())

  module Mat = struct
    let load name = H5.Float64.read_float_genarray h name Bigarray.c_layout

    let save name x =
      let h, name = parse_group h name in
      if H5.exists h name then H5.delete h name ;
      H5.Float64.write_float_genarray h name x
  end

  module Arr = Mat
end
