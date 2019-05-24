open Hdf5_caml

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

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir = Printf.sprintf "%s/%s" dir

module Make (F : sig
  val file : [`replace of string | `reuse of string]
end) : T = struct
  let _ = match F.file with `replace f -> if Sys.file_exists f then Sys.remove f | _ -> ()
  let filename = match F.file with `replace f -> f | `reuse f -> f

  let with_handle do_this =
    let h = H5.open_rdwr filename in
    let result = do_this h in
    H5.flush h ; H5.close h ; result

  let get_object h name =
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

  module Mat = struct
    let load name = with_handle (fun h -> H5.Float64.read_float_genarray h name Bigarray.c_layout)

    let save name x =
      with_handle (fun h ->
          let h, name = get_object h name in
          if H5.exists h name then H5.delete h name ;
          H5.Float64.write_float_genarray h name x )
  end

  module Arr = Mat
end
