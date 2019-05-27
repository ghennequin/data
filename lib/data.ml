open Hdf5_caml

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

  module Prms : sig
    val t : string -> t_prms -> t_prms
    val load : string -> t_prms
    val int : string -> int -> int
    val float : string -> float -> float
    val string : string -> string -> string
  end
end

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir = Printf.sprintf "%s/%s" dir

module Make (F : sig
  type t_prms

  val sexp_of_t_prms : t_prms -> Sexplib0.Sexp.t
  val t_prms_of_sexp : Sexplib0.Sexp.t -> t_prms
  val file : [ `replace of string | `reuse of string ]
end) =
struct
  include F

  let _ =
    match F.file with
    | `replace f -> if Sys.file_exists f then Sys.remove f
    | _ -> ()


  let filename =
    match F.file with
    | `replace f -> f
    | `reuse f -> f


  let with_handle do_this =
    let h = H5.open_rdwr filename in
    let result = do_this h in
    H5.flush h;
    H5.close h;
    result


  let get_object h name =
    let path = String.split_on_char '/' name |> List.filter (( <> ) "") in
    let rec traverse cur_h accu =
      match accu with
      | [] -> assert false
      | [ hd ] -> cur_h, hd
      | g :: rest -> traverse H5.(open_group cur_h g) rest
    in
    traverse h path


  module Mat = struct
    let load name =
      with_handle (fun h ->
          let h, name = get_object h name in
          H5.Float64.read_float_genarray h name Bigarray.c_layout)


    let save name x =
      with_handle (fun h ->
          let h, name = get_object h name in
          if H5.exists h name then H5.delete h name;
          H5.Float64.write_float_genarray h name x)
  end

  module Arr = Mat

  module Prms = struct
    let generic label s =
      with_handle (fun h ->
          let p_ = H5.open_group h "prms" |> H5.hid in
          let open Hdf5_raw in
          let atype = H5t.copy H5t.c_s1 in
          H5t.set_size atype (String.length s);
          H5t.set_strpad atype H5t.Str.NULLTERM;
          let ds = H5s.create_simple [| 1 |] in
          let attr = H5a.create p_ label atype ds in
          H5a.write_string attr atype s;
          H5a.close attr;
          H5g.close p_)


    let t label value =
      let s = Sexplib0.Sexp.to_string (F.sexp_of_t_prms value) in
      generic label s;
      value


    let load _ = F.t_prms_of_sexp Sexplib0.Sexp.(List [])

    let int label value =
      let s = string_of_int value in
      generic label s;
      value


    let float label value =
      let s = string_of_float value in
      generic label s;
      value


    let string label value =
      let s = value in
      generic label s;
      value
  end
end
