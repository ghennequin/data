open Hdf5_caml

let in_dir =
  let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]") in
  Printf.sprintf "%s/%s" dir


type data = string

let file = function
  | `reuse f -> f
  | `replace f ->
    if Sys.file_exists f then Sys.remove f;
    f


let with_handle ~data do_this =
  let h = H5.open_rdwr data in
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
  let load ~data name =
    with_handle ~data (fun h ->
        let h, name = get_object h name in
        H5.Float64.read_float_genarray h name Bigarray.c_layout)


  let save ~data name x =
    with_handle ~data (fun h ->
        let h, name = get_object h name in
        if H5.exists h name then H5.delete h name;
        H5.Float64.write_float_genarray h name x)
end

module Arr = Mat

module type Param_type = sig
  type t

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Param = struct
  let generic ~data label conv value =
    let s = conv value in
    with_handle ~data (fun h ->
        let p_ = H5.open_group h "prms" |> H5.hid in
        let open Hdf5_raw in
        let atype = H5t.copy H5t.c_s1 in
        H5t.set_size atype (String.length s);
        H5t.set_strpad atype H5t.Str.NULLTERM;
        let ds = H5s.create_simple [| 1 |] in
        let attr = H5a.create p_ label atype ds in
        H5a.write_string attr atype s;
        H5a.close attr;
        H5g.close p_);
    value


  let t (type _t) ~data (module P : Param_type with type t = _t) label =
    let conv value = value |> P.sexp_of_t |> Sexplib0.Sexp.to_string in
    generic ~data label conv


  let int ~data label = generic ~data label string_of_int
  let float ~data label = generic ~data label string_of_float
  let string ~data label = generic ~data label (fun value -> value)
end
