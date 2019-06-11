open Hdf5_caml

let in_dir =
  let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]") in
  let dir = Fpath.(v dir) in
  fun s -> Fpath.(dir / s) |> Fpath.to_string


let ensure_ext ext f = if Fpath.has_ext ext f then f else Fpath.add_ext ext f

type h5 = string

let h5 ?(reuse = false) f =
  let f = Fpath.(v f) |> ensure_ext "h5" |> Fpath.to_string in
  if (not reuse) && Sys.file_exists f then Sys.remove f;
  f


let with_handle h5 do_this =
  let h = H5.open_rdwr h5 in
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
  let load ~h5 name =
    with_handle h5 (fun h ->
        let h, name = get_object h name in
        H5.Float64.read_float_genarray h name Bigarray.c_layout)


  let save ~h5 name x =
    with_handle h5 (fun h ->
        let h, name = get_object h name in
        if H5.exists h name then H5.delete h name;
        H5.Float64.write_float_genarray h name x)
end

module Arr = Mat

type json = string

let json ?(reuse = false) f =
  let f = Fpath.(v f) |> ensure_ext "json" |> Fpath.to_string in
  if (not reuse) && Sys.file_exists f then Sys.remove f;
  f


let gen conv ~json label x =
  let prms_prev =
    if Sys.file_exists json then Yojson.Safe.from_file json else `Assoc []
  in
  let prms = Yojson.Safe.(Util.combine prms_prev (`Assoc [ label, conv x ])) in
  let f = open_out json in
  Yojson.Safe.pretty_to_channel f prms;
  close_out f;
  x


let bool = gen (fun x -> `Bool x)
let int = gen (fun x -> `Int x)
let float = gen (fun x -> `Float x)
let string = gen (fun x -> `String x)
let any ~json label conv x = gen conv ~json label x
