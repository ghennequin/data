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


module H5Attr = struct
  let exists ~h5 name =
    with_handle h5 (fun h ->
        let h, name = get_object h name in
        H5.attribute_exists h name)


  let delete ~h5 name =
    if exists ~h5 name
    then
      with_handle h5 (fun h ->
          let h, name = get_object h name in
          if H5.attribute_exists h name then H5.delete_attribute h name)


  let get f ~h5 name =
    with_handle h5 (fun h ->
        let h, name' = get_object h name in
        if H5.attribute_exists h name'
        then f h name
        else failwith Printf.(sprintf "Attribute %s does not exist" name))


  let get_int64 = get H5.read_attribute_int64
  let get_int ~h5 name = get_int64 ~h5 name |> Int64.to_int
  let get_float = get H5.read_attribute_float
  let get_float_array = get H5.read_attribute_float_array
  let get_string = get H5.read_attribute_string
  let get_string_array = get H5.read_attribute_string_array

  let write f ~h5 name x =
    with_handle h5 (fun h ->
        let h, name = get_object h name in
        if H5.attribute_exists h name then H5.delete_attribute h name;
        f h name x)


  let write_int64 = write H5.write_attribute_int64
  let write_int ~h5 name x = write_int64 ~h5 name (Int64.of_int x)
  let write_float = write H5.write_attribute_float
  let write_float_array = write H5.write_attribute_float_array
  let write_string = write H5.write_attribute_string
  let write_string_array = write H5.write_attribute_string_array
end

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
