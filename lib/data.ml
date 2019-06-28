open Hdf5_caml
open Hdf5_raw

let in_dir s =
  let dir =
    lazy
      (let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]") in
       Fpath.(v dir))
  in
  Fpath.(Lazy.force dir / s) |> Fpath.to_string


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


module Iter = struct
  type index_order =
    | Default
    | Incr
    | Decr

  type index_by =
    | Name
    | Create_order

  let _iter iterate_by_name ~h5 ?(index_by = Name) ?(index_order = Default) ~stop name =
    with_handle h5 (fun h ->
        let h, name = get_object h name in
        let iter_order =
          match index_order with
          | Default -> Hdf5_raw.H5_raw.Iter_order.NATIVE
          | Incr -> Hdf5_raw.H5_raw.Iter_order.INC
          | Decr -> Hdf5_raw.H5_raw.Iter_order.DEC
        in
        let index_type =
          match index_by with
          | Name -> Hdf5_raw.H5_raw.Index.NAME
          | Create_order -> Hdf5_raw.H5_raw.Index.CRT_ORDER
        in
        let stop a b _c _d =
          if stop (H5.h5t a) b
          then Hdf5_raw.H5_raw.Iter.STOP
          else Hdf5_raw.H5_raw.Iter.CONT
        in
        iterate_by_name (H5.hid h) name index_type iter_order stop |> ignore)


  let iter =
    let iterate_by_name h name idx_type iter_order stop =
      H5l.iterate_by_name h name idx_type iter_order stop ()
    in
    _iter iterate_by_name


  let iter_attr =
    let iterate_by_name h5 name idx_type iter_order stop () =
      H5a.iterate_by_name h5 name ~idx_type ~iter_order stop ()
    in
    _iter iterate_by_name


  let _map
      (iter :
        h5:h5
        -> ?index_by:index_by
        -> ?index_order:index_order
        -> stop:(H5.t -> h5 -> bool)
        -> h5
        -> unit)
      ~h5
      ~index_by
      ~index_order
      ~f
      name
    =
    let x = ref [] in
    let stop h name =
      x := f h name :: !x;
      false
    in
    iter ~h5 ~index_by ~index_order ~stop name;
    let x = !x |> List.rev |> Array.of_list in
    Gc.minor ();
    x


  let map ~h5 ?(index_by = Name) ?(index_order = Default) ~f name =
    _map iter ~h5 ~index_by ~index_order ~f name


  let map_attr ~h5 ?(index_by = Name) ?(index_order = Default) ~f name =
    _map iter_attr ~h5 ~index_by ~index_order ~f name
end

module Link = struct
  let exists ~h5 name =
    with_handle h5 (fun h ->
        let link, link_name = get_object h name in
        H5l.exists (Hdf5_caml.H5.hid link) link_name)


  let delete ~h5 name =
    with_handle h5 (fun h ->
        let link, link_name = get_object h name in
        if H5l.exists (Hdf5_caml.H5.hid link) link_name
        then H5l.delete (Hdf5_caml.H5.hid link) link_name)


  let soft ~h5 ~target name =
    with_handle h5 (fun h ->
        let link, link_name = get_object h name in
        if H5l.exists (Hdf5_caml.H5.hid link) link_name
        then H5l.delete (Hdf5_caml.H5.hid link) link_name;
        H5.create_soft_link ~target_path:target ~link ~link_name)


  let ext ~h5 ~target_file ~target_path name =
    with_handle h5 (fun h ->
        let link, link_name = get_object h name in
        if H5l.exists (Hdf5_caml.H5.hid link) link_name
        then H5l.delete (Hdf5_caml.H5.hid link) link_name;
        H5.create_external_link
          link
          ~target_file_name:target_file
          ~target_obj_name:target_path
          ~link_name)
end

module H5Attr = struct
  type attr =
    | Int of int
    | Float of float
    | String of string
    | Float_array of float array
    | String_array of string array

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
        let h, n = get_object h name in
        if H5.attribute_exists h n
        then f h n
        else failwith Printf.(sprintf "Attribute %s does not exist" name))


  let get_int64 = get H5.read_attribute_int64
  let get_int ~h5 name = get_int64 ~h5 name |> Int64.to_int
  let get_float = get H5.read_attribute_float
  let get_float_array = get H5.read_attribute_float_array
  let get_string = get H5.read_attribute_string
  let get_string_array = get H5.read_attribute_string_array

  let get_all =
    let f h name =
      let h = H5.hid h in
      let attr = H5a.open_ h name in
      let atype = H5a.get_type attr in
      let aspace = H5a.get_space attr in
      let c1 = H5t.get_class atype in
      let c2 = H5s.get_simple_extent_type aspace in
      H5t.close attr;
      H5s.close aspace;
      H5a.close atype;
      let x =
        let open H5t.Class in
        let open H5s.Class in
        match c1, c2 with
        | INTEGER, SCALAR -> Int (H5.read_attribute_int64 H5.(h5t h) name |> Int64.to_int)
        | FLOAT, SCALAR -> Float (H5.read_attribute_float H5.(h5t h) name)
        | FLOAT, SIMPLE -> Float_array (H5.read_attribute_float_array H5.(h5t h) name)
        | STRING, SCALAR -> String (H5.read_attribute_string H5.(h5t h) name)
        | STRING, SIMPLE -> String_array (H5.read_attribute_string_array H5.(h5t h) name)
        | _ -> failwith "yo"
      in
      name, x
    in
    fun ~h5 name -> Iter.map_attr ~h5 ~f name


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


  (** [load all ~h5 name ] is an array of matrices within a group or file specified by h5
      and name *)
  let load_all =
    let f h name = H5.Float64.read_float_genarray h name Bigarray.c_layout in
    fun ~h5 name -> Iter.map ~h5 ~f name


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
