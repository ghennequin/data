open Protocol_conv_yaml

type 'a t = Fpath.t (* whole path *)

let default_exdir_meta typ =
  let x = `O [ "type", `String typ; "version", `Float 1.0 ] in
  `O [ "exdir", x ]


let with_ext path =
  if not (Fpath.has_ext "exdir" path) then Fpath.add_ext "exdir" path else path


let path_of x = Fpath.to_string x
let meta_of x = Yaml_unix.of_file_exn Fpath.(x / "exdir.yml")

(* here we should check, if the directory exists, that the "type" label in "exdir.yml" is
   correct *)
let elt ?attr root_path name typ =
  let path = Fpath.(root_path / name) in
  let _path = Fpath.to_string path in
  if not (Sys.file_exists _path && Sys.is_directory _path)
  then Unix.mkdir Fpath.(to_string path) 0o755;
  default_exdir_meta typ |> Yaml_unix.to_file_exn Fpath.(path / "exdir.yml");
  (match attr with
  | None -> ()
  | Some y -> Yaml_unix.to_file_exn Fpath.(path / "attributes.yml") y);
  path


let file ?attr path = elt ?attr (with_ext path) "" "file"
let group ?attr root name = elt ?attr root name "group"

type dim = int array [@@deriving protocol ~driver:(module Yaml)]

let dim path =
  Fpath.(path / "attributes.yml") |> Yaml_unix.of_file_exn |> dim_of_yaml_exn


let dataset root name arr =
  let open Bigarray in
  let attr = `O [ "dim", Genarray.(dims arr) |> dim_to_yaml ] in
  let path = elt ~attr root name "dataset" in
  let file =
    Unix.openfile
      Fpath.(path / "data.bin" |> to_string)
      [ Unix.O_RDWR; Unix.O_CREAT ]
      0o640
  in
  let arr_mem = Unix.map_file file Float64 c_layout true Genarray.(dims arr) in
  Genarray.blit arr arr_mem;
  Unix.close file;
  path
