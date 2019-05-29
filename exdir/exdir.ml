open Protocol_conv_yaml

type t = Fpath.t (* whole path *)

type meta =
  { version : string
  ; typ : [ `file | `group | `dataset | `raw ] [@key "type"]
  }
[@@deriving protocol ~driver:(module Yaml)]

type exdir_meta = { exdir : meta } [@@deriving protocol ~driver:(module Yaml)]

let default_exdir_meta typ = { exdir = { version = "1"; typ } }

let meta_of x =
  let y = Yaml_unix.of_file_exn Fpath.(x / "exdir.yml") |> exdir_meta_of_yaml_exn in
  y.exdir


let in_context_do ctx arg f =
  if Sys.file_exists Fpath.(to_string ctx) then f ctx arg else failwith "bad context"


let open_file path =
  if Sys.file_exists path && Sys.is_directory path
  then Fpath.(v path)
  else failwith "file does not exist; use [new_file] instead"


let new_elt ?attr root name typ =
  in_context_do root name (fun root name ->
      let path = Fpath.(root / name) in
      Unix.mkdir Fpath.(to_string path) 0o640;
      default_exdir_meta typ
      |> exdir_meta_to_yaml
      |> Yaml_unix.to_file_exn Fpath.(path / "exdir.yml");
      match attr with
      | None -> ()
      | Some y -> Yaml_unix.to_file_exn Fpath.(path / "attributes.yml") y)


let new_file ?attr path = new_elt ?attr Fpath.(v "") path `file

let new_group ?attr root name =
  match (meta_of root).typ with
  | `file | `group -> new_elt ?attr root name `group
  | _ -> failwith "can only create a group inside a file or another group"
