# Data management module

```ocaml
(* create the main data object *)
let data = Data.file (`replace Data.(in_dir "results.h5"))

(* save a few simulation parameters *)
let n = 5 |> Data.Param.int ~data "n"
let dir = "strawberry" |> Data.Param.string ~data "dir"
let pi = 3.1415 |> Data.Param.float ~data "pi"

(* save a matrix *)
let x = Owl.Mat.gaussian 10 20
let () = Data.Mat.save ~data "group1/group2/x" x

(* a more advanced example based on ppx_deriving of type-to-sexp converters *)
module P = struct
  open Sexplib.Std

  type t =
    { a : int list
    ; b : float
    }
  [@@deriving sexp]
end

(* [x] has type P.t (the struct above), and gets stored as a sexp in the hdf5 file *)
let x = Data.Param.t ~data (module P) "param_set" P.{ a = [ 0; 1 ]; b = 0.5 }
```
