# Data management module

The API documentation is [here](https://hennequin-lab.github.io/docs/data/Data/index.html).

```ocaml
(* create the h5 object *)
let h5 = Data.(h5 (in_dir "results.h5"))

(* save a matrix *)
let x = Owl.Mat.gaussian 10 20
let _ = Data.Mat.save ~h5 "group1/group2/x" x

(* create the json object for saving parameters *)
let json = Data.(json (in_dir "results.json"))

(* define some parameters and save them in the json file *)
let n = 5 |> Data.int ~json "n"
let dir = "strawberry" |> Data.string ~json "dir"
let pi = 3.1415 |> Data.float ~json "pi"

(* more advanced example showing the use of an automatic
   Json converter from a custom record type *)
type prms =
  { sim : [ `test | `real ]
  ; other : bool
  }
[@@deriving yojson]

let p = { sim = `test; other = true } |> Data.any ~json "prms" prms_to_yojson
```
