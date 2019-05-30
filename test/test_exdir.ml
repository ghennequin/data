open Exdir

let f = file Fpath.(v "bla")

let g1 = group f "mygroup"
let _ = Printf.printf "file: %s\n%!" (path_of f)
let _ = Printf.printf "group: %s\n%!" (path_of g1)

let d1 = dataset g1 "owlarray" Owl.Mat.(gaussian 10 20)

let _ =
  let d = dim d1 in
  Printf.printf "%i, %i\n%!" d.(0) d.(1)

