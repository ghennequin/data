module D = Data.Make (struct
  let file = `replace (Data.in_dir "results.h5")
end)

let n = 5 |> D.Prms.int "n"
let dir = Data.(in_dir "") |> D.Prms.string "dir"
let pi = 3.1415 |> D.Prms.float "pi"
