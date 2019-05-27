module P = struct
  open Sexplib.Std

  type t =
    { a : int
    ; b : float
    }
  [@@deriving sexp]
end

module D =
  Data.Make
    (P)
    (struct
      let file = `replace (Data.in_dir "results.h5")
    end)

let set = P.{ a = 999; b = 0.5 } |> D.Prms.t "set"
let n = 5 |> D.Prms.int "n"
let dir = Data.(in_dir "") |> D.Prms.string "dir"
let pi = 3.1415 |> D.Prms.float "pi"
