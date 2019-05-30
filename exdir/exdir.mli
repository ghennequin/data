type 'a t

val path_of : 'a t -> string
val meta_of : 'a t -> Yaml.value

(** operations on files *)
val file : ?attr:Yaml.value -> Fpath.t -> [ `file ] t

(** operations on groups *)
val group : ?attr:Yaml.value -> [< `file | `group ] t -> string -> [ `group ] t

(** operations on datasets *)
val dataset
  :  [< `file | `group ] t
  -> string
  -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t
  -> [ `dataset ] t

val dim : [ `dataset ] t -> int array
