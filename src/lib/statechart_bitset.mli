
type t = int array

val length : t -> int
val make : int -> t
val init : (int -> bool) -> int -> t
val get : t -> int -> bool
val set : t -> int -> unit
val clear : t -> int -> unit
val has_and : t -> t -> bool
val clear_all : t -> unit
val has_any : t -> bool
val copy : t -> t
val copy_clear : t -> t
val bor : t -> t -> unit
val bxor : t -> t -> unit
val band : t -> t -> unit

val iter_left : (int -> unit) -> t -> unit
val iter_right : (int -> unit) -> t -> unit
val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a
val fold_right : ('a -> int -> 'a) -> 'a -> t -> 'a
val filter : (int -> bool) -> t -> t
val find_left : (int -> bool) -> t -> int option
val find_right : (int -> bool) -> t -> int option
val first : t -> int option
val last : t -> int option

val of_list : int list -> t
val to_list : t -> int list
val of_idx_array : int -> int array -> t
val to_idx_array : t -> int array
val of_string : string -> t
val to_string : t -> string
