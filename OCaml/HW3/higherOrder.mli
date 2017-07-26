(* Do NOT modify this file. *)

val member: 'a -> 'a list -> bool
val assoc: 'k -> ('k * 'v) list -> 'v
val transform: ('a -> 'b) -> 'a list -> 'b list
val capitalize: string list -> string list
val fold: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val sum: int list -> int
val list_length: 'a list -> int
val reverse: 'a list -> 'a list
val concat: 'a list list -> 'a list
val for_all: ('a -> bool) -> 'a list -> bool
val exists: ('a -> bool) -> 'a list -> bool
val filter: ('a -> bool) -> 'a list -> 'a list
val filter': ('a -> bool) -> 'a list -> 'a list
