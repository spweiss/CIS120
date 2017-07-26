val assoc : 'k -> ('k *'v) list -> 'v option

val join_option : 'a option option -> 'a option

val cat_option : 'a option list -> 'a list

val partial_transform : ('a -> 'b option) -> 'a list -> 'b list

val iter : ('a -> unit) -> 'a list -> unit

val ref_incr : int ref -> int

val swap : 'a ref -> 'a ref -> unit

val refs_aliased : 'a ref -> 'a ref -> bool

val int_refs_aliased : int ref -> int ref -> bool

val contains_alias : 'a -> 'a list -> bool

val equality_test_results : unit -> bool list

val contains_alias_option : 'a -> 'a option list -> bool
