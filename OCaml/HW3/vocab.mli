(* Do NOT modify this file. *)

open TreeSet.BSTSet

val ocaml_vocab : string list
val sat_vocab : string list
val count_if : ('a -> bool) -> 'a list -> int
(* val print_counts : string -> (string set -> int) -> unit *)

val count_words_in_text : string list -> string set -> int
val count_ocaml_words : string set -> int

val count_sat_words : string set -> int
val modest_sat_words : int
val weak_sat_words : int
val twentyk_sat_words : int

val count_length_gt7 : string set -> int
val modest_length_gt7 : int
val weak_length_gt7 : int
val twentyk_length_gt7 : int

val count_sat_vocab_starting_with : char -> string set -> int
val frequency_table : (char * (int list)) list
