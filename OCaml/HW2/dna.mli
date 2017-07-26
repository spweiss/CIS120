(* You do not need to edit this file. *)
type nucleotide = G | C | A | T
type helix = nucleotide list
val complementary_helix : helix -> helix 
val lar_gibbon : helix
val pileated_gibbon : helix 
val siamang : helix 
val white_cheeked_gibbon : helix 
val orangutan : helix 
val gorilla : helix 
val chimpanzee : helix
val human : helix
val string_of_ape : helix -> string
val hamming_distance : helix -> helix -> int
val mutation_string : helix -> string
val question_most_like_human : unit -> helix list
val decreasing_similarity : helix list -> bool
type acid =
    Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Glu
  | Gln
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val
  | END
val acid_of_triplet : nucleotide -> nucleotide -> nucleotide -> acid
val acids_of_helix : helix -> acid list
type tree = Leaf of helix | Node of tree * tree
val greater_apes : unit -> tree
val string_of_tree : tree -> string
val lesser_apes : unit -> tree
val count_leaves : tree -> int
val all_trees : helix -> helix -> helix -> helix -> tree list
val all_greater_ape_trees : unit -> tree list
val all_lesser_ape_trees : unit -> tree list
val string_of_tree_list : tree list -> string
type labeled_tree =
    LLeaf of helix
  | LNode of labeled_tree * helix * labeled_tree
val helix_of_tree : labeled_tree -> helix
val unlabel_tree : labeled_tree -> tree
val guess_parent_helix : helix -> helix -> helix
val add_ancestor_labels : tree -> labeled_tree
val labeled_greater_ape_trees : unit -> labeled_tree list
val labeled_lesser_ape_trees : unit -> labeled_tree list
val parent_child_hamming : labeled_tree -> int
val simplest_tree : labeled_tree list -> labeled_tree * int
val simplest_greater_ape_tree : unit -> tree
val simplest_lesser_ape_tree : unit -> tree 
val find_simplest_tree : helix -> helix -> helix -> helix -> tree 

val all_acids_of_helix : helix -> acid list list
