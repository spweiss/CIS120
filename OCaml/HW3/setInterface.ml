(* Do NOT modify this file. *)

(******************************************************************************)
(* ABSTRACT SET INTERFACE                                                     *)
(******************************************************************************)

(* This module signature defines the values associated with the abstract type
 * `'a set`. Any module struct that implements the `SET` interface must contain
 * all of the values defined in this interface, otherwise it won't compile.
 *
 * You should read through this interface to understand the behaviors it
 * specifies, before moving on to writing test cases in the file setTest.ml. *)

module type SET = sig

  (* The "abstract type" 'a set.
   *
   * A set is an _unordered_ collection of _distinct_ elements.
   *
   * In mathematics, sets are typically written with curly braces; e.g. the set
   * containing four, five, and six would be written {4, 5, 6} (but note that
   * this is NOT valid OCaml syntax for a set).
   *
   * By unordered, we mean that {4, 5, 6} and {5, 6, 4} are equivalent
   * mathematically. By distinct, we mean that a set contains any particular
   * element only either 0 or 1 times (no duplicates are allowed). *)
  type 'a set

  (* The empty set has no elements. *)
  val empty : 'a set

  (* `is_empty s` is true exactly when s has no elements. *)
  val is_empty : 'a set -> bool

  (* `member x s` returns true exactly when x is a member of s. *)
  val member : 'a -> 'a set -> bool

  (* `add x s` returns a set just like s, except x is now also an element. If x
   * is already an element of s, then it just returns s. For example:
   *
   *   add 3 {4, 5, 6} = {3, 4, 5, 6}
   *   add "a" {"b", "a", "c"} = {"b", "a", "c"}
   *)
  val add : 'a -> 'a set -> 'a set

  (* `remove x s` returns a set just like s, except x is not an element. If x
   * already wasn't an element, then it just returns s. *)
  val remove : 'a -> 'a set -> 'a set

  (* `size s` returns the "cardinality" (number of elements) of s. *)
  val size : 'a set -> int

  (* `equals s1 s2` returns true exactly when s1 and s2 have the same elements.
   * For example:
   *
   *   equals {"a", "b"} {"b", "a"} = true
   *   equals {true} {false} = false
   *)
  val equals : 'a set -> 'a set -> bool

  (* `set_of_list l` returns a set containing all the elements of the list l. *)
  val set_of_list : 'a list -> 'a set

  (* `list_of_set s` returns a list of all the elements of s. This list should
   * be sorted in ascending order and may not contain duplicate elements. *)
  val list_of_set : 'a set -> 'a list

  (* This value is for debugging purposes only: it allows us to print out the
   * name of the implementation that's currently being tested. *)
  val debug_name: string

  (* In practice, we would want a richer interface for working with sets, 
   * possibly including operations like "intersection" and "union", as
   * well as the set analog of transform and fold.  We don't include these
   * operations in this HW, though. *)
end

