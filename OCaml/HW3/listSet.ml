;; open Assert
;; open HigherOrder

(******************************************************************************)
(* PROBLEM 6: ORDERED LIST SET                                                *)
(******************************************************************************)

(* We will first implement the `SET` interface using a list. Because a set is
 * an "abstract type", it does not have a concrete implementation built into
 * OCaml. Instead, it is defined in terms of its behaivor (its interface).
 * Any implementation that conforms to the interface and exhibits the
 * desired properties is a set.
 *
 * The OCaml module system allows us to hide some information about how our
 * set is represented under the hood, and gives us the opportunity to control
 * how values of type `'a set` can be created. This allows us to maintain
 * "representation invariants" (see Lecture) so our lists behaves like a set. *)

module OLSet : SetInterface.SET = struct

  (* We now inform OCaml that the abstract type 'a set is, behind the scenes,
   * actually a `'a list`. This information is not available to any clients of
   * the `SET` interface, because it is an implementation detail.
   *
   * NOTE: We are going to maintain the additional invariants that this list
   * is sorted in ascending order, and contains no duplicates. *)

  type 'a set = 'a list

  let empty : 'a set = []

  let is_empty (s: 'a set) : bool =
    s = empty


  (* You already implemented a generic version of this function in problem 1,
   * but now write it using `fold`. You may not add the `rec` keyword. *)

  let member (x: 'a) (s: 'a set) : bool =
    fold (fun n acc -> n = x || acc) false s


  (* This function will be a bit more complex than just cons-ing the element
   * onto the list. Remember, we're maintaining the invariants that this list
   * is sorted and contains no duplicate elements. You may NOT use a sorting
   * function (such as `List.sort`) to accomplish this. *)

  let rec add (x: 'a) (s: 'a set) : 'a set =
    begin match s with
    | [] -> [x]
    | p :: ps -> if x < p then x :: s
                 else if x = p then s
                 else s @ [x]
    end


  (* The `remove` function returns a set without `x`. Try to use the invariants
   * in order to "short-circuit" (terminate earlier than the end of the list) if
   * the element is not found in its expected position. We will be automatically
   * testing that this function short-circuits. *)

  let rec remove (x: 'a) (s: 'a set) : 'a set =
    begin match s with
    | [] -> empty
    | p :: ps -> if x < p then s
                 else if x = p then ps
                 else p :: (remove x ps)
    end


  (* Because one of our representation invariants requires that our list contain
   * no duplicate elements, it is much easier to calculate the size of the set.
   * Implement this function using `fold`. You may not add the `rec` keyword. *)

  let size (s: 'a set) : int =
    fold (fun x acc -> 1 + acc) 0 s


  (* We will be automatically testing that your implementation of `equals`
   * makes use of the invariants to ensure efficiency. *)

  let equals (s: 'a set) (t: 'a set) : bool =
    s = t


  (* Remember that because it accepts arbitrary input of type `'a list`, this
   * function will have to ensure that the resulting list follows the
   * invariants. You should write some tests to ensure this is the case! *)

  let rec set_of_list (l: 'a list) : 'a set =
    begin match l with
    | [] -> empty
    | x :: xs -> add x (set_of_list xs)
    end

  let test () : bool =
    set_of_list [1; 2; 3] = add 3 (add 2 (add 1 empty))
  ;; run_test "{1, 2, 3} is generated" test

  let test () : bool =
    set_of_list [] = empty
  ;; run_test "{} is generated" test

  let test () : bool =
    set_of_list [1; 1; 2; 3] = add 3 (add 2 (add 1 empty))
  ;; run_test "filters duplicates" test

  let test () : bool =
    set_of_list [4; 1; 2; 3] = add 4 (add 3 (add 2 (add 1 empty)))
  ;; run_test "given unordered list" test


  (* This function should return a list which is sorted in ascending order and
   * contains no duplicate elements. We will be automatically testing that you
   * are leveraging the invariants to write this as efficiently as possible. *)

  let list_of_set (s: 'a set) : 'a list =
    fold (fun x acc -> x :: acc) [] s

  (* Don't modify this: for testing purposes. *)
  let debug_name: string = "OLSet"
end

