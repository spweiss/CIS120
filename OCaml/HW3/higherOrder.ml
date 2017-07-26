;; open Assert

(* NOTE: Throughout this assignment, you may NOT use any library functions,
 * in particular, the functions defined in the `List` module, unless given
 * specific instructions to. If you use such functions in your solutions, the
 * TAs reserve the right to deduct from your automatically graded score. *)

(******************************************************************************)
(* PROBLEM 1: GENERIC TYPES                                                   *)
(******************************************************************************)

(* Recall that "generic types" in OCaml can be used to help abstract out
 * frequently used programming patterns. Here are two functions that determine
 * whether an element is part of a list. *)

let rec int_member (i: int) (l: int list) : bool =
  begin match l with
  | [] -> false
  | x :: xs -> i = x || int_member i xs
  end

let rec string_member (s: string) (l: string list) : bool =
  begin match l with
  | [] -> false
  | x :: xs -> s = x || string_member s xs
  end

(* The pattern here is pretty obvious. Note that it doesn't depend on any
 * type-specific details (because the (=) operator is defined for all types).
 * Let's extract the pattern into a generic function that works for ANY type
 * of list. The `'a` is known as a "type variable", and allows any concrete
 * type to be substituted for it everywhere in the function. Complete the
 * following implementation of a genereic 'member' function: *)

let rec member (x: 'a) (l: 'a list) : bool =
  begin match l with
  | [] -> false
  | head :: tail -> head = x || member x tail
  end

(* Notice that we can use values of any type in our test cases, but we don't
 * need to test every single type. If this function can be shown to work
 * properly for ints, for example, then it will work for all other types too. *)

let test () : bool =
  not (member false [])
;; run_test "member: empty list" test

let test () : bool =
  member 1 [2; 4; 1; 5]
;; run_test "member: element is found" test

let test () : bool =
  not (member 'q' ['a'; 'b'; 'c'])
;; run_test "member: element not found" test

let test () : bool =
  member (5, 3) [(5, 3)]
;; run_test "member: element is found" test

(* Let's do something a bit more challenging. We'll first introduce the notion
 * of an "association list" (also known as a "dictionary"), which is a list of
 * tuples of keys and values. An example association list is below. *)

let assoc_list: (int * string) list =
  [(110, "Java"); (120, "OCaml"); (121, "Java"); (240, "C")]

(* Define a generic function `assoc` which, given a list of tuples of the form
 * (key, value), and a particular key, finds its associated value. If the key
 * is not present, the function should raise an error using `failwith`.
 * If there are multiple instances of a key, return the value associated with
 * the first one. *)

let rec assoc (key: 'k) (l: ('k * 'v) list) : 'v =
  begin match l with
  | [] -> failwith "Element not found"
  | (x, y) :: tail -> if x = key then y else assoc key tail
  end

let test () : bool =
  assoc 120 assoc_list = "OCaml"
;; run_test "assoc: key found" test

let test () : bool =
  assoc 42 assoc_list = "should fail"
;; run_failing_test "assoc: key not found" test

let test () : bool =
  assoc 42 [] = "should fail"
;; run_failing_test "assoc: empty list" test

let test () : bool =
  assoc 1 [(1, true); (1, false); (1, false)] = true
;; run_test "assoc: multiple keys" test

(******************************************************************************)
(* PROBLEM 2: ANONYMOUS & HIGHER-ORDER FUNCTIONS                              *)
(******************************************************************************)

(* Recall the `transform` function from lecture. It is a "higher-order function"
 * because it takes another function as an argument, and applies it to every
 * element of a list. *)

let rec transform (f: 'a -> 'b) (l: 'a list) : 'b list =
  begin match l with
  | [] -> []
  | x :: xs -> f x :: transform f xs
  end

(* We can use `transform` to help define other functions. Write a function that
 * capitalizes every string in a list of strings. You can use the function
 * `String.uppercase` (of type `string -> string`) in your implementation. Do
 * not add the `rec` keyword to this definition. *)

let capitalize (l: string list) : string list =
  transform String.uppercase l

let test () : bool =
  capitalize ["a"; "list"; "of"; "words"] = ["A"; "LIST"; "OF"; "WORDS"]
;; run_test "capitalize' non-empty list" test

let test () : bool =
  capitalize [] = []
;; run_test "capitalize' empty list" test

let test () : bool =
  capitalize ["a"] = ["A"]
;; run_test "capitalize' singleton list" test

let test () : bool =
  capitalize ["A"] = ["A"]
;; run_test "capitalize' capitalized list" test

(* Recall the `fold` higher-order function, which collapses a list into a
 * single result value. Its definition is below. *)

let rec fold (combine: 'a -> 'b -> 'b) (base: 'b) (l: 'a list) : 'b =
  begin match l with
  | [] -> base
  | x :: xs -> combine x (fold combine base xs)
  end

(* Here's an example use of `fold` to sum an integer list. Notice that the `x`
 * is the element at the head of `l`, and `acc` is the accumulation of calls
 * to `fold` on the remainder of the list. In terms of types, `x` will be of the
 * same type as the elements in the list, and `acc` (as well as the base case)
 * will be same type as the result of the fold. *)

let sum (l: int list) : int =
  fold (fun x acc -> x + acc) 0 l

(* Try your hand at implementing the `list_length` function using `fold`.  Do
 * not add `rec` to the declaration of `list_length` *)

let list_length (l: 'a list) : int =
  fold (fun _ acc -> 1 + acc) 0 l

(* Now, use `fold` to write `concat`, which takes a list of lists and "flattens"
 * it into a single list containing all the elements of the lists, in order.
 * It is okay to use the built-in `@` function, which appends two lists.  Do
 * not add `rec` to the declaration of `concat`. *)

let concat (l: 'a list list) : 'a list =
  fold (fun x acc -> x @ acc) [] l

let test () : bool =
  concat [[]] = []
;; run_test "concat list of empty list" test

let test () : bool =
  concat [[1; 2]; [3]; [4; 5; 6]] = [1; 2; 3; 4; 5; 6]
;; run_test "concat list of int lists" test

let test () : bool =
  concat [[]; []] = []
;; run_test "concat list of empty lists" test

(* Use `fold` to write a function that reverses a generic list. Again, you may
 * use the `@` function to append two lists. Do not add the `rec` keyword. *)

let reverse (l: 'a list) : 'a list =
  fold (fun x acc -> acc @ [x]) [] l

let test () : bool =
  reverse ["you"; "are"; "how"; "hi"] = ["hi"; "how"; "are"; "you"]
;; run_test "reverse list" test

let test () : bool =
  reverse [] = []
;; run_test "reverse empty list" test

let test () : bool =
  reverse [1; 2; 3] = [3; 2; 1]
;; run_test "reverse int list" test

let test () : bool =
  reverse [true; false] = [false; true]
;; run_test "reverse bool list" test

(* We can also use `fold` to write other higher-order functions. Two other
 * common higher-order functions are `for_all` and `exists` (you may have
 * seen these before in CIS 160). They both operate on lists of elements, and
 * apply a "predicate" function that returns true or false for each element
 * of that list.
 *
 * Use a single call to `fold` to implement both of these functions. Your
 * solution should be only one line in length, and you may not add the `rec`
 * keyword to the function declarations. Hint: think carefully about why the
 * base cases of each function are different. *)

(* The `for_all` function returns true only if every element in the list `l`
 * satisfies the predicate (i.e., `pred x` is true for all `x` in `l`). *)

let for_all (pred: 'a -> bool) (l: 'a list) : bool =
  fold (fun x acc -> pred x && acc) true l

let test () : bool =
  for_all (fun x -> x > 0) []
;; run_test "for_all base case" test

let test () : bool =
  not (for_all (fun x -> x > 0) [1; 2; -5; 3])
;; run_test "for_all not satisfied" test

let test () : bool =
  for_all (fun x -> x > 0) [1; 2; 5; 3]
;; run_test "for_all satisfied" test

let test () : bool =
  for_all (fun x -> (x mod 2 = 0)) [4; 2; 8; 6]
;; run_test "for_all evens" test

(* The `exists` function returns true only if there is some element in the
 * list `l` that satisfies the predicate (i.e., `pred x` is true for at least
 * one `x` in `l`). *)

let exists (pred: 'a -> bool) (l: 'a list) : bool =
  fold (fun x acc -> pred x || acc) false l

let test () : bool =
  not (exists (fun x -> x > 0) [])
;; run_test "exists base case" test

let test () : bool =
  exists (fun x -> x > 0) [1; 2; -5; 3]
;; run_test "exists satisfied" test

let test () : bool =
  not (exists (fun x -> x > 0) [-1; -2; -5; -3])
;; run_test "exists not satisfied" test

let test () : bool =
  exists (fun x -> (x mod 2 = 0)) [1; 2; -5; 3]
;; run_test "exists evens" test

(* The last higher-order function we'll explore is `filter`, which, as its name
 * might suggest, uses a predicate function to filter out elements that don't
 * satisfy the condition. *)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  begin match l with
  | [] -> []
  | x :: xs ->
      let rest = filter pred xs in
      if pred x then x :: rest else rest
  end

(* Rewrite `filter` using `fold`. It should pass the test cases below. *)

let filter' (pred: 'a -> bool) (l: 'a list) : 'a list =
  fold (fun x acc -> if pred x then x :: acc else acc) [] l

let test () : bool =
  filter' (fun x -> x > 0) [1; 2; -5; 3] = [1; 2; 3]
;; run_test "filter' positive ints" test

let test () : bool =
  filter' (fun _ -> false) ["a"; "b"; "c"] = []
;; run_test "filter' everything false" test

let test () : bool =
  filter' (fun _ -> true) [] = []
;; run_test "filter' empty list" test

let test () : bool =
  filter' (fun x -> x = "a") ["a"; "b"; "c"] = ["a"]
;; run_test "filter' string" test