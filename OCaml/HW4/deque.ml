(******************************************************************************)
(* Problem 5: Deques **********************************************************)
(******************************************************************************)

open Assert
open Imp

(* Deques are like queues, but with the ability to insert and delete at both
   ends. They are "double-ended queues". Your task here is to implement deques
   using doubly linked lists of nodes in the heap--i.e., the nodes of these
   lists will have pointers to the next *and* previous nodes.

   Hint: When working out how to implement these programs, you will probably
   find it VERY HELPFUL to work things out on paper using "box and pointer"
   diagrams in the style of the Abstract Stack Machine. *)

(* A deque is like a queue but with links in both directions.

   A dqnode contains a value and references to the next AND PREVIOUS nodes in
   the structure. *)

type 'a dqnode = {
  v: 'a;
  mutable next: 'a dqnode option;
  mutable prev: 'a dqnode option;
}

type 'a deque = {
  mutable head: 'a dqnode option;
  mutable tail: 'a dqnode option;
}

(**********************************************)
(****** DEQUE implementation INVARIANT ********)

(* INVARIANT: either...

   The deque is empty, and the head and tail are both None,

   or, the deque is non-empty, and:
   - head = Some n1 and tail = Some n2
      (a) n2 is reachable from n1 by following 'next' pointers
      (b) n2.next = None   (there is no element after the tail)

      (c) n1 is reachable from n2 by following 'prev' pointers
      (d) n1.prev = None   (there is no element before the head)

      (e) for every node n in the deque:
          if n.next = Some m then
            m.prev = Some mp and n == mp
  
      (f) if n.prev = Some m then
            m.next = Some mn and n == mn
*)

(* Below are some provided functions intended to check if a given deque
   satisfies the deque invariants. While you do not have to write the code for
   these, take some time to read over them and understand how they work. You
   will find yourself writing functions of similar structure below. *)

(* Helper function for 'valid' which checks deque invariants from head to tail.
   The tail of the deque is returned if traversal is successful. *)
let check_to_tail (n: 'a dqnode) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    begin match curr.next with
    | None -> Some curr
    | Some m ->
        begin match m.prev with
        | None -> None
        | Some mp ->
            if mp != curr || contains_alias curr seen
            then None
            else loop m (curr :: seen)
        end
    end
  in loop n []

(* Helper function for 'valid' which checks deque invariants from tail to head.
   The head of the deque is returned if traversal is successful. *)
let check_to_head (n: 'a dqnode) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    begin match curr.prev with
    | None -> Some curr
    | Some m ->
        begin match m.next with
        | None -> None
        | Some mn ->
            if mn != curr || contains_alias curr seen
            then None
            else loop m (curr :: seen)
        end
    end
  in loop n []

(* Function which checks whether a given deque is valid. The function performs a
   check of invariants in both directions down the deque, and then checks if the
   head and tail of the given deque match those returned by check_to_tail and
   check_to_head. *)
let valid (d: 'a deque) : bool =
  begin match d.head, d.tail with
  | None, None -> true
  | Some h, Some t ->
      begin match check_to_tail h, check_to_head t with
      | Some n2, Some n1 -> n2 == t && n1 == h
      | _, _ -> false
      end
  | _, _ -> false
  end


(*****************************************)
(****** DEQUE examples and cycles ********)

(* Write a function that returns an empty deque. *)
let create () : 'a deque =
  { head = None; tail = None }

(* We've done this one for you. Do not change it! *)
let is_empty (d: 'a deque) : bool =
  d.head = None && d.tail = None

(* This function directly makes a *valid* deque of length 1. *)
let deque_1 () : int deque =
  let n = { v = 1; prev = None; next = None } in
  { head = Some n; tail = Some n }

(* Write a function that makes a *valid* deque of length 2. *)
let deque_12 () : int deque =
  let x = { v = 1; prev = None; next = None } in
  let y = { v = 2; prev = None; next = None } in
  x.next <- Some y;
  y.prev <- Some x;
  { head = Some x; tail = Some y }

(* Note that if you draw the result of the ASM for a correct deque_12, you'll
   get a heap with a *cycle*---when following a combination of next or prev
   pointers you can get back to where you started. Deques are different from
   queues in that valid deques with length >= 2 will always have cycles. In
   contrast, all cyclic queues are invalid. *)

(* The next two tests demonstrate the difference between = and == with respect
   to cyclic datastructures like deques.  The first one passes trivially, the
   second test should fail because OCaml cannot figure out if cyclic
   datastructures are structurally equal. *)

;; run_test "Trivial test" (fun () ->
  let d = deque_12 () in
  d.head == d.head)

;; run_failing_test "Bad test" (fun () ->
  let d = deque_12 () in
  d.head = d.head)

(* Write down an *invalid* deque that does not contain any cycles. There should
   be no path of pointers that will allow you to return to a previously visited
   node. *)
let invalid_acyclic () : int deque =
  let x = { v = 1; prev = None; next = None } in
  let y = { v = 2; prev = None; next = None } in
  { head = Some x; tail = Some y }

(* Of course, not all deques with cycles are valid. Write down an *invalid*
   deque that contains a cycle. *)
let cycle () : int deque =
  let x = { v = 1; prev = None; next = None } in
  let y = { v = 2; prev = None; next = None } in
  let z = { v = 3; prev = None; next = None } in
  x.next <- Some y;
  y.prev <- Some x; y.next <- Some z;
  { head = Some x; tail = Some y }

(**********************************************)
(****** DEQUE implementation exercises ********)

(* NOTE: add all of your tests for the following deque functions at the bottom
   of the file.  Look for specified sections to write certain tests. *)

(* Write a function to return a list of elements in the deque, ordered from the
   head to the tail.

   This function should create the list in a single iteration of the deque with
   no additional processing. This means you will get full credit only if you
   implement to_list using tail call recursion. Do not use or reimplement the
   List library functions, @, or reverse.

   HINT: Why are those List operations needed for singly-linked queues? *)

let to_list (q: 'a deque) : 'a list =
  if not (valid q) then failwith "to_list: given invalid deque";
  let rec loop (dqno: 'a dqnode option) (l: 'a list) : 'a list =
    begin match dqno with
    | None -> l
    | Some n -> loop n.prev (n.v :: l)
    end
  in loop q.tail []

(* Write two functions---insert_head and insert_tail---for adding elements to
   the head and tail of a deque, respectively.

   You may assume that the deque input satisfies the deque invariants.

   Just as with queues, you should update the deque in place, and make sure
   to maintain the invariants. *)

let insert_head (x: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "insert_head: given invalid deque";
  let a = { v = x; prev = None; next = None } in
    begin match q.head with
    | None -> q.head <- Some a; q.tail <- Some a;
    | Some n -> n.prev <- Some a; a.next <- Some n; q.head <- Some a;
    end

let insert_tail (x: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "insert_tail: given invalid deque";
  let a = { v = x; prev = None; next = None } in
    begin match q.tail with
    | None -> q.head <- Some a; q.tail <- Some a;
    | Some n -> n.next <- Some a; a.prev <- Some n; q.tail <- Some a;
    end

(* Write two functions, remove_head and remove_tail, that remove and then return
   the first and last elements of a deque, respectively.

   These functions should fail if the deque is empty.

   Again, you should update the deque in place, and be sure to maintain the
   deque invariants. *)


let remove_head (q: 'a deque) : 'a =
  if not (valid q) then failwith "remove_head: given invalid deque";
  begin match q.head with
  | None -> failwith "remove_head called on empty deque"
  | Some n -> begin match n.next with
              | None -> q.head <- None; q.tail <- None; n.v
              | Some m -> m.prev <- None; n.next <- None; q.head <- Some m; n.v
              end
  end

let remove_tail (q: 'a deque) : 'a =
  if not (valid q) then failwith "remove_tail: given invalid deque";
  begin match q.tail with
  | None -> failwith "remove_head called on empty deque"
  | Some n -> begin match n.prev with
              | None -> q.head <- None; q.tail <- None; n.v
              | Some m -> m.next <- None; n.prev <- None; q.tail <- Some m; n.v
              end
  end

(* Write a function that removes the last (nearest the tail) occurrence of a
   given element from a deque. *)
(* NOTE: your TAs will be grading your test cases for this function. Remember
   to add tests to the part marked below before completing this problem. *)
let delete_last (v: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "delete_last: given invalid deque";
  let rec loop (dqno: 'a dqnode option) : unit =
    begin match dqno with
    | None -> ()
    | Some n -> if n.v = v then
                  if q.tail == dqno then
                    begin match n.prev with
                    | None -> (q.head <- None; q.tail <- None)
                    | Some m ->
                        (m.next <- None; n.prev <- None; q.tail <- Some m)
                    end
                  else if q.head == dqno then
                    begin match n.next with
                    | None -> q.head <- None; q.tail <- None;
                    | Some m ->
                        m.prev <- None; n.next <- None; q.head <- Some m;
                    end
                  else (begin match n.next with
                        | None -> (q.head <- None; q.tail <- None)
                        | Some m -> (m.prev <- n.prev)
                        end;
                        begin match n.prev with
                        | None -> (q.head <- None; q.tail <- None)
                        | Some m -> (m.next <- n.next)
                        end;
                        n.next <- None; n.prev <- None)
                else loop n.prev
    end
  in loop q.tail

(* Write a function that deletes the first (nearest the head) occurrence of a
   given value. *)
let delete_first (v: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "delete_first: given invalid deque";
  let rec loop (dqno: 'a dqnode option) : unit =
    begin match dqno with
    | None -> ()
    | Some n -> if n.v = v then
                  if q.tail == Some n then
                    begin match n.prev with
                    | None -> q.head <- None; q.tail <- None;
                    | Some m ->
                        m.next <- None; n.prev <- None; q.tail <- Some m;
                    end
                  else if q.head == Some n then
                    begin match n.next with
                    | None -> q.head <- None; q.tail <- None;
                    | Some m ->
                        m.prev <- None; n.next <- None; q.head <- Some m;
                    end
                  else (begin match n.next with
                        | None -> q.head <- None; q.tail <- None;
                        | Some m -> m.prev <- n.prev;
                        end;
                        begin match n.prev with
                        | None -> q.head <- None; q.tail <- None;
                        | Some m -> m.next <- n.next;
                        end)
                else loop n.next
    end
  in loop q.head

(* Write a function to mutate the given deque in-place, so that the order of the
   element is reversed. (i.e.: traversal (such as to_list) visits the nodes in
   the opposite order.)

   In-place means that after reversal, the deque uses the same qnodes as before
   it started.

   It is possible to satisfy the tests for this problem without doing an
   in-place reversal (for example, by removing all of the elements, storing them
   in a list, reversing the list and then inserting the elements back). However,
   this is not the solution we are looking for and you will lose points from
   your style grade if you submit it. 

   We will manually grade the tests for reverse. *)
let reverse (q: 'a deque) : unit =
  if not (valid q) then failwith "reverse: given invalid deque";
  let rec loop (dqno: 'a dqnode option) : unit =
    begin match dqno with
    | None -> ()
    | Some n -> begin match (n.next, n.prev) with
                | (None, None) -> ()
                | (None, Some m) ->
                  (n.prev <- None; n.next <- Some m;
                  q.tail <- q.head; q.head <- dqno)
                | (Some m, None) ->
                  (n.next <- None; n.prev <- Some m; loop (Some m))
                | (Some a, Some b) ->
                  (n.next <- Some b; n.prev <- Some a; loop (Some a))
                end
    end
  in loop q.head


(****************************************************************)
(**** Deque kudos problem (ho_valid) ****************************)
(****************************************************************)

(* Our definitions of check_to_tail and check_to_head above share a lot of code.
   Write a higher-order function that will allow you to write both check_to_tail
   and check_to_head by calling this function. Make sure your new implementation
   behaves like the old one.

   Note: The required functionality will provide you with the ability to do the
   loop in both directions. You could also try making the functionality of the
   loop a parameter of the function like fold does for lists, although we won't
   be testing for that. *)
let ho_check (n: 'a dqnode) (one_way: 'a dqnode -> 'a dqnode option)
             (other_way: 'a dqnode -> 'a dqnode option) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    failwith "ho_check unimplemented"
  in loop n []

(* Next, implement ho_check_to_tail using ho_check. Add tests to make sure
   ho_check_to_tail works exactly like check_to_tail. *)
let ho_check_to_tail (n: 'a dqnode) : 'a dqnode option =
  failwith "ho_check_to_tail unimplemented"

(* Now, do the same thing for check_to_head. *)
let ho_check_to_head (n: 'a dqnode) : 'a dqnode option =
  failwith "ho_check_to_tail unimplemented"

(* Finally we provide ho_valid: a variant of valid that uses the ho_ 
   versions of the checks. *)
let ho_valid (d: 'a deque) : bool =
  begin match d.head, d.tail with
  | None, None -> true
  | Some h, Some t ->
      begin match ho_check_to_tail h, ho_check_to_head t with
      | Some n2, Some n1 -> n2 == t && n1 == h
      | _, _ -> false
      end
  | _, _ -> false
  end



(****************************************************************)
(**** Testing code for deques. **********************************)
(****************************************************************)

(* We have provided a few tests; you should add your own here. *)



let test () : bool =
  valid (deque_1 ())
;; run_test "valid deque_1" test

let test () : bool =
  valid (deque_12 ())
;; run_test "valid deque_12" test

let test () : bool =
  not (valid (invalid_acyclic ()))
;; run_test "invalid_acyclic" test

let test () : bool =
  not (valid (cycle ()))
;; run_test "cycle invalid" test

let test () : bool =
  let d = create () in
  insert_head true d;
  remove_head d
;; run_test "insert true; remove" test

let test () : bool =
  let d = create () in
  insert_head false d;
  insert_head true d;
  remove_head d
;; run_test "insert_head; insert_head; remove_head" test

let test () : bool =
  let d = create () in
  insert_tail true d;
  remove_head d
;; run_test "insert_tail true; remove_head" test

(* --------------------------------------------------- *)
(* Add tests for delete_last here.                     *)

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  let d2 = create () in
  insert_tail 1 d2;
  insert_tail 2 d2;
  insert_tail 3 d2;
  delete_last 4 d;
  d = d2
;; run_test "delete_last at tail" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  let d2 = create () in
  insert_tail 2 d2;
  insert_tail 3 d2;
  insert_tail 4 d2;
  delete_last 1 d;
  d = d2
;; run_test "delete_last at head" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  let d2 = create () in
  insert_tail 1 d2;
  insert_tail 3 d2;
  insert_tail 4 d2;
  delete_last 2 d;
  d = d2
;; run_test "delete_last in middle" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 3 d;
  insert_tail 3 d;
  insert_tail 3 d;
  let d2 = create () in
  insert_tail 1 d2;
  insert_tail 3 d2;
  insert_tail 3 d2;
  delete_last 3 d;
  d = d2
;; run_test "delete_last multiple occurences" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  let d2 = create () in
  insert_tail 1 d2;
  insert_tail 2 d2;
  insert_tail 3 d2;
  insert_tail 4 d2;
  delete_last 5 d;
  d = d2
;; run_test "delete_last not present" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  let d2 = create () in
  delete_last 1 d;
  d = d2
;; run_test "delete_last singleton" test

(* --------------------------------------------------- *)


let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  let l1 = to_list d in
  reverse d;
  let l2 = to_list d in
  l2 = List.rev l1
;; run_test "reverse deque 1 2 3" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  let l1 = to_list d in
  reverse d;
  let l2 = to_list d in
  l2 = List.rev l1
;; run_test "reverse deque 1 2 " test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  let l1 = to_list d in
  reverse d;
  let l2 = to_list d in
  l2 = List.rev l1
;; run_test "reverse deque 1" test

let test () : bool =
  let d = create () in
  let l1 = to_list d in
  reverse d;
  let l2 = to_list d in
  l2 = List.rev l1
;; run_test "reverse deque empty" test
(* STUBWITH *)
