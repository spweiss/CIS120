(******************************************************************************)
(* PROBLEM 5: WRITING TEST CASES                                              *)
(******************************************************************************)

(* `SetTest` is a reuseable module that we'll use to test other modules that
 * conform to the `SET` interface. When `SetTest` is instantiated with a
 * particular implementation, it will run all of its test cases against the
 * set type defined in that implementation.  This means that the _same_ tests
 * can be used for both the OLSet and BSTSet implementations (and that both
 * implementations should behave the same for these tests!).
 *
 * Read through the module, and then write your test cases in the space
 * provided below. Your TAs will be grading the completeness of your tests. *)

module SetTest (SetImpl: SetInterface.SET) = struct
  ;; open SetImpl

  (* We'll redefine the `run_test` and `run_failing_test` functions so that
   * they prepend the name of the set we're testing to the test description. *)

  let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
  let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

  ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

  (* We'll write all our test cases for the `SET` abstract type below.
   * Here are a few to help get you started. *)

  let test () : bool =
    is_empty empty
  ;; run_test "empty set is empty" test

  (* Note that some tests in this test module (such as the one below) may not
   * pass until all the functions they depend on are implemented. For instance,
   * the test below will fail for sets whose `set_of_list` function is not yet
   * implemented (even if `is_empty` is correct). Therefore, it's also important
   * to write unit-tests for individual functions as you go. *)

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (is_empty s)
  ;; run_test "{1, 2, 3} is not empty" test


  (* Now, it's your turn! Make sure to comprehensively test all the other
   * functions defined in the `SET` interface. It will probably be helpful to
   * have the file setInterface.ml open as you work.  Your tests should
   * stress the abstract properties of what it means to be a set, as well
   * as the relationships among the operations provided by the SET interface.
   *
   * Your TAs will be manually grading the completeness of your test cases. *)

  (* ---------- Write your own test cases below. ---------- *)

  let test () : bool =
    not (member 1 empty)
  ;; run_test "1 is not a member of the empty set" test
  
  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    member 1 s
  ;; run_test "1 is a member of {1, 2, 3}" test

  let test () : bool =
    let s = set_of_list [2; 2; 3] in
    not (member 1 s)
  ;; run_test "1 is not a member of {2, 3}" test

  let test () : bool =
    add 1 empty = set_of_list [1]
  ;; run_test "add 1 to empty is {1}" test

  let test () : bool =
    let s = set_of_list [2; 2; 3] in
    equals (add 1 s) (set_of_list [1; 2; 3])
  ;; run_test "add 1 to {2, 3} is {1, 2, 3}" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    add 1 s  = set_of_list [1; 2; 3]
  ;; run_test "add 1 to {1, 2, 3} is {1, 2, 3}" test
  
  let test () : bool =
    let s = add 1 (add 2 (add 3 empty)) in
    equals (remove 1 s) (add 2 (add 3 empty))
  ;; run_test "remove 1 from {1, 2, 3} is {2, 3}" test

  let test () : bool =
    let s = set_of_list [2; 2; 3] in
    remove 1 s  = set_of_list [2; 3]
  ;; run_test "remove 1 from {2, 3} is {2, 3}" test

  let test () : bool =
    remove 1 empty = set_of_list []
  ;; run_test "remove 1 from empty is empty" test

  let test () : bool =
    size empty = 0
  ;; run_test "size empty is 0" test

  let test () : bool =
    let s = set_of_list [2; 2; 3] in
    size s = 2
  ;; run_test "size of {2, 3} is 2" test

  let test () : bool =
    let s1 = set_of_list [2; 2; 3] in
    let s2 = set_of_list [2; 2; 3] in
    equals s1 s2
  ;; run_test "{2, 3} equals {2, 3}" test

  let test () : bool =
    let s1 = set_of_list [1; 2; 3] in
    let s2 = set_of_list [2; 2; 3] in
    not (equals s1 s2)
  ;; run_test "{1, 2, 3} does not equal {2, 3}" test

  let test () : bool =
    let s1 = set_of_list [] in
    let s2 = set_of_list [] in
    equals s1 s2
  ;; run_test "{} equals {}" test

  let test () : bool =
    set_of_list [] = empty
  ;; run_test "[] corresponds to {}" test

  let test () : bool =
    let s = add 1 empty in
    set_of_list [1] = s
  ;; run_test "[1] corresponds to {1}" test

  let test () : bool =
    let s = add 1 empty in
    set_of_list [1; 1] = s
  ;; run_test "[1; 1] corresponds to {1}" test

  let test () : bool =
    let s = add 2 (add 1 empty) in
    set_of_list [2; 1] = s
  ;; run_test "[2; 1] corresponds to {1, 2}" test

  let test () : bool =
    list_of_set empty = []
  ;; run_test "{} corresponds to []" test

  let test () : bool =
    let s = add 2 (add 1 empty) in
    list_of_set s = [1; 2]
  ;; run_test "{2, 1} corresponds to [1; 2]" test

  (* ---------- Write your own test cases above. ---------- *)

end

(* We now instantiate the tests so they are executed for OLSet and BSTSet.
 * Don't modify anything below this comment. *)

module TestOLSet = SetTest(ListSet.OLSet)
;; print_newline ()

module TestBSTSet = SetTest(TreeSet.BSTSet)
;; print_newline ()

