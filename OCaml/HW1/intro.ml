(* Homework Assigment 1: OCaml Finger Exercises! *)

(* The following command tells OCaml to use the "assert" library that
   defines the run_test command used below. See the web pages for
   instructions on how to set up an Eclipse project for this program. *)
;; open Assert

(* NOTE: you should _not_ use functions built into OCaml, especially the ones in
   the List module, except where they are explicitly allowed in the comments.
   The purpose of this assignment is to familiarize you with the basics of OCaml
   programming, so we want you to explicitly write out each of these problems
   even though there is often a built-in function that would achieve the same
   result. You will not receive credit for solutions that are contrary to the
   spirit of the assignment. *)

(*************************************************************************)
(* Problem 1 (counting coins) *)

(* Your job in this problem is to calculate the smallest number of
   pennies, nickels, and dimes that can be used to add up to the given
   amount. For example, to make 7 cents, a total of 3 coins are
   needed (two pennies and a nickel); to make 99 cents, 14 coins are
   needed (9 dimes, 1 nickel, and 4 pennies). Fill in the body of the
   function 'coins' below so that it returns the right answer. (Start
   by deleting the line beginning 'failwith'.) *)

let rec coins (amount: int) : int =
	if (amount < 0 || amount = 0) then 0
  else
    let numdimes (amount: int) : int =
		  (amount - (amount mod 10)) / 10 in
	  let numnickels (amount: int) : int =
		  (amount - (10 * numdimes (amount: int)) -
      ((amount - (10 * numdimes (amount: int))) mod 5)) / 5 in
	  let numpennies (amount: int) : int =
		  (amount - (10 * numdimes (amount: int)) -
      (5 * numnickels (amount: int))) in
	  numdimes (amount) + numnickels (amount) + numpennies (amount)

(* Here are two test cases for this problem. *)

let test () : bool =
  (coins 7) = 3
;; run_test "coins nickels and pennies" test

let test () : bool =
  (coins 99) = 14
;; run_test "coins all three" test

(* Here are two test case stubs. Please edit them to produce real
   tests for the coins function. For each of the problems in the
   assignment, we provide some test cases like the ones above.
   However, just because your code passes the given tests does not
   mean that you will get full credit. When you submit your
   assignment, we will test it using DIFFERENT tests from the ones
   above. To make sure that your solution is robust enough to pass our
   tests, you should think about what tests you can do to make sure
   that your program is correct.

   STARTING FROM HW 02 WE WILL GRADE YOU ON THE QUALITY AND ROBUSTNESS
   OF YOUR TEST CASES IN YOUR STYLE GRADE.

   Please refer to the FAQ page for an explanation about test cases. *)

let test () : bool =
  (coins 0) = 0
;; run_test "coins zero" test

let test () : bool =
  (coins (-1)) = 0
;; run_test "coins negative" test


(*************************************************************************)
(* Example (printing) *)

(* Printing is a great tool for debugging, since it lets you see the 
   output of your code on the console. Below, we will show you how to
   print in OCaml. *)

(* Recall that OCaml files are composed of top-level definitions,
   which begin with the 'let' keyword, and commands, which begin with
   two semicolons. One useful command instructs OCaml to print
   text. *)

(* The print_endline function causes its string argument to appear in the
   output window, much like System.out.println in Java. *)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of printing example"

(* Adding commands to print values can be very useful for debugging
   your assignment below. For example, consider the following
   buggy function. *)

let day_after (day: string) : string =
  begin match day with
  | "Monday"    -> "Tuesday"
  | "Tuesday"   -> "Wednesday"
  | "Wednesday" -> "Thursday"
  | "Thursday"  -> "Friday"
  | "Friday"    -> "Saturday"
  | "Saturday"  -> "Sunday"
  | "Sunday"    -> "Monday"
  | _           -> failwith "not a valid day"
  end

(* We can see that one of the test cases for this definition fails, so
   this definition definitely has a bug. But running the program just
   tells us that the answer is wrong, without showing the actual
   answer. *)

let test () : bool =
  (day_after "Tuesday") = "Wednesday"
;; run_test "day_after Tuesday" test

(* Adding a print command lets us see what the erroneous result
   is... *)

;; print_endline ("The day after Tuesday is " ^ (day_after "Tuesday") ^ ".")

(* (After running this example, you can fix the bug in the day_after
   function so that the test passes). *)

(* Note: If the result that you want to print is not a string, you
   need to convert it to be a string. OCaml includes some library
   functions to do this, such as string_of_int and string_of_bool.

   After you finish problem 1 above, uncomment the next command
   to demonstrate printing integer values. *)


;; print_endline ("Coins to make 99 cents is "
               ^ (string_of_int (coins 99)))


(* Feel free to add whatever printing commands you like to this
   homework assignment. The testing infrastructure will ignore all of
   the output that your code produces. *)

;; print_endline "End of printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~"


(*************************************************************************)
(* Problem 2 (geometry) *)

(* Sometimes street magicians need to use crates as tables in their
   acts.  Given the dimensions of a crate, find the largest surface
   area it can provide when used as a table.

   Hint: OCaml provides built-in max and min functions that take in two
   arguments and behave exactly as you might expect: `max 5 2` returns 5,
   for example. 

   Note: The behavior of this function when at least one of the input side 
   lengths is <= 0 is undefined. Your function may return any value in this
   case; we will not test this case on submission. *)

let rec maximum_table_area (side1: int) (side2: int) (side3: int) : int =
  (max (side1 * side2) (max (side2 * side3) (side3 * side1)))

let test () : bool =
  (maximum_table_area 1 2 3) = 6
;; run_test "maximum_table_area three different side lengths" test

let test () : bool =
  (maximum_table_area 4 3 3) = 12
;; run_test "maximum_table_area two sides the same length" test

let test () : bool =
  (maximum_table_area 2 2 2) = 4
;; run_test "maximum_table_area all sides the same length" test

let test () : bool =
  (maximum_table_area 6 2 2) = 12
;; run_test "maximum_table_area test_24" test


(*************************************************************************)
(* Problem 3 (simulating robot movement) *)

(* Help a robot move along its track (with spaces numbered 0 through
   99) by calculating its new position when given `dir` (equal to
   "forward" or "backward") and `num_moves` indicating a non-negative
   number of spaces.  Keep in mind that the robot can't move past the
   0 or 99 spot so when it reaches either end it stays there. *)

let rec move_robot (pos: int) (dir: string) (num_moves: int) : int =
	if (dir = "forward") then
		if ((pos + num_moves) > 99) then 99
		else (pos + num_moves)
	else if (dir = "backward") then
		if ((pos - num_moves) < 0) then 0
		else (pos - num_moves)
	else (failwith "move_robot input error")

let test () : bool =
  (move_robot 10 "forward" 3) = 13
;; run_test "move_robot forward in bounds" test

let test () : bool =
  (move_robot 1 "backward" 4 ) = 0
;; run_test "move_robot backward out of bounds" test

let test () : bool =
  (move_robot 20 "forward" 100) = 99
;; run_test "move_robot forward out of bounds" test

let test () : bool =
  (move_robot 20 "backward" 5) = 15
;; run_test "move_robot backward in bounds" test


(*************************************************************************)
(* Problem 4 (Philadelphia geography) *)
(* Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.

   Even streets go one way and odd streets go another:

     East of Broad (<14th): even go south, odd go north
     West of Broad (>14th): even go north, odd go south
     West Philly  (>=32nd): even go south, odd go north
     West Philly  (>=46th): two-way

   There are, however, a few exceptions.
     - 1st and 14th do not actually exist as street names -- they're
       called Front and Broad. We'll ignore this and pretend they do.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.

   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist. We only consider Front
       (=1st) through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.
     - you might find the infix 'mod' (modulo) function useful:
           (x mod 2)
       evaluates to 0 if x is even and 1 otherwise.
     - sometimes there's no 'simple' way of writing down complex case
       analysis...

   Welcome to Philadelphia! *)

let rec street_direction (st: int) : string =
  if ((st <= 0) || (st >= 70)) then "N/A"
	else if ((st = 14) || (st = 25) || (st = 38) || (st = 41) ||
  (st = 42)) then "NS"
	else if ((st = 24) || (st = 59)) then "S"
	else if (st = 58) then "N"
	else if (st < 14) then
		if ((st mod 2) = 0) then "S"
		else "N"
	else if ((st > 14) && (st < 32)) then
		if ((st mod 2) = 0) then "N"
		else "S"
	else if ((st >=32) && (st < 46)) then
		if ((st mod 2) = 0) then "S"
		else "N"
	else "NS"

let test () : bool =
  (street_direction 14) = "NS"
;; run_test "street_direction Broad is two-way" test

let test () : bool =
  (street_direction 9) = "N"
;; run_test "street_direction 9th goes north" test

let test () : bool =
  (street_direction 18) = "N"
;; run_test "street_direction 18th goes north" test

let test () : bool =
  (street_direction 80) = "N/A"
;; run_test "street_direction out of bounds" test

let test () : bool =
  (street_direction 50) = "NS"
;; run_test "street_direction West Philly" test


(*************************************************************************)
(* The remaining exercises provide practice with lists and recursion.
   It is best to wait until after that topic is covered in lecture
   before continuing. *)

(*************************************************************************)
(* Problem 5 (exists) *)

(* Write a function that determines whether at least one boolean value
   in its input list is true. *)

let rec exists (bools: bool list) : bool =
  begin match bools with
	| [] -> false
	| element::tail -> ((element = true) || (exists tail)) 
	end

let test () : bool =
  (exists [false; false]) = false
;; run_test "exists all false" test

let test () : bool =
  (exists [true; false; true]) = true
;; run_test "exists multiple true" test

let test () : bool =
  (exists []) = false
;; run_test "exists empty list" test

let test () : bool =
  (exists [false; true; false; false]) = true
;; run_test "exists false then true" test


(*************************************************************************)
(* Problem 6 (join) *)

(* Write a function that takes a list of strings and "flattens" it
   into a single string. This function also takes an additional
   argument, a separator string, which is interspersed between all of
   the strings in the list. *)

(* Hint: the ^ operator concatenates two strings together. For example,
   "a" ^ "bc" evaluates to "abc". *)

let rec join (separator: string) (l: string list) : string =
  begin match l with
	| [] -> ""
	| element::tail ->
    if (tail = []) then element
    else element ^ separator ^ (join (separator: string) (tail: string list))
	end

let test () : bool =
  (join "," ["a"; "b"; "c"]) = "a,b,c"
;; run_test "test_join separator" test

let test () : bool =
  (join "" ["a"; "b"; "c"]) =  "abc"
;; run_test "test_join no separator" test

let test () : bool =
  (join ";" []) = ""
;; run_test "test_join empty list" test

let test () : bool =
  (join "" []) = ""
;; run_test "test_join no separator and empty list" test


(*************************************************************************)
(* Example (printing lists) *)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of list printing example"

(* Once you have implemented the join function above, you can use it
   to print out lists of strings. This can be very useful for
   debugging the remaining tasks in this assignment, as you can print
   out the output of your functions to help you understand why your
   test cases are failing *)


;; print_endline (join "," ["a"; "b"; "c"])


(* If you would like to print a list of ints, you'll need to rewrite
   the join function. We advise that you go ahead and do this so that
   you can use this function to help debug the last few tasks in this
   homework. *)

let rec int_join (separator: string) (l: int list) : string =
  begin match l with
	| [] -> ""
	| element::tail ->
		if (tail = []) then string_of_int element
    else (string_of_int element) ^ separator ^
      (int_join (separator: string) (tail: int list))
	end

;; print_endline ("[" ^ (int_join ";" [1; 2; 3]) ^ "]")

;; print_endline "End of list printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"

(*************************************************************************)
(* Problem 7 (append) *)

(* Write a function that takes lists l1 and l2 and returns a list
   containing all the items in l1 followed by all the items in l2.

   NOTE: OCaml already provides this function. In future homeworks you
   can use built in operator '@' that appends l1 and l2 as in l1 @
   l2. Do *not* use the @ operator in your solution to this
   problem. *)

let rec append (l1: string list) (l2: string list) : string list =
  begin match l1 with
	| [] -> l2
	| element::tail -> (element::(append (tail: string list) (l2: string list)))
	end

let test () : bool =
  (append [] []) = []
;; run_test "append two empty lists" test

let test () : bool =
  (append ["1"; "2"] ["3"]) = ["1"; "2"; "3"]
;; run_test "append different lengths" test

let test () : bool =
  (append [] ["3"]) = ["3"]
;; run_test "append first list empty" test

let test () : bool =
  (append ["1"; "2"] []) = ["1"; "2"]
;; run_test "append second list empty" test


(*************************************************************************)
(* Problem 8 (finding names in a list) *)

(* Write a function that checks whether a list of names contains some
   particular name. *)

let rec contains_str (l: string list) (name: string) : bool =
  begin match l with
	| [] -> false
	| element::tail -> ((element = name) || (contains_str tail name))
	end

let test () : bool =
  (contains_str ["Garnet"; "Amethyst"; "Pearl"] "Amethyst") = true
;; run_test "contains_str name in list once" test

let test () : bool =
  (contains_str ["Garnet"; "Amethyst"; "Pearl"] "Steven") = false
;; run_test "contains_str name not in list" test

let test () : bool =
  (contains_str ["Joe"; "John"; "Joe"] "Joe") = true
;; run_test "contains_str name in list twice" test

let test () : bool =
  (contains_str [] "Joe") = false
;; run_test "contains_str empty list" test


(* Next, write a function that, given two lists of names filters the
   first list so that only those also in the second remain. Your
   function should return a list containing all the elements that
   appear in both lists, in the order that they appear in the first
   list. *)

let rec in_both (names1: string list) (names2: string list) : string list =
  begin match names1 with
	| [] -> []
  | element1::tail1 -> if (contains_str names2 element1) then
                         element1::
                         (in_both (tail1: string list) (names2: string list))
                       else
                         (in_both (tail1: string list) (names2: string list))
  end

let test () : bool =
  (in_both ["Garnet"; "Amethyst"; "Pearl"] ["Pearl"; "Steven"]) = ["Pearl"]
;; run_test "in_both Pearl in both lists" test

let test () : bool =
  (in_both [] ["Pearl"; "Steven"]) = []
;; run_test "in_both empty name list" test

let test () : bool =
  (in_both ["Garnet"; "Amethyst"; "Pearl"] []) = []
;; run_test "in_both empty test list" test

let test () : bool =
  (in_both ["a"; "b"; "c"] ["c"; "b"]) = ["b"; "c"]
;; run_test "in_both test_94" test


(*************************************************************************)
(* Problem 9 (merging lists) *)

(* Write a function that merges two input lists into a single list
   that contains all the elements from both input lists in alternating order:
   the first, third, etc. elements come from the first input list and
   the second, fourth, etc. elements come from the second input list.

   The lengths of the two lists may not be the same -- any
   extra elements should appear at the very end of the result. *)

let rec merge (l1: int list) (l2: int list) : int list =
  begin match l1 with
  | [] -> l2
  | element1::tail1 -> element1::(merge l2 tail1)
  end

let test () : bool =
  (merge [1; 3; 5; 7] [2; 4; 6; 8]) = [1; 2; 3; 4; 5; 6; 7; 8]
;; run_test "merge lists same size" test

let test () : bool =
  (merge [] [1; 2; 3]) = [1; 2; 3]
;; run_test "merge one empty list" test

let test () : bool =
  (merge [1; 2; 3; 4; 5; 6] [1; 2; 3]) = [1; 1; 2; 2; 3; 3; 4; 5; 6]
;; run_test "merge first list longer" test

let test () : bool =
  (merge [1; 2; 3] [1; 2; 3; 4; 5; 6]) = [1; 1; 2; 2; 3; 3; 4; 5; 6]
;; run_test "merge second list longer" test


(*************************************************************************)
(* Problem 10 (is_sorted) *)

(* Write a function that determines whether a given list of integers
   is SORTED -- that is, whether the elements appear in ascending
   order. It is okay if the list has repeated elements, so long as they
   are next to each other. 

   Lists containing zero or one elements are always sorted. *)
let rec is_sorted (l: int list) : bool =
  begin match l with
  | [] -> true
  | [x] -> true
  | element1::element2::tail -> if (element2 >= element1)
                                  then is_sorted (element2::tail)
                                else false
  end

let test () : bool =
  (is_sorted [1; 2; 3]) = true
;; run_test "is_sorted sorted list" test

let test () : bool =
  (is_sorted [3; 2; 1]) = false
;; run_test "is_sorted unsorted list" test

let test () : bool =
  (is_sorted [1]) = true
;; run_test "is_sorted singleton" test

let test () : bool =
  (is_sorted []) = true
;; run_test "is_sorted nil" test


(*************************************************************************)
(* Problem 11 (merge_sorted) *)

(* Write a function that takes two sorted lists (in ascending order)
   and yields a merged list that is also sorted and contains all the
   elements from the two input lists. *)

let rec int_append (l: int list) (value: int) : int list =
  begin match l with
	| [] -> [value]
	| element::tail -> (element::(int_append (tail: int list) (value: int)))
	end

let rec integrate (value: int) (l2: int list) : int list = 
  begin match l2 with
  | [] -> int_append (l2: int list) (value: int)
  | element::tail -> if is_sorted (value::l2) then value::l2
                     else if is_sorted (element::value::tail)
                       then element::value::tail
                     else element::(integrate (value: int) (tail: int list))
  end

let rec merge_sorted (l1: int list) (l2: int list) : int list =
  begin match l1 with
  | [] -> l2
  | element1::tail1 -> merge_sorted (tail1: int list)
                         (integrate (element1: int) (l2: int list) : int list)
  end

let test () : bool =
  (merge_sorted [2; 7] [3; 5; 11]) = [2; 3; 5; 7; 11]
;; run_test "merge_sorted lists different size" test

let test () : bool =
  (merge_sorted [1; 2; 3] [4; 5; 6]) = [1; 2; 3; 4; 5; 6]
;; run_test "merge_sorted lists same size" test

let test () : bool =
  (merge_sorted [] [4; 5; 6]) = [4; 5; 6]
;; run_test "merge_sorted first list empty" test

let test () : bool =
  (merge_sorted [1; 2; 3] []) = [1; 2; 3]
;; run_test "merge_sorted second list empty" test


(*************************************************************************)
(* Problem 12 (sublist) *)

(* Write a function that takes two integer lists (not necessarily
   sorted) and returns true precisely when the first list is a sublist
   of the second.

   The first list may appear anywhere within the second, but its elements
   must appear contiguously.

   HINT: You should define and test a helper function that you can use
   in sublist.
*)

let rec locus_check (l1: int list) (l2: int list) : bool =
  begin match l2 with
  | [] -> if (l1 = []) then true else false
  | element2::tail2 -> begin match l1 with
                       | [] -> false
                       | element1::tail1 ->
                         if (element1 = element2)
                           then locus_check (tail1: int list) (tail2: int list)
                         else false
                       end
  end

let rec sublist (l1: int list) (l2: int list) : bool =
  if (l1 = []) then true
  else if (l2 = []) then false
  else if (locus_check (l1: int list) (l2: int list) = true) then true
  else begin match l2 with
       | [] -> false
       | element2::tail2 ->
           if (locus_check (l1: int list) (tail2: int list) = false)
             then sublist (l1: int list) (tail2: int list)
           else true
       end

let test () : bool =
  (sublist [] [])
;; run_test "sublist: [] []" test

let test () : bool =
  (sublist [] [1;2;3])
;; run_test "sublist: [] [1;2;3]" test

let test () : bool =
  (sublist [2;3] [1;2;3])
;; run_test "sublist: [2;3] [1;2;3]" test

let test () : bool =
  not (sublist [2;3] [2;1;3])
;; run_test "sublist: not [2;3] [2;1;3]" test


   (*************************************************************************)
(* Problem 13 (permutations) *)

(* This one is a challenge problem, so we've made it worth 0 points --
   kudos only. *)

(* A PERMUTATION of a list l is a list that has the same elements as l
   but is not necessarily in the same order.

   Write a function that, given a list l, calculates ALL of the
   permutations of l (and returns them as a list). For example,

       permutations [1;2;3]

   might yield

       [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]].

   (We say "might yield" here because we haven't specified the
   order of the permutations in the list returned by your function.
   For example, the result

       [[1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]; [1;2;3]]

   would also be correct.)

   Hint: Begin by writing a unit test or two, to make sure you understand the
   problem (even though you may need to rewrite them if your answer comes out
   in a different order, the exercise is useful). Also, you'll probably want
   to break the problem down into one or more sub-problems, each of which can
   be solved by recursion. *)

(* Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if 'permutations' is
   missing. *)

let rec permutations (l: int list) : int list list =
  failwith "permutations: unimplemented"

(* Note that you will also have to think about how to TEST
   permutations, as there may be several correct solutions for each
   input. *)

(* The last part of this file is a print statement. When you see this line
   as the result, you will know that all of the tests in this file have
   succeeded. *)
;; print_endline "intro.ml: ran to completion"
