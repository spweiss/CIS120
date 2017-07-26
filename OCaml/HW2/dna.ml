(* -------------------------------------------------------------------------- *)
(* ----- Computing Human Evolution ------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

;; open Assert


(* Biologists often try to estimate the "relatedness" of different species by
   comparing their genetic codes. As techniques to gather genetic information
   have become more sophisticated, it has become common to use computer
   programs to aid in analyzing genetic information. Your task in this homework
   is to develop functions that help in understanding genetic data. In
   particular, one task that you will perform is to use this data to determine
   the genetic similarity of different species. You will be doing this by
   analyzing sample DNA for several ape species, which is provided below.

   To achieve this goal, you will be guided through writing several OCaml
   functions that can be combined for processing DNA strands and generating
   candidate genetic trees to be used for analysis.

   DO NOT FEAR THE BIOLOGY! While this assignment uses real biological data, we
   will walk you through the necessary terminology.

   Happy Coding! :) *)

(* Unlike homework 1, we give you a number of INCOMPLETE tests in this file.
   The first provided test for each problem is ready to go, but for some of the
   test cases you will need to provide the right expected value, which you
   should calculate by hand. Before implementing each function, complete its
   corresponding test section at the bottom of the document. We use the OCaml 
   primitive "failwith" to cause an error so you know you have to fill in 
   something. *)

(* NOTE: The test cases for all functions in this assignment are found
   at the bottom of this file. *)

(* You can view supplementary links and images at

     http://www.seas.upenn.edu/~cis120/current/hw/hw02/index.shtml

   Biologists use trees (Figure A on the above web page) to show how species
   evolved (or might have evolved) from ancestor species. To practice
   and recursion over lists and trees, in this assignment you will
   write a program that automatically generates hypothesis trees like the one
   in Figure A.

   Your program inputs will be real samples of DNA, the genetic code that
   describes how to build organisms (Figure B). Each double helix of DNA
   consists of two complementary strands, each one a sequence of the
   nucleotides adenine, thymine, guanine, and cytosine. Adenine always appears
   opposite thymine, and guanine always appears opposite cytosine. *)

type nucleotide =
| G (* guanine *)
| C (* cytosine *)
| A (* adenine *)
| T (* thymine *)

type helix = nucleotide list

(* For this assignment, we represent individual nucleotides using an
   enumeration and represent DNA sequences as a nucleotide list. The list
   corresponds to just one helix in the double helix. However, since
   nucleotides always appear in a complementary fashion, we (i.e. you :)) can
   easily calculate the other helix... *)


(* -------------------------------------------------------------------------- *)
(* ----- Problem 1: Compute the complementary helix ------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Given a single helix, compute the other half of the double helix. Remember 
   to start each problem by first finishing the corresponding test section at 
   the bottom of the file. *)
let rec complementary_helix (x: helix) : helix =
  begin match x with
  | [] -> []
  | base :: tail -> if base = A then T :: complementary_helix tail
                    else if base = T then A :: complementary_helix tail
                    else if base = C then G :: complementary_helix tail
                    else C :: complementary_helix tail
  end

(* -------------------------------------------------------------------------- *)
(* ----- Ape DNA Sequences -------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* The lines below define DNA sequences taken from the genetic codes for 8 ape
   species. (Of course, the full genetic codes are longer.) This is real data
   extracted from the Entrez Nucleotide database
   (http://www.ncbi.nlm.nih.gov/nuccore).

   We violate the usual 80-character style guideline so you don't have to
   scroll forever to get past these definitions :-) *)

let lar_gibbon: helix = [T; A; T; A; A; A; G; A; G; A; G; T; A; A; A; A; A; G; T; G; T; A; A; A; C; C; C; C; A; T; A; G; T; T; G; G; C; C; T; A; A; A; A; G; C; A; G; C; C; A; C; C; A; A; T; T; A; A; G; A; A; A; G; C; G; T; T; C; A; A; G; C; T; C; A; A; C; A; C; C; A; C; C; T; A; T; C; C; A; A; C; A; A; A; T; C; C; C; A; A; A; C; A; C; A; C; A; A; C; T; G; A; A; C; T; C; C; T; T; C; C; A; C; C; A; C; A; T; T; G; G; A; C; C; A; A; T; C; T; A; T; C; A; T; T; T; T; A; T; A; G; A; A; G; A; A; A; T; A; A; T; G; T; T; A; G; T; A; T; A; A; G; T; A; A; C; A; T; G; A; A; T; A; A; C; A; T; T; C; T; C; C; C; C; C; G; C; A; T; A; A; A; C; C; T; A; T; A; T; C; A; G; A; C; C; A; A; A; A; A; A; C; T; T; C; G; C; T; G; A; C; A; G; T; T; A; A; C; A; G; C; C; C; A; A; T; A; T; C; T; A; A; A; A; C; C; A; A; C; T; G; A; T; A; A; A; C; C; A; T; T; A; T; T; G; C; C; C; A; C; A; C; T; G; T; C; A; A; C; C; C; A; A; C; A; T; A; G; G; C; A; T; G; C; C; C; A; C; A; A; G; G; A; A; A; G; G; T; T; A; A; A; A; A; A; A; G; T; A; A; A; A; G; G; A; A; C; T; C; G; G; C; A; A; A; C; A; C; T; A; C; C; C; C; G; C; C; T; G; T; T; T; A; C; C; A; A; A; A; A; C; A; T; C; A; C; C; T; C; T; A; G; C; A; T; T; A; C; C; A; G; T; A; T; T; A; G; A; G; G; C; A; C; C; G; C; C; T; G; C; C; C; A; G; T; G; A; C; A; C; A; T; G; T; T; C; A; A; C; G; G; C; C; G; C; G; G; T; A; C; C; C; T; A; A; C; C; G; T; G; C; A; A; A; G; G; T; A; G; C; A; T; A; A; T; C; A; C; T; T; G; T; T; C; C; T; T; A; A; A; T; G; G; G; G; A; C; T; T; G; T; A; T; G; A; A; T; G; G; C; T; C; C; A; C; G; A; G; G; G; T; T; C; A; G; C; T; G; T; C; T; C; T; T; A; C; T; T; T; C; A; A; C; C; A; G; T; G; A; A; A; T; T; G; A; C; C; T; G; T; C; C; G; T; G; A; A; G; A; G; G; C; G; G; A; C; A; T; A; A; C; C; T; A; A; C; A; A; G; A; C; G; A; G; A; A; G; A; C; C; C; T; A; T; G; G; A; G; C; T; T; T; A; G; T; C; T; A; T; C; A; A; T; G; C; A; A; A; C; A; A; C; A; T; T; C; A; A; T; A; A; A; C; C; A; A; C; A; G; G; T; C; A; T; A; A; A; T; T; A; C; C; A; A; A; C; C; T; G; C; A; T; C; G; A; A; G; A; C; T; T; C; G; G; T; T; G; G; G; G; C; G; A; C; C; T; C; G; G; A; G; C; A; T; A; G; A; C; T; A; A; C; C; T; C; C; G; A; G; C; A; G; T; A; T; A; T; G; C; T; A; A; G; A; C; C; A; C; A; C; C; A; G; T; C; A; A; A; A; C; G; A; A; A; C; T; C; C; A; T; G; T; G; C; A; A; T; T; G; A; C; C; C; A; A; T; A; A; C; T; T; G; A; T; C; A; A; C; G; G; A; A; C; A; A; G; T; T; A; C; C; C; T; A; G; G; G; A; T; A; A; C; A; G; C; G; C; A; A; T; C; C; T; A; T; T; C; T; A; G; A; G; T; C; C; A; T; A; T; C; A; A; C; A; A; T; A; G; G; G; T; T; T; A; C; G; A; C; C; T; C; G; A; T; G; T; T; G; G; A; T; C; A; G; G; A; C; A; T; C; C; C; G; A; T; G; G; T; G; C; A; G; C; C; G; C; T; A; T; C; A; A; A; G; G; T; T; C; G; T; T; T; G; T; T; C; A; A; C; G; A; T; T; A; A; A; G; T; C; C; T; A; C; G; T; G; A; T; C; T; G; A; G; T; T; C; A; G; A; C; C; G; G; A; G; T; A; A; T; C; C; A; G; G; T; C; G; G; T; T; T; C; T; A; T; C; T; G; T; T; C; T; A; T; A; T; T; T; C; T; C; C; C; T; G; T; A; C; G; A; A; A; G; G; A; C; A; A; G; A; G; A; A; A; T; A; G; G; G; C; C; C; A; C; T; T; C; G; C; A]

let pileated_gibbon: helix = [T; A; T; A; A; A; G; A; G; A; G; T; A; A; A; A; A; G; T; G; T; A; A; A; C; C; C; C; A; T; A; G; T; T; G; G; C; C; T; A; A; A; A; G; C; A; G; C; C; A; C; C; A; A; T; T; A; A; G; A; A; A; G; C; G; T; T; C; A; A; G; C; T; C; A; A; C; A; C; C; A; C; C; C; A; C; C; C; A; A; T; A; A; A; T; C; C; C; A; A; A; C; A; T; A; T; A; A; C; T; G; A; A; C; T; C; C; T; T; C; C; A; C; C; A; C; A; T; T; G; G; A; C; C; A; A; T; C; T; A; T; C; A; T; T; C; T; A; T; A; G; A; A; G; A; A; A; T; A; A; T; G; T; T; A; A; T; A; T; G; A; G; T; A; A; C; A; C; G; A; A; A; A; G; A; A; T; T; C; T; C; C; T; C; C; G; C; A; T; A; A; G; C; C; T; A; T; A; T; C; A; G; A; C; C; A; A; A; A; A; G; A; C; T; T; C; G; C; T; G; A; C; A; G; T; T; A; A; C; A; G; C; T; C; A; A; T; A; T; C; T; A; A; A; A; C; C; A; A; C; T; G; A; T; A; G; A; C; C; A; T; T; A; T; T; A; C; C; C; A; C; A; C; T; G; T; C; A; A; C; C; C; A; A; C; A; T; A; G; G; C; A; T; G; C; C; C; A; C; A; A; G; G; A; A; A; G; G; T; T; A; A; A; A; A; A; A; G; T; A; A; A; A; G; G; A; A; C; T; C; G; G; C; A; A; A; C; A; C; T; A; C; C; C; C; G; C; C; T; G; T; T; T; A; C; C; A; A; A; A; A; C; A; T; C; A; C; C; T; C; T; A; G; C; A; T; T; A; C; C; A; G; T; A; T; T; A; G; A; G; G; C; A; C; C; G; C; C; T; G; C; C; C; A; G; T; G; A; C; A; C; A; T; G; T; T; C; A; A; C; G; G; C; C; G; C; G; G; T; A; C; C; C; T; A; A; C; C; G; T; G; C; A; A; A; G; G; T; A; G; C; A; T; A; A; T; C; A; C; T; T; G; T; T; C; C; T; T; A; A; A; T; G; G; G; G; A; C; T; T; G; T; A; T; G; A; A; T; G; G; C; T; C; C; A; C; G; A; G; G; G; T; C; C; A; G; C; T; G; T; C; T; C; T; T; A; C; T; T; T; C; A; A; C; C; A; G; T; G; A; A; A; T; T; G; A; C; T; T; G; T; C; C; G; T; G; A; A; G; A; G; G; C; G; G; A; C; A; T; A; G; C; C; T; A; A; C; A; A; G; A; C; G; A; G; A; A; G; A; C; C; C; T; A; T; G; G; A; G; C; T; T; T; A; G; C; C; T; A; T; C; A; A; T; G; C; A; A; A; C; A; A; T; A; T; T; C; A; A; C; A; A; A; C; C; A; A; C; A; G; G; C; C; G; T; A; A; A; C; T; A; C; C; A; A; A; T; C; T; G; C; A; T; C; G; A; A; G; A; C; T; T; C; G; G; T; T; G; G; G; G; C; G; A; C; C; T; C; G; G; A; G; C; A; T; A; A; A; C; T; A; A; C; C; T; C; C; G; A; G; C; A; G; T; A; C; A; T; G; C; T; A; A; G; A; C; C; A; C; A; C; C; A; G; T; C; A; A; A; A; C; A; A; A; A; T; T; C; C; A; T; A; T; G; C; A; A; T; T; G; A; C; C; C; A; A; T; A; A; T; T; T; G; A; T; C; A; A; C; G; G; A; A; C; A; A; G; T; T; A; C; C; C; T; A; G; G; G; A; T; A; A; C; A; G; C; G; C; A; A; T; C; C; T; A; T; T; C; T; A; G; A; G; T; C; C; A; T; A; T; C; A; A; C; A; A; T; A; G; G; G; T; T; T; A; C; G; A; C; C; T; C; G; A; T; G; T; T; G; G; A; T; C; A; G; G; A; C; A; T; C; C; C; G; A; T; G; G; T; G; C; A; G; C; C; G; C; T; A; T; C; A; A; A; G; G; T; T; C; G; T; T; T; G; T; T; C; A; A; C; G; A; T; T; A; A; A; G; T; C; C; T; A; C; G; T; G; A; T; C; T; G; A; G; T; T; C; A; G; A; C; C; G; G; A; G; T; A; A; T; C; C; A; G; G; T; C; G; G; T; T; T; C; T; A; T; C; T; G; T; T; C; T; A; T; A; T; T; T; C; T; C; C; C; T; G; T; A; C; G; A; A; A; G; G; A; C; A; A; G; A; G; A; A; A; T; G; G; G; G; C; C; C; A; C; T; T; C; A; C]

let siamang: helix = [T; A; T; A; A; A; A; C; C; T; A; G; A; C; A; A; A; A; A; A; G; G; A; A; G; G; A; A; T; C; G; A; A; C; C; C; T; C; T; A; A; A; A; C; C; G; G; T; T; T; C; A; A; G; C; C; A; G; C; C; C; C; A; T; A; A; C; C; T; T; T; A; T; G; A; C; T; T; T; T; T; C; A; A; A; A; A; G; A; T; A; T; T; A; G; A; A; A; A; A; C; T; A; T; T; T; C; A; T; A; A; C; T; T; T; G; T; C; A; A; A; G; T; T; A; A; A; T; C; A; C; A; G; G; T; T; C; A; A; A; C; C; C; C; G; T; A; T; A; T; C; T; T; A; A; T; G; G; C; A; C; A; T; G; C; A; G; C; T; C; A; A; G; T; A; G; G; C; C; T; A; C; A; A; G; A; C; G; C; T; A; C; A; T; C; C; C; C; T; A; T; C; A; T; A; G; A; A; G; A; A; C; T; A; A; T; C; T; C; T; T; T; C; C; A; C; G; A; C; C; A; T; G; C; C; C; T; C; A; T; A; A; T; C; A; T; T; T; T; C; C; T; T; A; T; C; A; G; C; T; T; C; C; T; A; G; T; T; C; T; A; T; A; T; G; C; C; C; T; C; T; T; C; C; T; A; A; C; A; C; T; C; A; C; A; A; C; A; A; A; A; C; T; A; A; C; C; A; A; C; A; C; T; A; A; T; A; T; T; A; C; G; G; A; C; G; C; C; C; A; A; G; A; G; A; T; A; G; A; A; A; C; C; G; T; C; T; G; A; A; C; A; A; T; C; C; T; A; C; C; T; G; C; T; A; T; T; A; T; T; C; T; A; G; T; T; C; T; A; A; T; C; G; C; C; C; T; C; C; C; A; T; C; C; C; T; C; C; G; C; A; T; C; C; T; T; T; A; T; T; T; G; A; C; A; G; A; C; G; A; A; A; T; C; A; A; C; G; A; C; C; C; C; T; C; C; T; T; T; A; C; C; A; T; C; A; A; G; G; C; A; A; T; C; G; G; T; C; A; T; C; A; G; T; G; A; T; A; C; T; G; A; G; C; C; T; A; C; G; A; A; T; A; T; A; C; A; G; A; C; T; A; C; G; G; T; G; G; G; C; T; A; A; T; C; T; T; C; A; A; T; T; C; T; T; A; C; A; T; A; T; T; A; C; C; A; C; C; A; T; T; A; T; T; T; C; T; A; G; A; A; C; C; A; G; G; G; G; A; C; C; T; T; C; G; A; C; T; C; C; T; T; G; A; A; G; T; C; G; A; C; A; A; C; C; G; A; G; T; A; G; T; T; C; T; T; C; C; A; A; T; T; G; A; A; G; C; C; C; C; T; G; T; C; C; G; T; A; T; A; A; T; A; A; T; T; A; C; A; T; C; A; C; A; A; G; A; C; G; T; C; C; T; A; C; A; C; T; C; A; T; G; A; A; C; T; G; T; C; C; C; C; T; C; C; C; T; A; G; G; T; C; T; G; A; A; A; A; C; G; G; A; T; G; C; T; A; T; C; C; C; C; G; G; A; C; G; C; C; T; A; A; A; C; C; A; A; A; C; C; A; C; A; T; T; C; A; C; C; G; C; C; A; C; A; C; G; C; C; C; A; G; G; A; G; T; A; T; A; C; T; A; C; G; G; C; C; A; A; T; G; C; T; C; A; G; A; A; A; T; C; T; G; T; G; G; A; G; C; C; A; A; C; C; A; T; A; G; C; T; T; T; A; T; A; C; C; G; A; T; T; G; T; T; C; T; A; G; A; A; C; T; A; A; T; T; C; C; C; T; T; A; A; A; A; A; T; C; T; T; C; G; A; A; A; T; A; G; G; G; C; C; T; G; T; G; T; T; T; A; C; C; C; T; A; T; A; A; C; C; C; C; A; C; C; C; T; C; T; G; C; C; C; C; C; C; G; T; A; A; A; T; C; T; C; A; C; T; G; T; A; G; A; G; C; T; A; G; A; C; C; A; G; C; A; T; T; A; A; C; C; T; T; T; T; A; A; G; T; T; A; A; A; G; A; C; T; A; A; G; A; G; A; A; C; T; A; C; C; A; C; C; T; C; T; T; T; A; C; A; G; T; G; A; A; A; T; G; C; C; C; C; A; A; T; T; A; A; A; C; A; C; C; A; C; C; G; T; G; T; G; A; C; C; T; A; T; A; A; T; C; A; T; C; A; C; A; T; C; A; A; T; A; C; T; T; C; T; C; A; C; A; C; T; A; T; T; C; C; T; C; C; T; C; A; T; A; C; A; A; C; T; A; A; A; A; A; C; A; C]

let white_cheeked_gibbon: helix = [T; A; T; A; A; A; A; C; C; T; A; G; A; C; A; A; A; A; A; A; G; G; A; A; G; G; A; A; T; C; G; A; A; C; C; C; C; C; T; A; A; A; A; C; T; G; G; T; T; T; C; A; A; G; C; C; A; G; C; C; C; C; A; T; A; A; C; C; T; C; T; A; T; G; A; C; T; T; T; T; T; C; A; A; A; A; G; G; T; A; T; T; A; G; A; A; A; A; G; C; T; A; T; T; T; C; A; T; A; A; C; T; T; T; G; T; C; A; A; A; G; T; T; A; A; A; T; C; A; C; A; G; G; T; T; C; A; A; G; C; C; C; C; G; T; A; T; A; T; C; T; T; A; A; T; G; G; C; A; C; A; T; G; C; A; A; C; T; C; A; A; G; T; A; G; G; C; C; T; A; C; A; A; G; A; C; G; C; T; A; C; A; T; C; C; C; C; C; A; T; C; A; T; A; G; A; A; G; A; A; C; T; A; A; T; C; T; C; T; T; T; C; C; A; C; G; A; C; C; A; C; G; C; C; C; T; C; A; T; A; A; T; C; A; T; T; T; T; C; C; T; T; A; T; C; A; G; C; T; T; C; C; T; G; G; T; C; C; T; A; T; A; T; G; C; C; C; T; C; T; T; C; C; T; A; A; C; A; C; T; C; A; C; A; A; C; A; A; A; A; C; T; A; A; C; C; A; A; C; A; C; T; A; A; C; A; T; T; A; C; A; G; A; C; G; C; C; C; A; A; G; A; G; A; T; A; G; A; A; A; C; C; A; T; C; T; G; A; A; C; A; A; T; C; C; T; A; C; C; T; G; C; C; A; T; T; A; T; C; C; T; A; G; T; C; C; T; A; A; T; C; G; C; T; C; T; C; C; C; A; T; C; C; C; T; C; C; G; T; A; T; T; C; T; C; T; A; T; T; T; A; A; C; A; G; A; C; G; A; A; A; T; C; A; A; C; G; A; C; C; C; T; T; C; C; C; T; C; A; C; C; A; T; C; A; A; A; G; C; A; A; T; C; G; G; C; C; A; T; C; A; A; T; G; A; T; A; C; T; G; A; G; C; C; T; A; C; G; A; A; T; A; T; A; C; A; G; A; C; T; A; C; G; G; C; G; G; A; C; T; G; G; T; C; T; T; C; A; A; T; T; C; C; T; A; C; A; T; G; C; T; C; C; C; A; C; C; A; T; T; A; T; T; T; C; T; A; G; A; A; C; C; C; G; G; A; G; A; C; C; T; T; C; G; A; C; T; C; C; T; T; G; A; A; G; T; C; G; A; C; A; A; T; C; G; A; G; T; A; G; T; T; C; T; C; C; C; A; A; T; T; G; A; A; G; C; C; C; C; T; G; T; T; C; G; T; A; T; A; A; T; A; A; T; T; A; C; A; T; C; A; C; A; A; G; A; C; G; T; C; C; T; A; C; A; C; T; C; A; T; G; A; A; C; T; G; T; C; C; C; C; T; C; C; C; T; C; G; G; C; C; T; A; A; A; A; A; C; A; G; A; C; G; C; C; A; T; C; C; C; T; G; G; A; C; G; C; C; T; A; A; A; C; C; A; A; A; C; C; A; C; A; T; T; T; A; C; C; G; C; C; A; C; A; C; G; C; C; C; A; G; G; A; G; T; A; T; A; T; T; A; C; G; G; C; C; A; A; T; G; C; T; C; A; G; A; A; A; T; C; T; G; C; G; G; A; G; C; C; A; A; C; C; A; T; A; G; C; T; T; T; A; T; A; C; C; A; A; T; C; G; T; C; C; T; A; G; A; G; C; T; A; A; T; T; C; C; C; T; T; A; A; A; A; A; T; C; T; T; C; G; A; A; A; T; A; G; G; G; C; C; C; G; T; A; T; T; C; A; C; C; C; T; A; T; A; A; T; T; A; G; C; C; C; C; T; C; C; C; C; A; C; C; C; T; A; C; G; C; A; A; A; T; T; T; C; A; C; T; G; T; A; G; A; G; C; T; A; G; A; C; T; A; G; C; A; T; T; A; A; C; C; T; T; T; T; A; A; G; T; T; A; A; A; G; A; C; T; A; A; G; A; G; A; A; G; C; A; T; T; A; C; C; T; C; T; T; T; A; C; A; G; T; G; A; A; A; T; G; C; C; C; C; A; A; T; T; A; A; A; C; A; C; C; A; C; C; G; T; A; T; G; A; C; C; C; A; C; A; A; T; T; A; T; C; A; T; G; T; C; A; A; T; A; C; T; T; C; T; C; G; C; G; C; T; A; T; T; C; C; T; C; C; T; A; A; T; C; C; A; A; C; T; A; A; A; A; A; C; A]

let orangutan: helix = [T; A; T; A; A; A; A; C; C; C; C; C; T; G; C; A; A; T; A; T; C; C; C; A; A; T; A; C; C; A; A; A; C; C; C; C; C; C; T; C; T; T; C; G; T; C; T; G; A; T; C; A; G; T; C; T; T; G; A; T; C; A; C; A; G; C; A; G; T; C; C; T; A; C; T; T; C; T; C; C; T; T; T; C; C; C; T; C; C; C; A; G; T; C; C; T; A; G; C; C; G; C; T; G; G; C; A; T; C; A; C; C; A; T; A; C; T; A; C; T; A; A; C; A; G; A; T; C; G; C; A; A; C; C; T; A; A; A; C; A; C; C; A; C; A; T; T; C; T; T; T; G; A; C; C; C; A; G; C; C; G; G; A; G; G; T; G; G; A; G; A; T; C; C; C; A; T; C; C; T; A; T; A; T; C; A; G; C; A; C; C; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; C; C; A; C; C; C; T; G; A; A; G; T; C; T; A; C; A; T; T; C; T; C; A; T; C; C; T; G; C; C; G; G; G; T; T; T; C; G; G; C; A; T; A; A; T; C; T; C; C; C; A; C; A; T; C; G; T; A; A; C; A; C; A; C; T; A; T; T; C; C; G; G; A; A; A; A; G; A; A; G; A; G; C; C; A; T; T; T; G; G; G; T; A; C; A; T; A; G; G; C; A; T; A; G; T; C; T; G; A; G; C; C; A; T; A; G; T; C; T; C; A; A; T; T; G; G; C; T; T; C; C; T; G; G; G; C; T; T; T; A; T; C; G; T; A; T; G; G; G; C; C; C; A; C; C; A; C; A; T; A; T; T; C; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; G; G; A; C; A; C; A; C; G; A; G; C; C; T; A; C; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; T; G; C; C; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; T; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; C; C; A; C; G; G; A; A; G; C; A; A; C; A; C; T; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; A; T; C; C; T; C; T; G; A; G; C; C; T; T; A; G; G; A; T; T; C; A; T; T; T; T; C; C; T; C; T; T; C; A; C; C; G; T; A; G; G; C; G; G; C; C; T; A; A; C; A; G; G; C; A; T; C; G; T; A; C; T; A; G; C; A; A; A; C; T; C; A; T; C; A; C; T; A; G; A; C; A; T; T; G; T; A; T; T; A; C; A; C; G; A; T; A; C; A; T; A; C; T; A; C; G; T; T; G; T; A; G; C; C; C; A; C; T; T; T; C; A; T; T; A; C; G; T; C; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; C; G; C; C; A; T; C; A; T; G; G; G; A; G; G; C; T; T; C; A; T; C; C; A; C; T; G; G; T; T; C; C; C; A; C; T; A; T; T; C; T; C; A; G; G; C; T; A; C; A; C; C; T; T; A; G; A; C; C; A; G; A; C; C; T; A; T; G; C; T; A; A; A; A; T; T; C; A; C; T; T; C; A; T; C; A; C; C; A; T; A; T; T; T; A; T; C; G; G; C; G; T; A; A; A; T; T; T; A; A; C; T; T; T; C; T; T; C; C; C; A; C; A; A; C; A; T; T; T; C; C; T; C; G; G; C; C; T; G; T; C; A; G; G; C; A; T; A; C; C; C; C; G; A; C; G; C; T; A; C; T; C; C; G; A; C; T; A; C; C; C; C; G; A; C; G; C; G; T; A; C; A; C; C; A; C; C; T; G; A; A; A; T; A; T; T; T; T; A; T; C; A; T; C; C; G; C; A; G; G; C; T; C; A; T; T; T; A; T; C; T; C; C; C; T; A; A; C; A; G; C; A; G; T; C; A; T; A; C; T; A; A; T; A; A; T; T; T; T; C; A; T; A; A; T; T; T; G; A; G; A; A; G; C; C; T; T; C; G; C; C; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; C; A; A; T; A; G; T; T; G; A; A; C; A; A; C; C; C; T; C; C; A; C; A; A; G; C; C; T; A; G; A; G; T; G; A; T; T; G; T; A; C; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; T; G; A; A; G; A; A; C; C; C; G; T; C; T; A]

let gorilla: helix = [T; A; T; A; A; A; A; C; C; C; C; C; C; G; C; C; A; T; A; A; C; C; C; A; A; T; A; C; C; A; A; A; C; A; C; C; C; C; T; T; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; T; T; A; C; T; T; C; T; T; C; T; A; T; C; T; C; T; C; C; C; A; G; T; A; C; T; A; G; C; T; G; C; T; G; G; A; A; T; T; A; C; C; A; T; A; T; T; A; T; T; A; A; C; A; G; A; C; C; G; T; A; A; C; C; T; C; A; A; C; A; C; C; A; C; C; T; T; T; T; T; C; G; A; C; C; C; A; G; C; C; G; G; A; G; G; A; G; G; A; G; A; T; C; C; T; A; T; C; C; T; A; T; A; C; C; A; A; C; A; C; T; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; A; C; A; C; C; C; C; G; A; A; G; T; T; T; A; C; A; T; T; C; T; A; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; C; T; C; C; C; A; C; A; T; T; G; T; A; A; C; T; T; A; T; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; C; G; G; A; T; A; T; A; T; A; G; G; T; A; T; A; G; T; C; T; G; A; G; C; T; A; T; A; A; T; A; T; C; A; A; T; T; G; G; T; T; T; C; C; T; G; G; G; A; T; T; T; A; T; T; G; T; G; T; G; A; G; C; C; C; A; C; C; A; C; A; T; A; T; T; T; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; A; G; A; T; A; C; A; C; G; A; G; C; C; T; A; C; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; C; G; C; T; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; C; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; C; C; A; T; G; G; A; A; G; T; A; A; T; A; C; C; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; A; T; G; C; T; C; T; G; A; G; C; C; C; T; A; G; G; G; T; T; C; A; T; T; T; T; T; C; T; C; T; T; C; A; C; T; G; T; A; G; G; C; G; G; C; C; T; A; A; C; C; G; G; C; A; T; C; G; T; A; C; T; A; G; C; A; A; A; C; T; C; G; T; C; A; T; T; A; G; A; T; A; T; C; G; T; G; C; T; G; C; A; C; G; A; C; A; C; A; T; A; T; T; A; C; G; T; C; G; T; A; G; C; T; C; A; C; T; T; C; C; A; C; T; A; T; G; T; C; C; T; A; T; C; T; A; T; A; G; G; A; G; C; T; G; T; G; T; T; C; G; C; C; A; T; C; A; T; A; G; G; G; G; G; C; T; T; T; A; T; T; C; A; C; T; G; A; T; T; T; C; C; C; C; T; A; T; T; C; T; C; A; G; G; C; T; A; C; A; C; T; C; T; A; G; A; T; C; A; A; A; C; C; T; A; C; G; C; C; A; A; A; A; T; C; C; A; C; T; T; T; G; C; C; A; T; C; A; T; A; T; T; C; A; T; T; G; G; C; G; T; T; A; A; T; C; T; A; A; C; C; T; T; C; T; T; C; C; C; A; C; A; A; C; A; C; T; T; T; C; T; T; G; G; C; C; T; A; T; C; T; G; G; A; A; T; A; C; C; C; C; G; A; C; A; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; T; A; C; T; A; C; A; T; G; A; A; A; T; A; T; C; C; T; G; T; C; A; T; C; C; G; T; G; G; G; C; T; C; A; T; T; C; A; T; T; T; C; C; C; T; A; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; T; A; T; A; A; T; C; T; G; A; G; A; A; G; C; C; T; T; C; G; C; C; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; A; T; C; G; A; A; G; A; A; C; C; C; T; C; C; A; C; A; A; A; T; C; T; G; G; A; G; T; G; A; C; T; G; T; A; T; G; G; A; T; G; C; C; C; T; C; C; A; C; C; C; T; A; T; C; A; T; A; C; A; T; T; T; G; A; A; G; A; G; T; C; T; G; T; A; T; A]

let chimpanzee: helix = [T; A; T; A; A; A; A; C; C; T; C; C; T; G; C; C; A; T; G; A; C; C; C; A; A; T; A; C; C; A; A; A; C; A; C; C; C; C; T; C; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; T; T; A; C; T; T; C; T; C; C; T; A; T; C; C; C; T; C; C; C; A; G; T; C; C; T; A; G; C; T; G; C; T; G; G; C; A; T; C; A; C; C; A; T; A; C; T; A; T; T; G; A; C; A; G; A; T; C; G; T; A; A; C; C; T; C; A; A; C; A; C; T; A; C; C; T; T; C; T; T; C; G; A; C; C; C; A; G; C; C; G; G; G; G; G; A; G; G; A; G; A; C; C; C; T; A; T; T; C; T; A; T; A; T; C; A; A; C; A; C; T; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; C; C; A; C; C; C; C; G; A; A; G; T; T; T; A; T; A; T; T; C; T; T; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; T; T; C; C; C; A; C; A; T; T; G; T; A; A; C; T; T; A; T; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; T; G; G; A; T; A; T; A; T; A; G; G; C; A; T; G; G; T; T; T; G; A; G; C; T; A; T; A; A; T; A; T; C; A; A; T; T; G; G; C; T; T; C; C; T; A; G; G; G; T; T; T; A; T; C; G; T; G; T; G; A; G; C; A; C; A; C; C; A; T; A; T; A; T; T; T; A; C; A; G; T; A; G; G; G; A; T; A; G; A; C; G; T; A; G; A; C; A; C; C; C; G; A; G; C; C; T; A; T; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; T; G; C; T; A; T; T; C; C; T; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; C; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; T; C; A; C; G; G; A; A; G; C; A; A; T; A; T; G; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; G; T; A; C; T; C; T; G; A; G; C; C; C; T; A; G; G; G; T; T; T; A; T; C; T; T; T; C; T; C; T; T; C; A; C; C; G; T; A; G; G; T; G; G; C; C; T; A; A; C; C; G; G; C; A; T; T; G; T; A; C; T; A; G; C; A; A; A; C; T; C; A; T; C; A; T; T; A; G; A; C; A; T; C; G; T; G; C; T; A; C; A; C; G; A; C; A; C; A; T; A; C; T; A; C; G; T; C; G; T; A; G; C; C; C; A; C; T; T; C; C; A; C; T; A; C; G; T; T; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; C; G; C; C; A; T; C; A; T; A; G; G; A; G; G; C; T; T; C; A; T; T; C; A; C; T; G; A; T; T; C; C; C; C; C; T; A; T; T; C; T; C; A; G; G; C; T; A; T; A; C; C; C; T; A; G; A; C; C; A; A; A; C; C; T; A; T; G; C; C; A; A; A; A; T; C; C; A; A; T; T; T; G; C; C; A; T; C; A; T; G; T; T; C; A; T; T; G; G; C; G; T; A; A; A; C; C; T; A; A; C; C; T; T; C; T; T; C; C; C; A; C; A; G; C; A; C; T; T; C; C; T; T; G; G; C; C; T; A; T; C; T; G; G; G; A; T; G; C; C; C; C; G; A; C; G; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; C; A; C; C; A; C; A; T; G; A; A; A; T; G; T; C; C; T; A; T; C; A; T; C; C; G; T; A; G; G; C; T; C; A; T; T; T; A; T; C; T; C; C; C; T; G; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; C; A; T; G; A; T; T; T; G; A; G; A; A; G; C; C; T; T; T; G; C; T; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; G; T; A; G; A; A; G; A; G; C; C; C; T; C; C; G; C; A; A; A; C; C; T; G; G; A; A; T; G; A; C; T; A; T; A; T; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; C; G; A; A; G; A; A; C; C; C; G; T; A; T; A]

let human: helix = [T; A; T; A; A; A; A; C; C; C; C; C; T; G; C; C; A; T; A; A; C; C; C; A; A; T; A; C; C; A; A; A; C; G; C; C; C; C; T; T; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; C; T; A; C; T; T; C; T; C; C; T; A; T; C; T; C; T; C; C; C; A; G; T; C; C; T; A; G; C; T; G; C; T; G; G; C; A; T; C; A; C; T; A; T; A; C; T; A; C; T; A; A; C; A; G; A; C; C; G; C; A; A; C; C; T; C; A; A; C; A; C; C; A; C; C; T; T; C; T; T; C; G; A; C; C; C; C; G; C; C; G; G; A; G; G; A; G; G; A; G; A; C; C; C; C; A; T; T; C; T; A; T; A; C; C; A; A; C; A; C; C; T; A; T; T; C; T; G; A; T; T; T; T; T; C; G; G; C; C; A; C; C; C; T; G; A; A; G; T; T; T; A; T; A; T; T; C; T; C; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; C; T; C; C; C; A; T; A; T; T; G; T; A; A; C; T; T; A; C; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; T; G; G; A; T; A; C; A; T; A; G; G; T; A; T; G; G; T; C; T; G; A; G; C; T; A; T; G; A; T; A; T; C; A; A; T; T; G; G; C; T; T; C; C; T; A; G; G; G; T; T; T; A; T; C; G; T; G; T; G; A; G; C; A; C; A; C; C; A; T; A; T; A; T; T; T; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; A; G; A; C; A; C; A; C; G; A; G; C; A; T; A; T; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; C; G; C; T; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; T; A; G; C; T; G; A; C; T; C; G; C; C; A; C; A; C; T; C; C; A; C; G; G; A; A; G; C; A; A; T; A; T; G; A; A; A; T; G; A; T; C; T; G; C; T; G; C; A; G; T; G; C; T; C; T; G; A; G; C; C; C; T; A; G; G; A; T; T; C; A; T; C; T; T; T; C; T; T; T; T; C; A; C; C; G; T; A; G; G; T; G; G; C; C; T; G; A; C; T; G; G; C; A; T; T; G; T; A; T; T; A; G; C; A; A; A; C; T; C; A; T; C; A; C; T; A; G; A; C; A; T; C; G; T; A; C; T; A; C; A; C; G; A; C; A; C; G; T; A; C; T; A; C; G; T; T; G; T; A; G; C; T; C; A; C; T; T; C; C; A; C; T; A; T; G; T; C; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; T; G; C; C; A; T; C; A; T; A; G; G; A; G; G; C; T; T; C; A; T; T; C; A; C; T; G; A; T; T; T; C; C; C; C; T; A; T; T; C; T; C; C; G; G; C; T; A; C; A; C; C; C; T; A; G; A; C; C; A; A; A; C; T; T; A; C; G; C; C; A; A; A; A; T; C; C; A; T; T; T; C; A; C; T; A; T; C; A; T; A; T; T; C; A; T; C; G; G; C; G; T; A; A; A; T; C; T; A; A; C; T; T; T; C; T; T; C; C; C; A; C; A; A; C; A; C; T; T; T; C; T; C; G; G; C; C; T; A; T; C; C; G; G; A; A; T; G; C; C; C; C; G; A; C; G; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; C; A; C; C; A; C; A; T; G; A; A; A; T; A; T; C; C; T; A; T; C; A; T; C; T; G; T; A; G; G; C; T; C; A; T; T; C; A; T; T; T; C; T; C; T; A; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; C; A; T; G; A; T; T; T; G; A; G; A; A; G; C; C; T; T; C; G; C; T; T; C; G; A; A; G; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; G; T; A; G; A; A; G; A; A; C; C; C; T; C; C; A; T; A; A; A; C; C; T; G; G; A; G; T; G; A; C; T; A; T; A; T; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; C; G; A; A; G; A; G; C; C; C; G; T; A; T; A]

(* To help with printing trees that contain this data, we provide the following
   function to convert helices to the corresponding species name. *)

(* Given a helix, output the ape name (e.g. "Orangutan") or "Non-Ape" if the
   helix doesn't match any of the ape helices. *)
let rec string_of_ape (ape: helix) : string =
  if ape = lar_gibbon then "Lar Gibbon"
  else if ape = pileated_gibbon then "Pileated Gibbon"
  else if ape = siamang then "Siamang"
  else if ape = white_cheeked_gibbon then "White Cheeked Gibbon"
  else if ape = orangutan then "Orangutan"
  else if ape = gorilla then "Gorilla"
  else if ape = chimpanzee then "Chimpanzee"
  else if ape = human then "Human"
  else "Non-Ape"


(* -------------------------------------------------------------------------- *)
(* ----- Problem 2: Compute the hamming distance between two helices -------- *)
(* -------------------------------------------------------------------------- *)

(* Given two equal-length helices, compute the number of differences between
   the nucleotide sequences, i.e. the number of nucleotides that would need to
   be changed in order to make the helices equivalent.

   You may ASSUME that this function will only be applied to lists with the
   same number of elements.  Do NOT test the lengths of the lists when
   implementing this function.  If you do so, your code will be
   ungradeable because it is TOO SLOW; doing so could require a length
   calculation to occur at every recursive invocation.

 
   It does not matter what this function returns when applied to
   different length lists. *)
let rec hamming_distance (x1: helix) (x2: helix) : int =
  begin match x1 with
  | [] -> 0
  | base1 :: tail1 -> begin match x2 with
                      | [] -> failwith "lists are differnt length"
                      | base2 :: tail2 -> if base1 = base2
                                            then hamming_distance tail1 tail2
                                          else 1 + hamming_distance tail1 tail2
                      end
  end

(* We can use the hamming distance function to determine the similarities
   between various species.

   After you have a correct implementation of hamming_distance, uncomment the
   following printing commands, run the program to see the output in the
   console, and then fill in the answer to 'question_most_like_human' below. *)

let mutation_string (ape: helix) : string =
  "Number of mutations between humans and " ^ (string_of_ape ape)
    ^ ": " ^ (string_of_int (hamming_distance human ape)) ^ "\n"


;; print_endline (mutation_string gorilla)
;; print_endline (mutation_string lar_gibbon)
;; print_endline (mutation_string pileated_gibbon)
;; print_endline (mutation_string siamang)
;; print_endline (mutation_string white_cheeked_gibbon)
;; print_endline (mutation_string orangutan)
;; print_endline (mutation_string chimpanzee)


(* Based on the number of mutations that were printed in the
   console, order the seven non-human ape helices by their similarity to
   humans, with the *most* human-like helix appearing first in the list. *)
let question_most_like_human () : helix list =
  [chimpanzee; gorilla; orangutan; siamang; pileated_gibbon;
  white_cheeked_gibbon; lar_gibbon]

(* How do you know that 'question_most_like_human' is correct? What sort of
   tests could you write for it?

   Even though you don't know the actual order of the ape helices, you know
   that their similarity to humans should decrease throughout the list.
   Therefore, to test your answer, write a function that determines whether a
   list of helices is ordered in decreasing similarity to humans. This function
   should return true for lists that are empty or that contain exactly one
   helix. *)

let rec decreasing_similarity (apes: helix list) : bool =
  begin match apes with
  | [] -> true
  | [x] -> true
  | x1 :: x2 :: xs -> hamming_distance x1 human < hamming_distance x2 human &&
                        decreasing_similarity (x2 :: xs)
  end




(* -------------------------------------------------------------------------- *)
(* ----- Problem 3: Decode a helix to get an acid chain --------------------- *)
(* -------------------------------------------------------------------------- *)

(* DNA describes how to build organisms by encoding instructions for making
   amino acid chains, which in turn enables basic cellular functions. In this
   problem, you will write functions that determine the amino acid chains that
   are encoded in a particular helix.

   The key to this decoding is that every nucleotide triplet in the helix
   encodes one of 20 acids, or else indicates the end of the chain. *)

type acid =
| Ala | Arg | Asn | Asp | Cys | Glu | Gln | Gly | His | Ile | Leu | Lys | Met
| Phe | Pro | Ser | Thr | Trp | Tyr | Val | END

(* Given a nucleotide triplet, the function acid_of_triplet outputs a decoded
   acid or END. *)
let rec acid_of_triplet (n1: nucleotide) (n2: nucleotide) (n3: nucleotide)
        : acid =
  begin match (n1, n2, n3) with
  | (A, A, A) -> Phe | (A, A, G) -> Phe | (A, A, T) -> Leu | (A, A, C) -> Leu
  | (G, A, A) -> Leu | (G, A, G) -> Leu | (G, A, T) -> Leu | (G, A, C) -> Leu
  | (T, A, A) -> Ile | (T, A, G) -> Ile | (T, A, T) -> Ile | (T, A, C) -> Met
  | (C, A, A) -> Val | (C, A, G) -> Val | (C, A, T) -> Val | (C, A, C) -> Val
  | (A, G, A) -> Ser | (A, G, G) -> Ser | (A, G, T) -> Ser | (A, G, C) -> Ser
  | (G, G, A) -> Pro | (G, G, G) -> Pro | (G, G, T) -> Pro | (G, G, C) -> Pro
  | (T, G, A) -> Thr | (T, G, G) -> Thr | (T, G, T) -> Thr | (T, G, C) -> Thr
  | (C, G, A) -> Ala | (C, G, G) -> Ala | (C, G, T) -> Ala | (C, G, C) -> Ala
  | (A, T, A) -> Tyr | (A, T, G) -> Tyr | (A, T, T) -> END | (A, T, C) -> END
  | (G, T, A) -> His | (G, T, G) -> His | (G, T, T) -> Gln | (G, T, C) -> Gln
  | (T, T, A) -> Asn | (T, T, G) -> Asn | (T, T, T) -> Lys | (T, T, C) -> Lys
  | (C, T, A) -> Asp | (C, T, G) -> Asp | (C, T, T) -> Glu | (C, T, C) -> Glu
  | (A, C, A) -> Cys | (A, C, G) -> Cys | (A, C, T) -> END | (A, C, C) -> Trp
  | (G, C, A) -> Arg | (G, C, G) -> Arg | (G, C, T) -> Arg | (G, C, C) -> Arg
  | (T, C, A) -> Ser | (T, C, G) -> Ser | (T, C, T) -> Arg | (T, C, C) -> Arg
  | (C, C, A) -> Gly | (C, C, G) -> Gly | (C, C, T) -> Gly | (C, C, C) -> Gly
  end

(* Your task: given a helix, decode its first acid chain. This is done as
   follows...

   1. Start decoding by scanning the helix for the first occurrence of the
      sequence [T; A; C]. This starts the acid chain with Met as the head
      element.

   2. After the first occurrence of [T; A; C], decode the following nucleotides
      in the helix in triplets and build a list of the results.

   3. Stop decoding when either (a) we run out of triplets or (b) we encounter
      one of the triplets encoding the END acid ([A; T; T], [A; C; T], or
      [A; T; C]).  In this case, the END acid should NOT be included in the
      output acid list. *)
      
let rec start_coding (oligo: helix) : acid list =
  begin match oligo with
  | [] -> []
  | [_] -> []
  | [_; _] -> []
  | [basex; basey; basez] ->
    if ((basex = A && basey = T && basez = T) ||
      (basex = A && basey = T && basez = C) ||
      (basex = A && basey = C && basez = T)) then []
    else [acid_of_triplet basex basey basez]
  | basep :: baseq :: baser :: rest ->
    if ((basep = A && baseq = T && baser = T) ||
      (basep = A && baseq = T && baser = C) ||
      (basep = A && baseq = C && baser = T)) then []
    else (acid_of_triplet basep baseq baser) :: start_coding rest
  end

let rec acids_of_helix (x: helix) : acid list =
  begin match x with
  | [] -> []
  | [_] -> []
  | [_; _] -> []
  | [basea; baseb; basec] ->
    if (basea = T && baseb = A && basec = C) then [Met] else []
  | base1 :: base2 :: base3 :: bases ->
    if (base1 = T && base2 = A && base3 = C)
      then Met :: start_coding bases
    else acids_of_helix (base2 :: base3 :: bases)
  end



(* -------------------------------------------------------------------------- *)
(* ----- Kudos problem: Decode all acid chains in a helix ------------------- *)
(* -------------------------------------------------------------------------- *)

(* The next problem is not required. It is worth zero points, but will
   challenge your ability to work with recursive functions over lists. If you'd
   like to attempt this problem, you may want to come back to it after you have
   finished the rest of the assignment. *)

(* Problem 3 above asks you to decode the first acid chain of a helix. However,
   a given helix may describe several chains in order. Your job is to write a
   function that produces *all* of these chains, not just the first one.

   A few points to observe:
   - You should look for the next acid chain after the end triplet of the
     previous chain.
   - The beginning of the next chain (TAC) may not be immediately after the end
     of the previous one, there could be any number of nucleotides in between.
   - You should not decode overlapping chains. If, within the middle of one
     amino acid-encoding nucleotide sequence there's another T-A-C, you
     shouldn't also return that acid list. *)

let rec all_acids_of_helix (x: helix) : acid list list =
  failwith "all_acids_of_helix: unimplemented"

(* These tests are commented out because they are for the kudos problem.
   If you attempt this problem, remember to uncomment the tests. *)
(*
let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T] = [[Met]]
;; run_test "all_acids_of_helix  [T; A; C; A; C; T]" test

let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T; T; A; C; A; C; T] = [[Met]; [Met]]
;; run_test "all_acids_of_helix [T; A; C; A; C; T; T; A; C; A; C; T]" test

let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T; G; A; T; A; C; A; C; T] = [[Met]; [Met]]
;; run_test "all_acids_of_helix [T; A; C; A; C; T; G; A; T; A; C; A; C; T]" test
*)


(* -------------------------------------------------------------------------- *)
(* ----- Problem 4:  Representing phylogenetic trees ------------------------ *)
(* -------------------------------------------------------------------------- *)

(* Now that we have a DNA datatype and the ape data, we can analyze this data
   to figure out the relationship between current species. That relationship is
   described by evolutionary (or phylogenetic) trees, which show how the common
   ancestors for each species relate to each other. Specifically, we will take
   four ape helices and:

   1. First, generate all possible evolutionary trees with the ape DNA at the
      leaves. Each tree represents a possible set of family relationships among
      the given species;

   2. Next, take these generated trees and fill in hypothetical genetic codes
      for the ancestor species. These hypothetical codes will be stored in an
      auxiliary data structure called a labeled_tree (where internal nodes also
      store genetic information);

   3. Then, estimate each labeled_tree's complexity, and guess at how many
      evolutionary events were required to create the given tree; and

   4. Finally, return the simplest tree, stripping off the guessed ancestors.

   Before completing the remaining problems of this assignment, you should read
   through all of them so that you can see how they fit together and correspond
   to the four steps above. *)

(* An evolutionary tree is a binary tree whose leaves are labeled with
   DNA helices. As we learned in lecture, this kind of tree is either
   a Node or a Leaf. A Leaf merely contains a value with no children,
   while a Node contains two "child" trees; these "children" represent
   the two subtrees underneath this particular Node in the overall
   tree structure. *)

type tree =
| Leaf of helix
| Node of tree * tree

(* In this homework assignment, we will use the *real* evolutionary tree that
   scientists believe describes the relationships between ape species to
   evaluate our algorithm. The real tree is shown in Figure A on the homework
   website. *)

(* For example, we can represent the left side of the tree from Figure A as
   follows: *)

let greater_apes () : tree =
  Node (
    Leaf orangutan,
    Node (
      Leaf gorilla,
      Node (
        Leaf human,
        Leaf chimpanzee
      )
    )
  )

(* Furthermore, the following function can be used to display evolutionary
   trees in the console. *)

(* Given a tree whose leaves are labeled with the ape helices, output a string
   containing an "ascii art" picture of the tree. *)
let rec string_of_tree (r: tree) : string =
  let spaces (n: int) : string = String.make n ' ' in
  let dashes (n: int) : string = String.make n '-' in

  let rec zip ((t1, w1): string list * int) ((t2, w2): string list * int)
              : string list * int =
    let rec aux (t1: string list) (t2: string list) : string list =
      begin match t1, t2 with
      | [], _  -> List.map (fun s -> spaces w1 ^ s) t2
      | _ , [] -> List.map (fun s -> s ^ spaces w2) t1
      | t :: ts, s :: ss -> (t ^ s) :: (aux ts ss)
      end
    in
    let h1 = w1 / 2 in
    let h2 = w2 / 2 in
    let line1 = spaces h1 ^ dashes (h1 + h2 + 2) ^ spaces h2 in
    let line2 = spaces h1 ^ "|" ^ spaces (h1 + h2) ^ "|" ^ spaces h2 in
    (line1 :: line2 :: aux t1 t2, w1 + w2 + 2)
  in

  let rec aux (t: tree) : string list * int =
    begin match t with
    | Leaf x ->
        let str1 = " " ^ string_of_ape x ^ " " in
        let str2 =
          if (String.length str1) mod 2 = 0 then str1
          else " " ^ str1
        in
        ([str2], String.length str2)
    | Node (left, right) ->
        zip (aux left) (aux right)
    end
  in
  let (strs, w) = aux r in
  let strs2 = (spaces (w/2) ^ " |" ^ (spaces (w/2))) :: strs in
  (String.concat "\n" strs2) ^ "\n"

(* These commands display the greater_apes tree in the console when your
   program runs. Before continuing, make sure that you understand the connection
   between this display, the definition of 'greater_apes' above, and
   Figure A. *)

;; print_endline "Greater apes (from Figure A)"
;; print_endline "----------------------------"
;; print_endline (string_of_tree (greater_apes ()))

(* Your next job is to fill in the tree for the lesser apes, also from the same
   figure. *)

let lesser_apes () : tree =
  Node (
    Leaf white_cheeked_gibbon,
    Node (
      Leaf siamang,
      Node (
        Leaf lar_gibbon,
        Leaf pileated_gibbon
      )
    )
  )

(* Once you have defined this tree, uncomment the following commands and make
   sure that it matches the one in Figure A. *)

;; print_endline "Lesser apes (from Figure A)"
;; print_endline "---------------------------"
;; print_endline (string_of_tree (lesser_apes ()))

(* One way to test that you have defined 'lesser_apes' correctly is to count the
   number of species that it contains. We can calculate this number by counting
   the number of leaf nodes in the tree. *)

let rec count_leaves (t: tree) : int =
  begin match t with
  | Leaf _ -> 1
  | Node (lt, rt) -> count_leaves lt + count_leaves rt
  end

(* -------------------------------------------------------------------------- *)
(* ----------------- Generating Possible Phylogenetic Trees ----------------- *)
(* -------------------------------------------------------------------------- *)

(* Now that we have a representation for phylogenetic trees, we can move to
   Step 1---generating all possible trees by listing all distinct tree
   structures with the input helices at the leaves. Because internal nodes
   correspond to ancestor species, we will (later) guess the internal labels
   before evaluating functions that compute tree complexity.

   The following function (whose body we provide for you) generates all possible
   trees for a set of four present-day species... *)

(* Given four helices that should appear on the leaves of each output tree, list
   all distinct tree structures with the input helices at their leaves. *)
let rec all_trees (x1: helix) (x2: helix) (x3: helix) (x4: helix)
                  : tree list =
  let tree1 (x1: helix) (x2: helix) (x3: helix) (x4: helix): tree =
    Node (Node (Leaf x1, Leaf x2),  Node (Leaf x3, Leaf x4)) in
  let tree2 (x1: helix) (x2: helix) (x3: helix) (x4: helix): tree =
    Node (Leaf x1, Node (Leaf x2, Node (Leaf x3, Leaf x4))) in
  [tree1 x1 x2 x3 x4; tree1 x1 x3 x2 x4; tree1 x1 x4 x2 x3;
   tree2 x1 x2 x3 x4; tree2 x1 x3 x2 x4; tree2 x1 x4 x2 x3;
   tree2 x2 x1 x3 x4; tree2 x2 x3 x1 x4; tree2 x2 x4 x1 x3;
   tree2 x3 x1 x2 x4; tree2 x3 x2 x1 x4; tree2 x3 x4 x1 x2;
   tree2 x4 x1 x2 x3; tree2 x4 x2 x1 x3; tree2 x4 x3 x1 x2]

let all_greater_ape_trees () : tree list =
  all_trees gorilla human chimpanzee orangutan

let all_lesser_ape_trees () : tree list =
  all_trees lar_gibbon pileated_gibbon siamang white_cheeked_gibbon

(* If you would like to see the candidate evolutionary trees for the greater
   apes, uncomment the following lines. Note that the tree we are hoping to
   pick out (from Figure A) is third from the end. *)

let rec string_of_tree_list (l: tree list) : string =
  begin match l with
  | []       -> ""
  | hd :: tl -> string_of_tree hd ^ "\n\n" ^ string_of_tree_list tl
  end

(*
;; print_endline "Possible evolutionary trees (greater apes)"
;; print_endline "------------------------------------------"
;; print_endline (string_of_tree_list (all_greater_ape_trees ()))
*)


(* -------------------------------------------------------------------------- *)
(* ------------------- Problem 5:  Guessing ancestors  ---------------------- *)
(* -------------------------------------------------------------------------- *)

(* Now on to step 2. The next step is to compare the generated evolutionary
   trees above with the help of an intermediate data structure, called a
   'labeled_tree'. This data structure is like the evolutionary tree, except
   that internal nodes are labeled with guesses about the dna data of ancestor
   species. *)

type labeled_tree =
| LLeaf of helix
| LNode of labeled_tree * helix * labeled_tree

(* The following helper function (for you to write) will be useful later on: *)

(* Find the helix contained within the given tree. *)
let helix_of_tree (r: labeled_tree) : helix =
  begin match r with
  | LLeaf oligo -> oligo
  | LNode (_, oligo, _) -> oligo
  end

(* Our goal is to compare labeled trees, but it is a nice to think first about
   what we will do once we find the best one. We would like to produce an
   (unlabeled) evolutionary tree, so we will need to strip off the guessed
   ancestors.

   Given a labeled_tree as defined above, we can do this by returning a tree of
   identical structure with the helix in each Node removed. Don't forget to add
   another test for unlabel_tree. *)
let rec unlabel_tree (r: labeled_tree) : tree =
  begin match r with
  | LNode (lt, _, rt) -> Node (unlabel_tree lt, unlabel_tree rt)
  | LLeaf x -> Leaf x
  end


(* -------------------------------------------------------------------------- *)

(* The next step is to label the internal nodes of our generated trees. See
   Figure C for a completely labeled candidate tree whose helices are of length
   four. Figure C is sample output for add_ancestor_labels, which you'll write
   below. *)

(* BEFORE WRITING add_ancestor_labels, READ CAREFULLY THROUGH THE HELPER
   FUNCTION guess_parent_helix AND FILL IN THE REQUIRED TEST CASE RESULTS. *)

(* Assuming x1 and x2 are valid helices, guess the valid helix that labels their
   parent node in an evolutionary tree. The guessed helix is computed using
   nucleotide-by-nucleotide comparison of x1 and x2: if the nucleotides match,
   the corresponding parent nucleotide is the same. If they don't match, the
   corresponding parent nucleotide is (arbitrarily) A. *)
let rec guess_parent_helix (x1: helix) (x2: helix) : helix =
  begin match x1, x2 with
  | [], []             -> []
  | h1 :: t1, h2 :: t2 ->
      (if h1 = h2 then h1 else A) :: guess_parent_helix t1 t2
  | _  -> failwith "invalid input: x1 and x2 have unequal lengths"
  end

(* -------------------------------------------------------------------------- *)

(* Next, we need a way to infer the genetic makeup of the ancestors of species
   whose genetic makeup is already known. Below, you will be writing a function
   that attempts to guess the DNA of some ancestor species in a possible
   evolutionary tree, given the DNA of their descendants. *)

(* Given an unlabeled tree generate a tree with the same shape but with all
   internal nodes properly labeled. Use guess_parent_helix to compute the
   appropriate label for a parent node, given the labels of its two children.

   Some points to remember...

   - Your function should work for a tree of ANY height; make sure to test this
     prior to continuing with the assignment. We have provided a test stub
     reminding you to test this below.

   - You should NOT assume that all input trees are "balanced"; that is, the
     left and right sub-trees of any node may in general have different numbers
     of nodes! *)
let rec add_ancestor_labels (t: tree) : labeled_tree =
  begin match t with
  | Leaf x -> LLeaf x
  | Node (Leaf a, Leaf b) -> LNode (LLeaf a,
      guess_parent_helix a b, LLeaf b)
  | Node (Node (a, b), Leaf p) ->
      LNode (add_ancestor_labels (Node (a, b)),
      guess_parent_helix (helix_of_tree (add_ancestor_labels (Node (a, b)))) p,
      LLeaf p)
  | Node (Leaf p, Node (a, b)) ->
      LNode (LLeaf p,
      guess_parent_helix p (helix_of_tree (add_ancestor_labels (Node (a, b)))),
      add_ancestor_labels (Node (a, b)))
  | Node (Node (lt1, rt1), Node (lt2, rt2)) ->
      LNode (add_ancestor_labels (Node (lt1, rt1)),
      guess_parent_helix (helix_of_tree (add_ancestor_labels (Node (lt1, rt1))))
      (helix_of_tree (add_ancestor_labels (Node (lt2, rt2)))),
      add_ancestor_labels (Node (lt2, rt2)))
  end

(* Once your test cases for add_ancestor_labels pass, we can use this function 
   to label all of the trees for the greater and lesser apes. To label all of 
   the trees in the list of candidates, we define (for you) the following helper
   function: *)

let rec add_ancestor_labels_list (ts: tree list) : labeled_tree list =
  begin match ts with
  | []       -> []
  | hd :: tl -> add_ancestor_labels hd :: add_ancestor_labels_list tl
  end

(* Labeled trees for lesser and greater apes. *)

let labeled_greater_ape_trees () : labeled_tree list =
  add_ancestor_labels_list (all_greater_ape_trees ())

let labeled_lesser_ape_trees () : labeled_tree list =
  add_ancestor_labels_list (all_lesser_ape_trees ())


(* -------------------------------------------------------------------------- *)
(* ----- Problem 6: Parent-Child Hamming ------------------------------------ *)
(* -------------------------------------------------------------------------- *)

(* Now we move to step 3: we want to compare the labeled_trees by approximating
   their complexity and then choosing the simplest one. Below,we can approximate
   the complexity of a tree by summing the number of mutations needed to convert
   between parent and child DNA sequences. *)

(* Given a labeled tree, compute the sum of Hamming distances between all
   parent-child pairs. Your solution should use the hamming_distance function
   from Problem 2. *)
let rec parent_child_hamming (r: labeled_tree) : int =
  begin match r with
  | LLeaf _ -> 0
  | LNode (LLeaf x, a, LLeaf y) ->
    hamming_distance a x + hamming_distance a y
  | LNode (LLeaf x, a, LNode (lt, y, rt)) ->
    hamming_distance a x + hamming_distance a y +
    parent_child_hamming (LNode (lt, y, rt))
  | LNode (LNode (lt, x, rt), a, LLeaf y) ->
    hamming_distance a x + hamming_distance a y +
    parent_child_hamming (LNode (lt, x, rt))
  | LNode (LNode (lt1, x, rt1), a, LNode (lt2, y, rt2)) ->
    hamming_distance a x + hamming_distance a y +
    parent_child_hamming (LNode (lt1, x, rt1)) +
    parent_child_hamming (LNode (lt2, y, rt2))
  end


(* -------------------------------------------------------------------------- *)
(* ----- Problem 7: Finding the simplest tree ------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Finally we are at step 4: Given a list of labeled trees, we want to find the
   simplest one in the list, using the parent_child_hamming function defined
   above to estimate the complexity of the tree.

   The simplest_tree function should, given a list of labeled_trees, return both
   the simplest tree in the list and its complexity. If there  are two trees in
   the list with the same complexity, it should return the first one.

   You can assume that this function will only be applied to a non-empty list of
   labeled_trees. Your function should throw an error (with failwith) if it is
   passed an empty list. Again, make sure that you add additional test cases for
   this function. *)

let rec simplest_tree (rs: labeled_tree list) : labeled_tree * int =
  begin match rs with
  | [] -> failwith "simplest_tree called on empty list"
  | [x] -> (x, parent_child_hamming x)
  | [x1; x2] -> if (parent_child_hamming x1 > parent_child_hamming x2)
                  then (x2, parent_child_hamming x2)
                else (x1, parent_child_hamming x1)
  | x1 :: x2 :: xs -> if (parent_child_hamming x1 > parent_child_hamming x2)
                        then simplest_tree (x2 :: xs)
                      else simplest_tree (x1 :: xs)
  end


(* -------------------------------------------------------------------------- *)

(* We are now ready to put everything together to see how well calculating
   evolutionary trees using the parent-child hamming distance complexity measure
   works. *)

let simplest_greater_ape_tree () : tree =
  let (t, _) = simplest_tree (labeled_greater_ape_trees ()) in
  unlabel_tree t

(* We can compare this computed tree with the one in Figure A, to see that this
   methodology works well for greater apes. Uncomment the following printing
   commands to make sure that the result matches. If this part of the program 
   runs slowly, you'll want to recomment these commands before you submit. *)

(*
;; print_endline "Computed evolutionary tree for greater apes"
;; print_endline "-------------------------------------------"
;; print_endline (string_of_tree (simplest_greater_ape_tree ())) *)

(* However, trying this out for lesser apes, produces something close, but not
   quite exactly what we wanted. Uncomment the printing commands below to see
   how close we get. *)

let simplest_lesser_ape_tree () : tree =
  let (t, _) = simplest_tree (labeled_lesser_ape_trees ()) in
  unlabel_tree t

(*
;; print_endline "Computed evolutionary tree for lesser apes"
;; print_endline "------------------------------------------"
;; print_endline (string_of_tree (simplest_lesser_ape_tree ())) *)


(* -------------------------------------------------------------------------- *)
(* ----- Problem 8: Refactoring --------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Once you have solved a problem, it is always a good idea to go back, look for
   repeated patterns, and generalize your solution. In  this case, if you look
   closely at the computation of the simplest trees for the lesser and greater
   apes, you can see that we are doing the same  thing twice. Capture steps 1-4
   with a single function that starts with *any* four ape helices and outputs
   the computed simplest tree.

   Note that your solution will be simple (no more than 4 lines long) and you
   will be able to reuse much of the code that you have already developed in
   this assignment. In particular, your implementation below will not require
   you to write any additional helper functions. *)

let find_simplest_tree (x1: helix) (x2: helix) (x3: helix) (x4: helix) : tree =
  let trees = all_trees x1 x2 x3 x4 in
  let labeled_trees = add_ancestor_labels_list trees in
  let (x, _) = simplest_tree labeled_trees in
  unlabel_tree x




(********************* WRITE ALL TESTS BELOW THIS POINT ***********************)

(************************ complementary_helix tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test cases
   we provided. *)

let test () : bool =
  complementary_helix [T] = [A]
;; run_test "complementary_helix T" test

let test () : bool =
  complementary_helix [C; T; T; C] = [G; A; A; G]
;; run_test "complementary_helix [C; T; T; C]" test

let test () : bool =
  complementary_helix [C; T; A; A; T; G; T] = [G; A; T; T; A; C; A]
;; run_test "complementary_helix [C; T; A; A; T; G; T]" test

let test () : bool =
  complementary_helix [] = []
;; run_test "complementary_helix empty" test



(*************************** hamming_distance tests ***************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem. *)

let test () : bool =
  hamming_distance [G; A] [G; T] = 1
;; run_test "hamming_distance [G; A] [G; T]" test

let test () : bool =
  hamming_distance [] [] = 0
;; run_test "hamming_distance empty" test

let test () : bool =
  hamming_distance [G; A] [G; A] = 0
;; run_test "hamming_distance same DNA" test

let test () : bool =
  hamming_distance [G; A; T; A; C; G] [G; A; T; A; C; T] = 1
;; run_test "hamming_distance difference at end" test



(************************ decreasing_similarity tests *************************)

(* Here's an example test case for the list from question_most_like_human. Be 
   sure to add your own--we will be grading the tests for this problem. *)

let test () : bool =
  decreasing_similarity (question_most_like_human ())
;; run_test "decreasing_similarity question_most_like_human" test

let test () : bool =
  not (decreasing_similarity [siamang; chimpanzee])
;; run_test "decreasing_similarity wrong order" test

let test () : bool =
  decreasing_similarity [gorilla]
;; run_test "decreasing_similarity singleton" test

let test () : bool =
  decreasing_similarity []
;; run_test "decreasing_similarity empty" test



(**************************** acids_of_helix tests ****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem.  Also be sure to fill in the incomplete test case
   we provided. *)

let test () : bool =
  acids_of_helix [A; G; T; A; C; C; C; A; T; C; G; G] = [Met; Gly; Ser]
;; run_test "acids_of_helix [A; G; T; A; C; C; C; A; T; C; G; G]" test

let test () : bool =
  acids_of_helix [A; G; T; A; C; T; A; C; T; C; C] = [Met; Met; Arg]
;; run_test "acids_of_helix [A; G; T; A; C; T; A; C; T; C; C]" test

let test () : bool =
  acids_of_helix [A; G; T; A; C; T; A; C; T; C; C; A; T; T] = [Met; Met; Arg]
;; run_test "acids_of_helix END codon" test

let test () : bool =
  acids_of_helix [A; G; T; A; C; T; A; C; T; C; C; A; T; ] = [Met; Met; Arg]
;; run_test "acids_of_helix end at incomplete codon" test



(***************************** count_leaves tests *****************************)

(* Here are two example tests cases. Both trees should contain exactly four 
   species. Don't forget to add your own test cases! *)

let test () : bool =
  count_leaves (greater_apes ()) = 4
;; run_test "count_leaves lesser_apes" test

let test () : bool =
  count_leaves (lesser_apes ()) = 4
;; run_test "count_leaves lesser_apes" test

let test () : bool =
  count_leaves (Leaf orangutan) = 1
;; run_test "count_leaves one" test

let test () : bool =
  count_leaves (Node (Leaf orangutan, Leaf gorilla)) = 2
;; run_test "count_leaves two" test



(**************************** helix_of_tree tests *****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem. *)

let test () : bool =
  helix_of_tree (LNode (LLeaf [T], [A], LLeaf [G])) = [A]
;; run_test "helix_of_tree (LNode (LLeaf [T], [A], LLeaf [G]))" test

let test () : bool =
  helix_of_tree (LLeaf [T]) = [T]
;; run_test "helix_of_tree LLeaf [T]" test

let test () : bool =
  helix_of_tree (LLeaf []) = []
;; run_test "helix_of_tree LLeaf []" test

let test () : bool =
  helix_of_tree (LNode (LNode
  (LLeaf [T], [C], LLeaf [G]), [A], LLeaf [G])) = [A]
;; run_test "helix_of_tree large" test



(**************************** unlabel_tree tests ******************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem. *)

let test () : bool =
  unlabel_tree (LNode (LLeaf [T], [A], LLeaf [G])) =
    Node (Leaf [T], Leaf [G])
;; run_test "unlabel_tree (LNode (LLeaf [T], [A], LLeaf [G]))" test

let test () : bool =
  unlabel_tree (LLeaf [A]) = Leaf [A]
;; run_test "unlabel_tree (LLeaf [A])" test

let test () : bool =
  unlabel_tree (LLeaf []) = Leaf []
;; run_test "unlabel_tree (LLeaf [])" test

let test () : bool =
  unlabel_tree (LNode (LNode (LLeaf [T], [A], LLeaf [G]), [G], LLeaf [T])) =
    Node (Node (Leaf [T], Leaf [G]), Leaf [T])
;; run_test
  "unlabel_tree (LNode (LNode (LLeaf [T], [A], LLeaf [G]), [G], LLeaf [T]))" test



(************************* guess_parent_helix tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test case
   we provided and don't forget to add your own test cases! *)

let test () : bool =
  guess_parent_helix [T; C; A] [G; C; A] = [A; C; A]
;; run_test "guess_parent_helix [T; C; A] [G; C; A]" test

let test () : bool =
  guess_parent_helix [A; C; G; G; T; A; C]
                     [C; T; G; C; T; A; A] =
    ([A; A; G; A; T; A; A])
;; run_test "guess_parent_helix long" test

let test () : bool =
  guess_parent_helix [] [] = []
;; run_test "guess_parent_helix [] []" test

let test () : bool =
  guess_parent_helix [A; T] [A; T] = [A; T]
;; run_test "guess_parent_helix [A] []" test



(************************ add_ancestor_labels tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test cases
   we provided and don't forget to add your own test cases! *)

let test () : bool =
  add_ancestor_labels (Node (Leaf [T], Leaf [G])) =
    LNode (LLeaf [T], [A], LLeaf [G])
;; run_test "add_ancestor_labels (Node (Leaf [T], [], Leaf [G]))" test

let test () : bool =
  add_ancestor_labels (Node (Leaf [T; C], Leaf [T; C])) =
    LNode (LLeaf [T; C], [T; C], LLeaf [T; C])
;; run_test "add_ancestor_labels (Node (Leaf [T; C], [], Leaf [T; C]))" test

let test () : bool =
  add_ancestor_labels (Leaf [T; C]) =
    LLeaf [T; C]
;; run_test "add_ancestor_labels (Leaf [T; C])" test

let test () : bool =
  add_ancestor_labels (Node (Node (Leaf [T; C],
                                  Leaf [T; C]),
                            (Node (Leaf [G; C],
                                  Leaf [T; C])))) =
                      (LNode (LNode (LLeaf [T; C],
                                    [T; C],
                                    LLeaf [T; C]),
                             [A; C],
                             (LNode (LLeaf [G; C],
                                    [A; C],
                                    LLeaf [T; C]))))
;; run_test "add_ancestor_labels depth-3 tree" test



(************************ parent_child_hamming tests **************************)

(* Here we give you one incomplete test to complete.  Add your own tests--we
   will grade the test cases for this problem.  Be sure to test for trees of
   greater depth. *)

let test () : bool =
  parent_child_hamming (LNode (LLeaf [T], [A], LLeaf [G])) = 2
;; run_test "parent_child_hamming (LNode (LLeaf [T], [A], LLeaf [G]))" test

let test () : bool =
  parent_child_hamming (LNode (LLeaf [A], [A], LLeaf [A])) = 0
;; run_test "parent_child_hamming (LNode (LLeaf [T], [A], LLeaf [G]))" test

let test () : bool =
  parent_child_hamming (LNode
                         (LNode
                           (LLeaf [C],
                           [A],
                           LLeaf [T]),
                         [A],
                         LLeaf [G])) = 3
;; run_test "parent_child_hamming depth-3" test

let test () : bool =
  parent_child_hamming (LNode (LLeaf [], [], LLeaf [])) = 0
;; run_test "parent_child_hamming empty helix" test



(**************************** simplest_tree tests *****************************)

(* Here are two example test cases. Don't forget to add your own test cases! *)

let test () : bool =
  simplest_tree [] = (LLeaf [], 0)
;; run_failing_test "simplest_tree []" test

let test () : bool =
  let t1 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t2 = LNode (LLeaf [G; T], [A; T],
                  LNode (LLeaf [T; T], [T; T], LLeaf [C; G])) in
  simplest_tree [t1; t2] = (t1, 2)
;; run_test "simplest_tree [t1; t2]" test

let test () : bool =
  let t1 = LNode (LLeaf [G; T], [A; T],
                  LNode (LLeaf [T; T], [T; T], LLeaf [C; G])) in
  let t2 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t3 = LNode (LLeaf [G; T], [A; T],
                  LNode (LLeaf [T; T], [T; T], LLeaf [C; G])) in
  simplest_tree [t1; t2; t3] = (t2, 2)
;; run_test "simplest_tree [t1; t2; t3]" test

let test () : bool =
  let t1 = LNode (LLeaf [G; T], [A; T],
                  LNode (LLeaf [T; T], [T; T], LLeaf [C; G])) in
  let t2 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t3 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t4 = LNode (LLeaf [G; T], [A; T],
                  LNode (LLeaf [T; T], [T; T], LLeaf [C; G])) in
  simplest_tree [t1; t2; t3; t4] = (t2, 2)
;; run_test "simplest_tree [t1; t2; t3; t4]" test



(********************* simplest_greatest_ape_tree tests ***********************)

(* Refactoring is the process of rewriting your code to make it simpler and
   more general without changing its behavior. Therefore, it is easy to write
   test cases for the new version---they should compute the same answers
   that we did before. *)

let test () : bool =
  find_simplest_tree gorilla human chimpanzee orangutan =
    simplest_greater_ape_tree ()
;; run_test "simplest_greater_ape_tree" test

let test () : bool =
  find_simplest_tree lar_gibbon pileated_gibbon siamang white_cheeked_gibbon =
    simplest_lesser_ape_tree ()
;; run_test "simplest_lesser_ape_tree" test

let test () : bool =
  find_simplest_tree [G] [] [] [] = Node (Leaf [], Leaf [])
;; run_failing_test "simplest_half_empty_tree" test

let test () : bool =
  find_simplest_tree [G; T] [A; T] [] [] = Node (Leaf [], Leaf [])
;; run_failing_test "simplest_other_half_empty_tree" test

;; print_endline "dna.ml: ran to completion."
