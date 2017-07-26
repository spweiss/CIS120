(* You DO NOT need to modify this file. *)

let file = open_in "books.txt"

let rec goto_line (key : string) : unit =
  if input_line file = key then () else goto_line key

let words_of_line () : string list =
  let line = input_line file in
  let length = String.length line in
  let rec parse_line (idx : int) : string list =
    let loc =
      try Some (String.index_from line idx ' ')
      with Not_found -> None
    in begin match loc with
    | None -> [String.sub line idx (length - idx)]
    | Some idx' -> (String.sub line idx (idx' - idx)) :: parse_line (idx' + 1)
    end
  in if length = 0 then [] else parse_line 0

let read_book () : string list =
  let rec read_lines (l : string list) : string list =
    begin match words_of_line () with
    | [] -> l
    | l' -> read_lines (l' @ l)
    end
  in read_lines []

let modest  = goto_line "(* MODEST *)";  read_book ()
let weak    = goto_line "(* WEAK *)";    read_book ()
let twentyk = goto_line "(* TWENTYK *)"; read_book ()

;; close_in file
