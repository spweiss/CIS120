


type 'a deque 

val create : unit -> 'a deque
val is_empty : 'a deque -> bool



(* add to the front of the deque *)
val insert_head : 'a -> 'a deque -> unit  

(* add to the back of the deque *)
val insert_tail : 'a -> 'a deque -> unit  

(* remove the first element from the deque and return it *)
(* fail if the queue is empty *)
val remove_head : 'a deque -> 'a  

(* remove the last element from the deque and return it *)
(* fail if the queue is empty *)
val remove_tail : 'a deque -> 'a   

(* return a list of queue elements, ordered from the 
   head to the tail *)
val to_list : 'a deque -> 'a list

(* delete the first occurence (if any) of an element 
   in the deque; first means closest to the head *)
val delete_first : 'a -> 'a deque -> unit

(* delete the last occurrence (if any) of an element
   in the deque; last means closest to the tail *)
val delete_last : 'a -> 'a deque -> unit 

(* mutate the deque to reverse the order of its elements *)
val reverse : 'a deque -> unit 

