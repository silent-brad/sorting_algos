(* Merge sort *)

(* Steps:

*)

let rec merge_sort = function
  | [] -> []
  | hd :: tl -> merge_sort (merge_sort tl)
