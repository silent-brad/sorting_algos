(* Selection sort *)

(* Steps:
  1. Start with the entire array as the unsorted region.
  2. Find the minimum element in the unsorted region.
  3. Swap it with the first element of the unsorted region.
  4. The sorted region now grows by one element.
  5. Repeat steps 2–4 until the unsorted region is empty.
 *)

let rec selection_sort = function
  | [] -> []
  | x :: xs ->
      let rec find_min = function
        | [] -> x
        | x :: xs ->
            let min_of_rest = find_min xs in
            if x < min_of_rest then x else min_of_rest
      in
      let min = find_min xs in
      let rec remove_first y = function
        | [] -> []
        | hd :: tl -> if hd = y then tl else hd :: remove_first y tl
      in
      let sorted = remove_first min (x :: xs) in
      min :: selection_sort sorted
