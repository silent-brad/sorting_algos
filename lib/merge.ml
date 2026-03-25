(* Merge sort *)

(* Steps:
  1. *Divide*: Split the array into two halves.
  2. *Conquer*: Recursively sort each half.
  3. *Combine*: Merge the two sorted halves into a single sorted array by
     comparing elements from each half and placing the smaller one first.

  ** The Merge Step (in detail)

  Given two sorted sub-arrays, the merge procedure:
  1. Maintain a pointer (index) into each sub-array, both starting at 0.
  2. Compare the elements at the two pointers.
  3. Copy the smaller element into the result array and advance that pointer.
  4. When one sub-array is exhausted, copy the remaining elements from the
     other.
*)

let rec merge_sort = function
  | [] -> []
  | [ x ] -> [ x ]
  | lst ->
      let rec split = function
        | [] -> ([], [])
        | [ x ] -> ([ x ], [])
        | x :: y :: zs ->
            let left, right = split zs in
            (x :: left, y :: right)
      in
      let rec merge left right acc =
        match (left, right) with
        | [], ys -> List.rev_append acc ys
        | xs, [] -> List.rev_append acc xs
        | x :: xs, y :: ys ->
            if x <= y then merge xs right (x :: acc)
            else merge left ys (y :: acc)
      in
      let left, right = split lst in
      merge (merge_sort left) (merge_sort right) []
