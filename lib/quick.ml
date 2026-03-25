(* Quick sort *)

(* Steps:
  1. *Choose a pivot*: Pick an element from the array (common choices:
     first, last, middle, or random element).
  2. *Partition*: Rearrange the array so that:
     - All elements less than the pivot are to its left.
     - All elements greater than the pivot are to its right.
     - The pivot is now in its final sorted position.
  3. *Recurse*: Recursively apply quick sort to the left sub-array and the
     right sub-array.
  4. *Base case*: An array of length 0 or 1 is already sorted.

  ** The Partition Step (Lomuto scheme)

  1. Choose the last element as the pivot.
  2. Maintain an index =i= pointing to the position where the next element
     smaller than the pivot should go (starts before the first element).
  3. Scan from left to right. Whenever an element ≤ pivot is found, increment
     =i= and swap the element with the element at =i=.
  4. After the scan, swap the pivot (last element) with the element at =i+1=.
  5. Return =i+1= as the pivot's final index.
*)

let rec quick_sort = function
  | [] -> []
  | [ x ] -> [ x ]
  | lst ->
      let pivot = List.nth lst (List.length lst / 2) in
      let left = List.filter (fun x -> x < pivot) lst in
      let middle = List.filter (fun x -> x = pivot) lst in
      let right = List.filter (fun x -> x > pivot) lst in
      quick_sort left @ middle @ quick_sort right
