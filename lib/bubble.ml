(* Bubble sort *)

(* Steps: 
  1. Start at the beginning of the array.
  2. Compare each pair of adjacent elements.
  3. If the left element is greater than the right element, swap them.
  4. After one full pass through the array, the largest unsorted element is now
     in its correct final position at the end.
  5. Repeat the process for the remaining unsorted portion of the array (one
     fewer element each time, since the end is already sorted).
  6. Stop when a complete pass is made with no swaps — the array is sorted.
*)

let rec bubble_sort lst =
  let sorted = function
    | hd1 :: hd2 :: tl when hd1 > hd2 -> hd2 :: bubble_sort (hd1 :: tl)
    | hd :: tl -> hd :: bubble_sort tl
    | x -> x
  in
  let sorted_lst = sorted lst in
  if lst = sorted_lst then lst else bubble_sort sorted_lst
