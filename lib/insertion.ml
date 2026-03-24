(* Insertion sort *)

(* Steps:
  1. Start with the second element (index 1). The first element by itself is
     already "sorted."
  2. Take the current element (the "key") and compare it with elements to
     its left.
  3. Shift each left-side element that is greater than the key one position
     to the right.
  4. Insert the key into the gap created.
  5. Advance to the next element and repeat until the end of the array.
*)

let rec insert x = function
  | [] -> [ x ]
  | hd :: tl -> if x <= hd then x :: hd :: tl else hd :: insert x tl

let rec insertion_sort = function
  | [] -> []
  | hd :: tl -> insert hd (insertion_sort tl)
