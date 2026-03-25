let print_nums (lst : int list) =
  let strs = List.map string_of_int lst in
  Printf.printf "[ %s ]\n" (String.concat "; " strs)

let () =
  let lst = [ 5; 3; 8; 2; 1; 9; 4; 6; 7 ] in
  Printf.printf "Bubble sort: ";
  let sorted = Sorting_algos.Bubble.bubble_sort lst in
  print_nums sorted;

  Printf.printf "Selection sort: ";
  let sorted = Sorting_algos.Selection.selection_sort lst in
  print_nums sorted;

  Printf.printf "Insertion sort: ";
  let sorted = Sorting_algos.Insertion.insertion_sort lst in
  print_nums sorted;

  Printf.printf "Merge sort: ";
  let sorted = Sorting_algos.Merge.merge_sort lst in
  print_nums sorted;

  Printf.printf "Quick sort: ";
  let sorted = Sorting_algos.Quick.quick_sort lst in
  print_nums sorted;

  Printf.printf "Heap sort: ";
  let sorted = Sorting_algos.Heap.heap_sort lst in
  print_nums sorted
