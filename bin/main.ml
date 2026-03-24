(*let print_array arr =
  let strs = Array.map string_of_int arr in
  Printf.printf "[| %s |]\n" (String.concat "; " (Array.to_list strs))*)

let print_nums (arr : int list) =
  let strs = List.map string_of_int arr in
  Printf.printf "[ %s ]\n" (String.concat "; " strs)

let () =
  let arr = [ 5; 3; 8; 2; 1; 9; 4; 6; 7 ] in
  (*let arr = [| 5; 3; 8; 2; 1; 9; 4; 6; 7 |] in*)
  Printf.printf "Bubble sort: ";
  let sorted = Sorting_algos.Bubble.bubble_sort arr in
  (*print_array sorted*)
  print_nums sorted;

  Printf.printf "Selection sort: ";
  let sorted = Sorting_algos.Selection.selection_sort arr in
  print_nums sorted
