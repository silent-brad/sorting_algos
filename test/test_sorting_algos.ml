let test_cases =
  [
    ("empty", [], []);
    ("single element", [ 1 ], [ 1 ]);
    ("already sorted", [ 1; 2; 3; 4; 5 ], [ 1; 2; 3; 4; 5 ]);
    ("reverse sorted", [ 5; 4; 3; 2; 1 ], [ 1; 2; 3; 4; 5 ]);
    ("duplicates", [ 3; 1; 2; 3; 1 ], [ 1; 1; 2; 3; 3 ]);
    ("all same", [ 7; 7; 7; 7 ], [ 7; 7; 7; 7 ]);
    ("two elements", [ 2; 1 ], [ 1; 2 ]);
    ("negative numbers", [ -3; 0; -1; 5; 2 ], [ -3; -1; 0; 2; 5 ]);
    ( "larger array",
      [ 5; 3; 8; 2; 1; 9; 4; 6; 7 ],
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] );
  ]

let run_tests algo_name sort_fn =
  List.iter
    (fun (name, input, expected) ->
      let result = sort_fn input in
      if List.equal Int.equal result expected then
        Printf.printf "  PASS: %s — %s\n" algo_name name
      else (
        Printf.printf "  FAIL: %s — %s\n" algo_name name;
        Printf.printf "    input:    [ %s ]\n"
          (String.concat "; " (List.map string_of_int input));
        Printf.printf "    expected: [ %s ]\n"
          (String.concat "; " (List.map string_of_int expected));
        Printf.printf "    got:      [ %s ]\n"
          (String.concat "; " (List.map string_of_int result))))
    test_cases

let () =
  Printf.printf "Running sorting algorithm tests...\n\n";

  (* Bubble sort *)
  Printf.printf "Bubble Sort:\n";
  run_tests "bubble_sort" Sorting_algos.Bubble.bubble_sort;
  print_newline ();

  Printf.printf "Selection Sort:\n";
  run_tests "selection_sort" Sorting_algos.Selection.selection_sort;
  print_newline ();

  Printf.printf "Insertion Sort:\n";
  run_tests "insertion_sort" Sorting_algos.Insertion.insertion_sort;
  print_newline ();

  Printf.printf "Merge Sort:\n";
  run_tests "merge_sort" Sorting_algos.Merge.merge_sort;
  print_newline ()

(* Printf.printf "Quick Sort:\n";
     run_tests "quick_sort" Sorting_algos.Quick.quick_sort;
     print_newline () *)

(* Printf.printf "Heap Sort:\n";
     run_tests "heap_sort" Sorting_algos.Heap.heap_sort;
     print_newline () *)
