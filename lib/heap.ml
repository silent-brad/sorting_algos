(* Heapsort *)

(* Steps
  1. *Build a max-heap*: Rearrange the array so it satisfies the max-heap
     property. Start from the last non-leaf node and "heapify" (sift down)
     each node.
  2. *Extract max repeatedly*:
     a. Swap the root (largest element) with the last element of the heap.
     b. Reduce the heap size by one (the last element is now sorted).
     c. Heapify the root to restore the max-heap property.
     d. Repeat until the heap size is 1.

  ** The Heapify (Sift Down) Procedure

  Given a node at index =i= that may violate the heap property:
  1. Compare the node with its left and right children.
  2. Find the largest among the three.
  3. If the largest is not the node itself, swap the node with the largest
     child and recursively heapify at the child's position.
*)

let rec heap_sort = function
  | [] -> []
  | [ x ] -> [ x ]
  | lst ->
      (* Heapsort requires O(1) random access to elements by index to calculate
         parent/child positions (i, 2i+1, 2i+2). Lists are linked structures with
         O(n) access time, making a list-based heapsort O(n² log n)—impractical.
         Arrays provide O(1) random access, so we convert for efficiency. *)
      let arr = Array.of_list lst in
      let n = Array.length arr in

      let swap i j =
        let temp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- temp
      in

      let rec heapify heap_size i =
        let left = (2 * i) + 1 in
        let right = (2 * i) + 2 in
        let largest = ref i in

        if left < heap_size && arr.(left) > arr.(!largest) then largest := left;

        if right < heap_size && arr.(right) > arr.(!largest) then
          largest := right;

        if !largest != i then begin
          swap i !largest;
          heapify heap_size !largest
        end
      in

      let rec build_heap i =
        if i >= 0 then begin
          heapify n i;
          build_heap (i - 1)
        end
      in

      let rec extract i =
        if i > 0 then begin
          swap 0 i;
          heapify i 0;
          extract (i - 1)
        end
      in

      build_heap ((n / 2) - 1);
      extract (n - 1);
      Array.to_list arr
