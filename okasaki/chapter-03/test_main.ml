open OUnit2

open Exercise3

module IntOrdered = struct
  type t = int
  let eq = (=)
  let lt s1 s2 = compare s1 s2 = -1
  let leq s1 s2 = compare s1 s2 != 1
end

module IntLeftistTreeHeap = LeftistTreeHeap(IntOrdered)

let _ = Random.self_init ()

(* Range operator. *)
let make_range i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc)
  in aux j []

let create_randomly_inserted_heap i j = let open IntLeftistTreeHeap in
  let
    random_range = List.sort (fun _ _ -> (Random.int 3) - 1) (make_range i j)
  in
  List.fold_left (fun acc i -> insert acc i) empty random_range

let tests = let open IntLeftistTreeHeap in [
    "empty" >:: (
      fun _ ->
        begin
          assert_equal true (is_empty empty);
          assert_equal false (is_empty (insert empty 1));
        end
    );
    "insert, find and delete" >:: (
      fun _ -> let open IntLeftistTreeHeap in
        let rec aux i = function
          | E -> ()
          | h -> (
              assert_equal i (find_min h);
              aux (i+1) (delete_min h);
            )
        in
        aux 1 (create_randomly_inserted_heap 1 100000)
    );
  ]

let suite = "chatper 3 tests suite" >::: tests

let _ = run_test_tt_main suite
