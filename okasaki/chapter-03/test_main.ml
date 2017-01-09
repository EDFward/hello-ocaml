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
    if n < i then acc else aux (n-1) (n::acc) in
  aux j []

let make_random_range i j = List.sort
    (fun _ _ -> (Random.int 3) - 1) (make_range i j)

let create_randomly_inserted_heap by_merge i j = let open IntLeftistTreeHeap in
  let insert_func = if by_merge then insert else non_merge_insert in
  let random_range = make_random_range i j in
  List.fold_left (fun acc i -> insert_func acc i) empty random_range

let tests =
  let open IntLeftistTreeHeap in
  let rec test_min_equal_of_range i = function
    | E -> ()
    | h -> (
        assert_equal i (find_min h);
        test_min_equal_of_range (i+1) (delete_min h);
      ) in [
    "empty" >:: (
      fun _ ->
        assert_equal true (is_empty empty);
        assert_equal false (is_empty (insert empty 1));
    );
    "insert, find and delete" >:: (
      fun _ ->
        test_min_equal_of_range 1 (create_randomly_inserted_heap true 1 10000);
        test_min_equal_of_range 1 (create_randomly_inserted_heap false 1 10000);
    );
    "from_list" >:: (
      fun _ ->
        let open Landmark in
        let profile_from_list_naive = register "profile_from_list_naive" in
        let profile_from_list = register "profile_from_list" in
        let random_range = make_random_range 1 10000 in (
          start_profiling ();
          test_min_equal_of_range 1 (
            enter profile_from_list_naive;
            let r = from_list_naive random_range in (
              exit profile_from_list_naive;
              r
            )
          );
          test_min_equal_of_range 1 (
            enter profile_from_list;
            let r = from_list random_range in (
              exit profile_from_list;
              r
            )
          );
        )
    );
  ]

let suite = "chatper 3 tests suite" >::: tests

let _ = run_test_tt_main suite
