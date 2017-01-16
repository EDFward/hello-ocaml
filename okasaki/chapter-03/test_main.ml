open OUnit2

open Exercise3

module IntOrdered = struct
  type t = int
  let eq = (=)
  let lt s1 s2 = compare s1 s2 = -1
  let leq s1 s2 = compare s1 s2 != 1
end

module IntLeftistHeap = LeftistHeap(IntOrdered)

module IntWeightBiasedLeftistHeap = WeightBiasedLeftistHeap(IntOrdered)

module IntBinomialHeap = BinomialHeap(IntOrdered)

module IntExplicitMinBinomialHeap = ExplicitMin(IntBinomialHeap)

module IntRedBlackTreeSet = RedBlackTreeSet(IntOrdered)

let _ = Random.self_init ()

(* Range operator. *)
let make_range i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc) in
  aux j []

let make_random_range i j = List.sort
    (fun _ _ -> (Random.int 3) - 1) (make_range i j)

let tests_for_leftist_heap =
  let open IntLeftistHeap in
  let rec test_leftist i h =
    if is_empty h then () else (
      assert_equal i (find_min h);
      test_leftist (i+1) (delete_min h);
    ) in
  [
    "empty" >:: (
      fun _ ->
        assert_equal true (is_empty empty);
        assert_equal false (is_empty (insert empty 1));
    );
    "insert, find and delete" >:: (
      fun _ ->
        let open List in
        let range = make_random_range 1 10000 in
        let h1 = fold_left (fun acc i -> non_merge_insert acc i) empty range in
        let h2 = fold_left (fun acc i -> insert acc i) empty range in (
          test_leftist 1 h1;
          test_leftist 1 h2;
        )
    );
    "from_list" >:: (
      fun _ ->
        let open Landmark in
        let profile_from_list_naive = register "profile_from_list_naive" in
        let profile_from_list = register "profile_from_list" in
        let profile_from_list_arr = register "profile_from_list_arr" in
        let random_range = make_random_range 1 10000 in (
          start_profiling ();
          List.iter (
            fun (label, f) ->
              test_leftist 1 (
                enter label;
                let r = f random_range in (
                  exit label;
                  r
                )
              )
          ) [
            (profile_from_list_naive, from_list_naive);
            (profile_from_list_arr, from_list_arr);
            (profile_from_list, from_list);
          ]
        )
    );
  ]

let tests_for_weight_biased_leftist_heap =
  let open IntWeightBiasedLeftistHeap in
  let rec test_weight_biased_leftist i h =
    if is_empty h then () else (
      assert_equal i (find_min h);
      test_weight_biased_leftist (i+1) (delete_min h);
    ) in
  [
    "weight-biased leftist tree heap" >:: (
      fun _ ->
        let range = make_random_range 1 10000 in
        let h = List.fold_left (fun acc i -> insert acc i) empty range in
        test_weight_biased_leftist 1 h
    );
  ]

let tests_for_binomial_heap =
  let open IntBinomialHeap in
  let rec test_binomial i h =
    if is_empty h then () else (
      assert_equal i (find_min h);
      test_binomial (i+1) (delete_min h);
    ) in
  [
    "binomial heap" >:: (
      fun _ ->
        let range = make_random_range 1 1000 in
        let h = List.fold_left (fun acc i -> insert acc i) empty range in
        test_binomial 1 h
    );
  ]

let tests_for_explicit_min_heap =
  let open IntExplicitMinBinomialHeap in
  let rec test_explicit_min i h =
    if is_empty h then () else (
      assert_equal i (find_min h);
      test_explicit_min (i+1) (delete_min h);
    ) in
  [
    "explicit min heap" >:: (
      fun _ ->
        let range = make_random_range 1 1000 in
        let h = List.fold_left (fun acc i -> insert acc i) empty range in
        test_explicit_min 1 h
    );
  ]

let tests_for_red_black_tree_set =
  let open IntRedBlackTreeSet in
  let range = make_range 1 50 in
  let s = List.fold_left (fun acc i -> insert i acc |> rep_ok) empty range in
  [
    "insert and member" >:: (
      fun _ ->
        for i = 1 to 50 do
          assert_equal true (member i s);
          assert_equal s (insert i s);
        done;
        let s' = insert 100 s in (
          assert_equal false (s == s');
          assert_equal true (member 100 s');
          assert_equal false (member 100 s);
        );
    );
  ]

let tests = List.concat [
    tests_for_leftist_heap;
    tests_for_binomial_heap;
    tests_for_weight_biased_leftist_heap;
    tests_for_explicit_min_heap;
    tests_for_red_black_tree_set;
  ]
let suite = "chatper 3 tests suite" >::: tests

let _ = run_test_tt_main suite
