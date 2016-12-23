open OUnit2

open Data
open Exercise2

module StringOrdered = struct
  type t = string
  let eq = String.equal
  let lt s1 s2 = String.compare s1 s2 = -1
  let leq s1 s2 = String.compare s1 s2 != 1
end

module ReferenceStringSet = OptimizedSet(StringOrdered)
module OptimizedStringSet = OptimizedSet(StringOrdered)

let _ = Random.self_init ()

(* Range operator. *)
let make_range i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc)
  in aux j []

(* Test utilities for set. *)
let range = make_range 1 100
let shuffle = List.sort (fun _ _ -> (Random.int 3) - 1)
let random_range = shuffle range
let make_ref_set_from_int_list range = let open ReferenceStringSet in
  List.fold_left (fun acc i -> insert i acc) empty
    (List.map string_of_int range)
let make_opt_set_from_int_list range = let open OptimizedStringSet in
  List.fold_left (fun acc i -> insert i acc) empty
    (List.map string_of_int range)
let s1_ref = make_ref_set_from_int_list range
let s1_opt = make_opt_set_from_int_list range
let s2_ref = make_ref_set_from_int_list random_range
let s2_opt = make_opt_set_from_int_list random_range

(* Test utilities for tree stuff. *)
let rec is_complete = function
  | E          -> true
  | T(l, _, r) -> l = r && is_complete l && is_complete r
let ref_balanced_tree x = function (* Ref implementation. *)
  | 0 -> E
  | 1 -> T(E, x, E)
  | n -> T(balanced_tree x ((n-1)/2), x, balanced_tree x (n-1-(n-1)/2))

let tests =
  ["suffixes" >:: (
      fun _ ->
        begin
          assert_equal [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []] (suffixes [1;2;3;4]);
        end
    );
   "optimized set" >:: (
     fun _ -> let open OptimizedStringSet in
       begin
         assert_equal false (member "42" empty);
         assert_equal true (member "42" (insert "42" empty));
         assert_equal true (member "42" s1_opt);
         assert_equal false (member "101" s1_opt);
         assert_equal true (member "42" s2_opt);
         assert_equal false (member "101" s2_opt);
         assert_equal s1_opt s1_ref;
         assert_equal s2_opt s2_ref;
       end
   );
   "complete tree" >:: (
     fun _ ->
       begin
         assert_equal true (is_complete (complete "dummy" 0));
         assert_equal true (is_complete (complete "dummy" 10));
         assert_equal true (is_complete (complete "dummy" 20));
       end
   );
   "size balanced tree" >:: (
     fun _ ->
       let aux sz = let
         t1 = balanced_tree "dummy" sz and
         t2 = ref_balanced_tree "dummy" sz in
         t1 = t2
       in begin
         assert_equal true (aux 0);
         assert_equal true (aux 2);
         assert_equal true (aux 10);
         assert_equal true (aux 100);
         assert_equal true (aux 233);
       end
   );
  ]

let suite = "chatper 2 tests suite" >::: tests

let _ = run_test_tt_main suite
