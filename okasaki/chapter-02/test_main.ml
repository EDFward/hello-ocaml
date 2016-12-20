open OUnit2

open Data
open Exercise2

module StringOrdered = struct
  type t = string
  let eq = String.equal
  let lt s1 s2 = String.compare s1 s2 = -1
  let leq s1 s2 = String.compare s1 s2 != 1
end

module StringSet = OptimizedSet(StringOrdered)

let _ = Random.self_init ()

(* Range operator. *)
let make_range i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc)
  in aux j []

let range = make_range 1 100
let shuffle = List.sort (fun _ _ -> (Random.int 3) - 1)
let random_range = shuffle range
let make_set_from_int_list range = 
  List.fold_left (fun acc i -> StringSet.insert i acc) StringSet.empty
    (List.map string_of_int range)
let s1 = make_set_from_int_list range
let s2 = make_set_from_int_list random_range

let tests =
  ["suffixes" >:: (
      fun _ ->
        begin
          assert_equal [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []] (suffixes [1;2;3;4]);
        end
    );
   "optimized set" >:: (
     fun _ -> let open StringSet in
       begin
         assert_equal false (member "42" empty);
         assert_equal true (member "42" (insert "42" empty));
         assert_equal true (member "42" s1);
         assert_equal false (member "101" s1);
         assert_equal true (member "42" s2);
         assert_equal false (member "101" s2);
       end
   );
  ]


let suite = "chatper 2 tests suite" >::: tests

let _ = run_test_tt_main suite
