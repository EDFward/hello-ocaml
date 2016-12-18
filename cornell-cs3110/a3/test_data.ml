open OUnit2
open Data

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc)
  in aux j []

module type Tests = sig
  val tests : OUnit2.test list
end

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M : DictionaryMaker) = struct

  module IntComparble = struct
    type t = int 
    let compare a b = match compare a b with
      | -1 -> `LT
      | 1 -> `GT
      | _ -> `EQ
    let format fmt = Format.fprintf fmt "%d"
  end


  module IntDict = M(IntComparble)

  let sample_dict = 
    List.fold_left (fun acc i -> IntDict.insert i i acc) IntDict.empty (1--100)

  let tests = let open IntDict in [
      "rep_ok" >:: (
        fun _ ->
          begin
            assert_equal empty (rep_ok empty);
            assert_equal sample_dict (rep_ok sample_dict);
          end
      );
      "empty dict" >:: (
        fun _ ->
          begin
            assert_equal true (is_empty empty);
            assert_equal 0 (size empty);
          end
      );
      "insert" >:: (
        fun _ -> 
          begin
            assert_equal 100 (size sample_dict);
            assert_equal 101 (size (insert 101 101 sample_dict));
            assert_equal 100 (size (insert 10 10 sample_dict));
          end
      );
      "member" >:: (
        fun _ ->
          begin 
            assert_equal true (member 50 sample_dict);
            assert_equal false (member 101 sample_dict);
            let d = insert 101 101 sample_dict in assert_equal true (member 101 d);
          end
      );
      "find" >:: (
        fun _ ->
          begin
            assert_equal (Some 42) (find 42 sample_dict);
            assert_equal None (find 42 empty);
            assert_equal None (find 101 sample_dict);
            assert_equal (Some 42) (find 101 (insert 101 42 sample_dict));
            assert_equal (Some 42) (find 1 (insert 1 42 sample_dict));
          end
      );
      "remove" >:: (
        fun _ -> let d1 = remove 42 sample_dict and d2 = remove 101 sample_dict in
          begin
            assert_equal None (find 42 d1);
            assert_equal 99 (size d1);
            assert_equal 100 (size d2);
            assert_equal true (is_empty (remove 42 empty));
          end
      );
      "choose" >:: (
        fun _ ->
          begin
            assert_equal None (choose empty);
            match choose sample_dict with
            | None        -> assert_failure "choose from sample_dict should return value"
            | Some (k, v) -> assert_equal (Some v) (find k sample_dict)
          end
      );
      "to_list" >:: (
        fun _ -> let d = insert 11 11 (insert 30 30 (insert 2 2 empty)) in
          begin
            assert_equal (to_list d) [(2, 2); (11, 11); (30, 30)];
            assert_equal (to_list sample_dict) (List.map (fun i -> (i, i)) (1--100));
          end
      );
      "fold" >:: (
        fun _ ->
          begin let d = insert 2 2 (insert 1 1 empty) in
            assert_equal (fold (fun k _ acc -> k - acc) 0 d) 1;
          end
      );
    ]
end

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)

module ListDictTester = DictTester(MakeListDictionary)

let tests = ListDictTester.tests

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)
