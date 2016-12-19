open OUnit2

(* Open all custom modules. *)
open Exercise2

let tests = 
  ["suffixes" >:: (
      fun _ ->
        begin
          assert_equal [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []] (suffixes [1;2;3;4])
        end
    );
  ]


let suite = "chatper 2 tests suite" >::: tests

let _ = run_test_tt_main suite
