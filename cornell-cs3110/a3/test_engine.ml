open OUnit2
open Engine

module EngineTester(E : Engine) = struct

  let idx = E.index_of_dir "test1"

  let tests = [
    "index_of_dir" >:: (
      fun _ ->
        begin
          assert_raises Not_found (fun () -> E.index_of_dir "wat");
          assert_equal idx (E.index_of_dir "test1");
        end
    );
    "and_not" >:: (
      fun _ ->
        begin
          assert_equal [] (E.and_not idx ["then"] []);
          assert_equal ["test1/medium.txt"] (E.and_not idx ["the"; "and"] []);
          assert_equal ["test1/medium.txt"; "test1/small.txt"] (E.and_not idx ["the"] []);
          assert_equal ["test1/small.txt"] (E.and_not idx ["the"] ["and"]);
        end
    );
    "or_not" >:: (
      fun _ ->
        begin
          assert_equal [] (E.or_not idx ["then"] []);
          assert_equal ["test1/medium.txt"] (E.or_not idx ["and"] []);
          assert_equal ["test1/medium.txt"; "test1/small.txt"] (E.or_not idx ["and"; "people"] []);
          assert_equal ["test1/medium.txt"; "test1/small.txt"] (E.or_not idx ["the"; "and"] []);
          assert_equal ["test1/medium.txt"; "test1/small.txt"] (E.or_not idx ["the"] []);
          assert_equal ["test1/small.txt"] (E.or_not idx ["the"] ["and"]);
          assert_equal [] (E.or_not idx ["and"; "people"] ["the"]);
        end
    );
  ]

end

module ListEngineTester = EngineTester(ListEngine)
module TreeEngineTester = EngineTester(TreeEngine)

let tests = List.append ListEngineTester.tests TreeEngineTester.tests
