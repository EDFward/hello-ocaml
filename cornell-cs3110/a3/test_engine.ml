open OUnit2
open Engine

let idx = ListEngine.index_of_dir "test1"

let tests = [
  "index_of_dir" >:: (
    fun _ ->
      begin
        assert_raises Not_found (fun () -> ListEngine.index_of_dir "wat");
        assert_equal idx (ListEngine.index_of_dir "test1");
      end
  );
  "and_not" >:: (
    fun _ ->
      begin
        assert_equal [] (ListEngine.and_not idx ["then"] []);
        assert_equal ["test1/medium.txt"] (ListEngine.and_not idx ["the"; "and"] []);
        assert_equal ["test1/medium.txt"; "test1/small.txt"] (ListEngine.and_not idx ["the"] []);
        assert_equal ["test1/small.txt"] (ListEngine.and_not idx ["the"] ["and"]);
      end
  );
  "or_not" >:: (
    fun _ ->
      begin
        assert_equal [] (ListEngine.or_not idx ["then"] []);
        assert_equal ["test1/medium.txt"] (ListEngine.or_not idx ["and"] []);
        assert_equal ["test1/medium.txt"; "test1/small.txt"] (ListEngine.or_not idx ["and"; "people"] []);
        assert_equal ["test1/medium.txt"; "test1/small.txt"] (ListEngine.or_not idx ["the"; "and"] []);
        assert_equal ["test1/medium.txt"; "test1/small.txt"] (ListEngine.or_not idx ["the"] []);
        assert_equal ["test1/small.txt"] (ListEngine.or_not idx ["the"] ["and"]);
        assert_equal [] (ListEngine.or_not idx ["and"; "people"] ["the"]);
      end
  );
]
