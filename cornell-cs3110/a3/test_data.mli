(* [Tests] is the output type of the testing functors below. *)
module type Tests = sig
  (* A list of OUnit tests. *)
  val tests : OUnit2.test list
end

(* [DictTester] takes a [DictionaryMaker], uses it to make
 * a dictionary, and returns OUnit tests for that dictionary. *)
module DictTester (M:Data.DictionaryMaker) : Tests

(* The [tests] value declared by this include should
 * contain test cases for all the data structure
 * implementations you write in [Data]:
 *  - dictionaries as association lists
 *  - dictionaries as 2-3 trees
 *  - sets as dictionaries
 * These tests should be constructed by applying
 * the testing functors declared above. *)
include Tests