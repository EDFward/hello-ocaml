(* Write a function of type a list -> a list list that takes a list xs and
 * returns a list of all the suffixes of xs in decreasing order of length. *)

exception Unimplemented

let suffixes ls =
  let rec aux acc = function
    | []          -> List.rev ([]::acc)
    | _::tl as ls -> aux (ls::acc) tl
  in
  aux [] ls
