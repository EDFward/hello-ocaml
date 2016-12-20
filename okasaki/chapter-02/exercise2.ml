exception Unimplemented

(* Write a function of type a list -> a list list that takes a list xs and
 * returns a list of all the suffixes of xs in decreasing order of length. *)

let suffixes ls =
  let rec aux acc = function
    | []          -> List.rev ([]::acc)
    | _::tl as ls -> aux (ls::acc) tl
  in
  aux [] ls


(* Optimize unbalanced tree set to have following properties:
 * 1. Instead of 2d comparisons (d is the tree depth), do no more than d + 1;
 * 2. Avoid unnecessary copying when inserting existing elements.. 
 * 
 * The traditional implementation is in `data.ml`. *)

module type OptimizedSet = functor (E : Data.Ordered) -> Data.Set

module OptimizedSet(E: Data.Ordered) =
struct
  type elt = E.t
  type tree = E | T of tree * elt * tree
  type t = tree

  let empty = E

  (* Keep track of possibly equal element and only compare at the bottom. *)
  let member v = function
    | E                         -> false
    | T(_, root_ele, _) as root ->
      let rec aux candidate = function
        | E                 -> E.eq v candidate
        | T(left, e, right) -> (* `e` should be the candidate. *)
          if E.lt v e then aux candidate left else aux e right
      in
      aux root_ele root

  exception Found

  let insert v = function
    | E                         -> T(E, v, E)
    | T(_, root_ele, _) as root ->
      let rec aux candidate = function
        | E when E.eq v candidate -> raise Found
        | E                       -> T(E, v, E)
        | T(left, e, right)       -> (
            if E.lt v e then T(aux candidate left, e, right)
            (* Update candidate. *)
            else T(left, e, aux e right)
          )
      in
      try
        aux root_ele root
      with
        Found -> root
end
