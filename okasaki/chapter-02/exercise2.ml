exception Unimplemented

(* Exercise 2.1:
 * Write a function of type a list -> a list list that takes a list xs and
 * returns a list of all the suffixes of xs in decreasing order of length. *)

let suffixes ls =
  let rec aux acc = function
    | []          -> List.rev ([]::acc)
    | _::tl as ls -> aux (ls::acc) tl
  in
  aux [] ls


(* Exercise 2.2 - 2.4:
 * Optimize unbalanced tree set to have following properties:
 * 1. Instead of 2d comparisons (d is the tree depth), do no more than d + 1;
 * 2. Avoid unnecessary copying when inserting existing elements.
 *
 * The traditional implementation is in `data.ml`. *)

type 'a tree = E | T of ('a tree) * 'a * ('a tree)

module type OptimizedSet = functor (E : Data.Ordered) -> Data.Set

module OptimizedSet(E: Data.Ordered) =
struct
  type elt = E.t
  type t = elt tree

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

(* Exercise 2.5
 * Using the idea of sharing within a single object, write a function to
 * generate complete binary tree, AND
 * write a function to generate arbitrary size balanced tree.*)
let rec complete x d = if d = 0 then E else
  let
    subtree = complete x (d-1)
  in
    T(subtree, x, subtree)

let rec balanced_tree x n =
  (* Create a pair of trees, one of size m, another of m+1. *)
    let rec create2 = function
    | 0 -> (E, T(E, x, E))
    | n ->
      let (l, r) = create2 ((n-1)/2) in
      if n mod 2 = 0 then (T(l, x, r), T(r, x, r)) else (T(l, x, l), T(l, x, r))
  in create2 n |> fst

(*
 * More straight forward method, however no structure sharing happens.

let rec balanced_tree' x = function
  | 0 -> E
  | 1 -> T(E, x, E)
  | n -> T(balanced_tree x ((n-1)/2), x, balanced_tree x (n-1-(n-1)/2))

*)
