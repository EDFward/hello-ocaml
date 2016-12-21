val suffixes : 'a list -> 'a list list

(* Binary tree. *)
type 'a tree = E | T of ('a tree) * 'a * ('a tree)

module type OptimizedSet = functor (E : Data.Ordered) -> Data.Set
module OptimizedSet :
  functor (E : Data.Ordered) ->
    sig
      type elt = E.t
      type t = elt tree
      val empty : t
      val member : E.t -> t -> bool
      val insert : elt -> t -> t
    end

(* Generate complete binary trees. *)
val complete : 'a -> int -> 'a tree

(* Generate balanced tree with arbitrary size. *)
val balanced_tree : 'a -> int -> 'a tree
