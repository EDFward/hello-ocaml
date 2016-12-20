val suffixes : 'a list -> 'a list list

module type OptimizedSet = functor (E : Data.Ordered) -> Data.Set
module OptimizedSet :
  functor (E : Data.Ordered) ->
    sig
      type elt = E.t
      type tree = E | T of tree * elt * tree
      type t = tree
      val empty : tree
      val member : E.t -> tree -> bool
      val insert : elt -> tree -> tree
    end
