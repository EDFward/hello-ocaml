module type Ordered =
  sig
    type t
    val eq : t -> t -> bool
    val lt : t -> t -> bool
    val leq : t -> t -> bool
  end
module type Set =
  sig
    type t
    type elt
    val empty : t
    val insert : elt -> t -> t
    val member : elt -> t -> bool
  end
module type UnbalancedSet = functor (E : Ordered) -> Set
module UnbalancedSet :
  functor (E : Ordered) ->
    sig
      type elt = E.t
      type tree = E | T of tree * elt * tree
      type t = tree
      val empty : tree
      val member : E.t -> tree -> bool
      val insert : elt -> tree -> tree
    end
