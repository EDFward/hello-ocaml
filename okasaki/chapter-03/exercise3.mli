exception Invalid_argument
module type Ordered =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end
module type Heap =
sig
  type t
  type elt
  val empty : t
  val is_empty : t -> bool
  val insert : t -> elt -> t
  val merge : t -> t -> t
  val find_min : t -> elt
  val delete_min : t -> t
end
module type LeftistTreeHeap = functor (E : Ordered) -> Heap
type 'a leftist_tree = E | T of int * 'a leftist_tree * 'a * 'a leftist_tree
module LeftistTreeHeap :
  functor (E : Ordered) ->
  sig
    type elt = E.t
    type t = elt leftist_tree
    val empty : 'a leftist_tree
    val is_empty : 'a leftist_tree -> bool
    val merge : E.t leftist_tree -> E.t leftist_tree -> E.t leftist_tree
    val insert : E.t leftist_tree -> E.t -> E.t leftist_tree
    val find_min : 'a leftist_tree -> 'a
    val delete_min : E.t leftist_tree -> E.t leftist_tree
  end
