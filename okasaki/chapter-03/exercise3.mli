exception Invalid_argument
exception Unimplemented

module type Ordered =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module type Heap =
sig
  module Element : Ordered
  type elt = Element.t
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : t -> elt -> t
  val merge : t -> t -> t
  val find_min : t -> elt
  val delete_min : t -> t

  (* Optional functions. *)
  val non_merge_insert : t -> elt -> t
  val from_list_naive : elt list -> t
  val from_list_arr : elt list -> t
  val from_list: elt list -> t
end

module type HeapMaker =
  functor (E : Ordered) -> Heap with module Element = E

module type HeapEnhancer =
  functor (H : Heap) -> Heap with module Element = H.Element

type 'a leftist_tree = E | T of int * 'a leftist_tree * 'a * 'a leftist_tree

type 'a binomial_tree = Node of 'a * 'a binomial_tree list

module LeftistHeap : HeapMaker

module WeightBiasedLeftistHeap : HeapMaker

module BinomialHeap : HeapMaker

module ExplicitMin : HeapEnhancer
