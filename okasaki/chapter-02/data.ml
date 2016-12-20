(* Signature. *)

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


(* Example implementation. *)

module UnbalancedSet(E: Ordered) = struct
  type elt = E.t
  type tree = E | T of tree * elt * tree
  type t = tree

  let empty = E

  let rec member v = function
    | E                 -> false
    | T(left, e, right) -> (
        if E.lt v e 
        then member v left 
        else if E.lt e v 
        then member v right 
        else true
      )

  let rec insert v = function
    | E                      -> T(E, v, E)
    | T(left, e, right) as s -> (
        if E.lt v e
        then T(insert v left, e, right)
        else if E.lt e v
        then T(left, e, insert v right)
        else s
      )
end
