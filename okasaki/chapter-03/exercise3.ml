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
  val delete_min: t -> t
end

module type LeftistTreeHeap = functor (E : Ordered) -> Heap

type 'a leftist_tree = E | T of int * ('a leftist_tree) * 'a * ('a leftist_tree)

module LeftistTreeHeap(E: Ordered) =
struct
  type elt = E.t
  type t = elt leftist_tree

  let empty = E
  let is_empty h = E == h

  let rank = function
    | E             -> 0
    | T(r, _, _, _) -> r

  (* Helper function to make a tree which properly updated the rank. *)
  let make v h1 h2 = let r1 = rank h1 and r2 = rank h2 in
    if r1 < r2 then T(r1+1, h2, v, h1)
    else T(r2+1, h1, v, h2)

  let rec merge h1 h2 = match (h1, h2) with
    | (E, h)                                 -> h
    | (h, E)                                 -> h
    | (T(r1, a1, v1, b1), T(r2, a2, v2, b2)) ->
      if E.lt v1 v2 then
        make v1 a1 (merge b1 h2)
      else
        make v2 a2 (merge h1 b2)

  let insert h v = merge h (T(1, E, v, E))

  let find_min = function
    | E                -> raise Invalid_argument
    | T(_, _, v, _)    -> v

  let delete_min = function
    | E -> raise Invalid_argument
    | T(_, a, _, b) -> merge a b

  (* Exercise 3.2:
   * Define 'insert' directly. *)
  let rec non_merge_insert h v = match h with
    | E              -> T(1, E, v, E)
    | T(r, a, v', b) ->
      if E.lt v v' then T(1, h, v, E)
      else make v' a (non_merge_insert b v)

  let from_list_naive = List.fold_left (fun acc i -> insert acc i) empty

  (* Exercise 3.3:
   * Implement `from_list` in (log n) passes where each pass merges adjacent
   * pairs of heaps, which takes O(n) time. *)
  let from_list = function
    | [] -> E
    | ls ->
      let arr = Array.of_list ls in
      let rec aux i j =
        if i+1 == j then T(1, E, Array.get arr i, E)
        else let mid = (i+j)/2 in merge (aux i mid) (aux mid j) in
      aux 0 (Array.length arr)
end
