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

type 'a leftist_tree = E | T of int * ('a leftist_tree) * 'a * ('a leftist_tree)

module type LeftistHeap = functor (E : Ordered) -> Heap

module LeftistHeap(E: Ordered) =
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

  let rec merge h1 h2 = match h1, h2 with
    | E, h                                 -> h
    | h, E                                 -> h
    | T(r1, a1, v1, b1), T(r2, a2, v2, b2) ->
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
   * pairs of heaps, which takes O(n) time in total. *)
  let from_list ls =
    let rec merge_pairs = function
      | []         -> []
      | h::[]      -> [h]
      | h1::h2::tl -> (merge h1 h2)::(merge_pairs tl) in
    let rec merge_until_one_left = function
      | []    -> E
      | h::[] -> h
      | ls    -> merge_pairs ls |> merge_until_one_left in
    merge_until_one_left (List.map (fun v -> T(1, E, v, E)) ls)

  (* Another kind of divide and conquer. *)
  let from_list_arr = function
    | [] -> E
    | ls ->
      let arr = Array.of_list ls in
      let rec aux i j =
        if i+1 == j then T(1, E, Array.get arr i, E)
        else let mid = (i+j)/2 in merge (aux i mid) (aux mid j) in
      aux 0 (Array.length arr)
end

(* Exercise 3.4:
 * Implement weight-biased leftist tree heap. *)
module type WeightBiasedLeftistHeap = functor (E : Ordered) -> Heap

module WeightBiasedLeftistHeap(E: Ordered) =
struct
  type elt = E.t
  type t = elt leftist_tree

  let empty = E
  let is_empty h = E == h

  let size = function
    | E             -> 0
    | T(s, _, _, _) -> s

  let find_min = function
    | E                -> raise Invalid_argument
    | T(_, _, v, _)    -> v

  let rec merge h1 h2 = match h1, h2 with
    | E, h                                         -> h
    | h, E                                         -> h
    | h1, h2 when E.lt (find_min h2) (find_min h1) -> merge h2 h1
    | T(s1, a1, v1, b1), T(s2, a2, v2, b2)         ->
      (* v1 is guaranteed to be smaller or equal. *)
      let s1' = size a1 and s2' = size b1 + size h2 in
      if s1' >= s2' then T(s1+s2+1, a1, v1, merge b1 h2)
      else T(s1+s2+1, merge b1 h2, v1, a1)

  let insert h v = merge h (T(1, E, v, E))

  let delete_min = function
    | E -> raise Invalid_argument
    | T(_, a, _, b) -> merge a b
end

(* Exercise 3.6:
 * Implement binomial heaps with new representation. *)
type 'a binomial_tree = Node of 'a * 'a binomial_tree list

module type BinomialHeap = functor (E : Ordered) -> Heap

module BinomialHeap(E: Ordered) =
struct
  type elt = E.t
  type t = (int * elt binomial_tree) list

  let empty = []
  let is_empty h = List.length h == 0

  (* Two trees should have the same rank. *)
  let link tr1 tr2 = match tr1, tr2 with
    | Node(v1, ls1), Node(v2, ls2) ->
      if E.lt v1 v2 then Node(v1, tr2::ls1)
      else Node(v2, tr1::ls2)

  let rec insert_tree tr rank = function
    | []                     -> [(rank, tr)]
    | (rank', tr') as hd::tl ->
      if rank' != rank then hd::(insert_tree tr rank tl)
      else insert_tree (link tr tr') (rank+1) tl

  let insert h v = insert_tree (Node(v, [])) 0 h

  let rec merge h1 h2 = match h1, h2 with
    | [], h                                            -> h
    | h, []                                            -> h
    | ((r1, tr1) as hd1::tl1), ((r2, tr2) as hd2::tl2) ->
      if r1 < r2 then hd1::(merge tl1 h2)
      else if r2 < r1 then hd2::(merge h1 tl2)
      else insert_tree (link tr1 tr2) (r1+1) (merge tl1 tl2)

  let get_val = function
    | Node(v, _) -> v

  (* Helper function to get the node with minimum value and rest of the list. *)
  let rec split_by_min = function
    | [] -> raise Invalid_argument
    | [pair] -> (pair, [])
    | (_, Node(v, _)) as hd::tl -> match split_by_min tl with
      | (_, Node(v', _)) as min, rest ->
        if v < v' then (hd, min::rest) else (min, hd::rest)

  let find_min h = match split_by_min h |> fst with
    | (_, Node(v, _)) -> v

  let delete_min h = match split_by_min h with
    | (_, Node(_, children)), rest ->
      let without_min = List.mapi (fun i v -> (i, v)) (List.rev children) in
      merge without_min rest

end
