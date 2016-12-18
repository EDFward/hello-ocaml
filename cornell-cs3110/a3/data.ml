exception Unimplemented

(* Take first n elements of a list. *)
let take (n: int) (ls: 'a list) = let open List in
  let rec aux acc = function
    | (_, [])           -> rev acc
    | (c, _) when c = n -> rev acc
    | (c, hd::tl)       -> aux (hd::acc) (c+1, tl)
  in aux [] (0, ls)

module type Comparable =
sig
  type t
  val compare : t -> t -> [ `EQ | `GT | `LT ]
  val format : Format.formatter -> t -> unit
end

module type Dictionary =
sig
  module Key : Comparable
  type key = Key.t
  type 'value t
  val rep_ok : 'value t  -> 'value t
  val empty : 'value t
  val is_empty : 'value t -> bool
  val size : 'value t -> int
  val insert : key -> 'value -> 'value t -> 'value t
  val member : key -> 'value t -> bool
  val find : key -> 'value t -> 'value option
  val remove : key -> 'value t -> 'value t
  val choose : 'value t -> (key * 'value) option
  val fold : (key -> 'value -> 'acc -> 'acc) -> 'acc -> 'value t -> 'acc
  val to_list : 'value t -> (key * 'value) list
  val format : (Format.formatter -> 'value -> unit)
    -> Format.formatter -> 'value t -> unit
end

module type DictionaryMaker =
  functor (C: Comparable) -> Dictionary with type Key.t = C.t

module MakeListDictionary (C: Comparable) = struct
  module Key = C
  type key = C.t

  type 'value t = (key * 'value) list

  (* Customized equality for key types. *)
  let (===) a b = Key.compare a b = `EQ

  let rep_ok d = 
    let _ = 
      List.fold_left 
        (fun acc (k, _) -> assert (not (List.mem k acc)); k::acc) [] d
    in d

  let empty = []

  let is_empty d = List.length d = 0

  let size = List.length

  let insert k v d = 
    let rec aux acc = function
      | []    -> (k, v)::acc
      | p::xs -> 
        if fst p === k then 
          (k, v)::(List.rev_append acc xs)
        else
          aux (p::acc) xs
    in aux [] d

  let remove k d =
    let rec aux acc = function
      | []                        -> acc
      | (k', _)::xs when k' === k -> List.rev_append acc xs
      | p::xs                     -> aux (p::acc) xs
    in aux [] d

  let find k d =
    try
      let (_, v) = List.find (fun (k', v) -> k === k') d in Some v
    with
      Not_found -> None

  let member k = List.exists (fun (k', _) -> k === k')

  let choose = function
    | []    -> None
    | p::xs -> Some p

  let to_list d =
    let compare (k1, _) (k2, _) = match Key.compare k1 k2 with
      | `LT -> -1
      | `EQ -> 0
      | `GT -> 1
    in List.fast_sort compare d

  let fold f init d = let ls = to_list d in
    List.fold_left (fun acc (k, v) -> f k v acc) init ls

  let format format_val fmt d =
    Format.fprintf fmt "{";
    List.iter 
      (fun (k, v) -> Format.fprintf fmt "\n  %a: %a," Key.format k format_val v)
      (take 50 d);
    Format.fprintf fmt (if size d > 10 then "\n  ...\n}" else "\n}");
end

(* TODO (later): implement a 2-3 tree.  For now,
   this code punts by equating the two functors. *)
module MakeTreeDictionary = MakeListDictionary

module type Set =
sig
  module Elt : Comparable
  type elt = Elt.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module type SetMaker =
  functor (C: Comparable) -> Set with type Elt.t = C.t

(* HINT:  To build a set out of a dictionary, consider this:
   a dictionary is much like a **set** of (key,value) pairs. *)
module MakeSetOfDictionary (D: Dictionary) = struct
  module Elt = D.Key
  type elt = Elt.t

  type dummy = Dummy
  type t = dummy D.t

  let rep_ok s = raise Unimplemented

  let empty = D.empty

  let is_empty = D.is_empty

  let size = D.size

  let insert x = D.insert x Dummy

  let member x = D.member x

  let remove x = D.remove x

  let choose s = match D.choose s with
    | Some (k, _) -> Some k
    | None        -> None

  let fold f init =
    D.fold (fun elt _ acc -> f elt acc) init

  let union s1 s2 =
    fold (fun elt acc -> insert elt acc) s1 s2

  let intersect s1 s2 =
    fold (fun elt acc -> if member elt s2 then insert elt acc else acc) empty s1

  let difference s1 s2 =
    fold (fun elt acc -> if member elt s2 then acc else insert elt acc) empty s1

  let to_list s = List.map fst (D.to_list s)

  let format fmt s = let ls = to_list s in
    Format.fprintf fmt "{";
    List.iter 
      (fun elt -> Format.fprintf fmt "%a, " Elt.format elt)
      (take 20 ls);
    Format.fprintf fmt (if List.length ls > 20 then "...}" else "}");
end

