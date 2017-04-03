(* A [Comparable] is a value that can be compared.
 * The comparison is a total order on the values. *)
module type Comparable = sig

  (* The type of comparable values. *)
  type t

  (* [compare t1 t2] is [`LT] if [t1] is less than [t2],
   * [`EQ] if [t1] is equal to [t2], or [`GT] if [t1] is
   * greater than [t2]. *)
  val compare : t -> t -> [`LT | `EQ | `GT]

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of a value of
   * type t on the given formatter. *)
  val format : Format.formatter -> t -> unit

end

(* A [Dictionary] maps keys to values. The keys
 * must be comparable, but there are no restrictions
 * on the values. *)
module type Dictionary = sig

  (* [Key] is a module representing the type of keys
   * in the dictionary and functions on them. *)
  module Key : Comparable

  (* [key] is the type of keys in the dictionary
   * and is a synonym for [Key.t]. *)
  type key = Key.t

  (* ['value t] is the type of dictionaries in which keys
   * are bound to values of type ['value] *)
  type 'value t

  (* [rep_ok d] returns [d] if [d] satisfies its representation
   * invariants. It's unusual for a data abstraction to
   * expose this function to its clients, but we do so here
   * to ensure that you implement it.
   * raises: [Failure] with an unspecified error message
   *   if [d] does not satisfy its representation invariants. *)
  val rep_ok : 'value t  -> 'value t

  (* [empty] is the empty dictionary *)
  val empty : 'value t

  (* [is_empty d] is [true] iff [d] is empty. *)
  val is_empty : 'value t -> bool

  (* [size d] is the number of bindings in [d]. *
   * [size empty] is [0]. *)
  val size : 'value t -> int

  (* [insert k v d] is [d] with [k] bound to [v]. If [k] was already
   * bound, its previous value is replaced with [v]. *)
  val insert : key -> 'value -> 'value t -> 'value t

  (* [member k d] is [true] iff [k] is bound in [d]. *)
  val member : key -> 'value t -> bool

  (* [find k d] is [Some v] if [k] is bound to [v] in [d]; or
   * if [k] is not bound, then it is [None]. *)
  val find : key -> 'value t -> 'value option

  (* [remove k d] contains all the bindings of [d] except
   * a binding for [k].  If [k] is not bound in [d], then
   * [remove] returns a dictionary with the same bindings
   * as [d]. *)
  val remove : key -> 'value t -> 'value t

  (* [choose d] is [Some (k,v)], where [k] is bound to [v]
   * in [d].  It is unspecified which binding of [d] is
   * returned.  If [d] is empty, then [choose d] is [None]. *)
  val choose : 'value t -> (key * 'value) option

  (* [fold f init d] is [f kn vn (f ... (f k1 v1 init) ...)],
   * if [d] binds [ki] to [vi].  Bindings are processed
   * in order from least to greatest, where [k1] is the
   * least key and [kn] is the greatest. *)
  val fold : (key -> 'value -> 'acc -> 'acc) -> 'acc -> 'value t -> 'acc

  (* [to_list d] is an association list containing the same
   * bindings as [d].  The order of elements in the list is
   * in order from the least key to the greatest. *)
  val to_list : 'value t -> (key * 'value) list

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of a dictionary
   * on the given formatter. *)
  val format : (Format.formatter -> 'value -> unit)
    -> Format.formatter -> 'value t -> unit
end

(* A [DictionaryMaker] is a functor that makes a [Dictionary]
 * out of a [Comparable]. *)
module type DictionaryMaker =
  functor (C : Comparable) -> Dictionary with type Key.t = C.t

(* [MakeListDictionary] makes a [Dictionary] implemented
 * with association lists. All the operations must be
 * tail recursive. *)
module MakeListDictionary : DictionaryMaker

(* [MakeTreeDictionary] makes a [Dictionary] implemented
 * with 2-3 trees. *)
module MakeTreeDictionary : DictionaryMaker

(* A [Set] contains elements, which must be comparable. *)
module type Set = sig

  (* [Elt] is a module representing the type of elements
   * in the set and functions on them. *)
  module Elt : Comparable

  (* [elt] is the type of elements in the set
   * and is a synonym for [Elt.t]. *)
  type elt = Elt.t

  (* [t] is the type of sets. *)
  type t

  (* [rep_ok s] returns [s] if [s] satisfies its representation
   * invariants.  It's unusual for a data abstraction to
   * expose this function to its clients, but we do so here
   * to ensure that you implement it.
   * raises: [Failure] with an unspecified error message
   *   if [s] does not satisfy its representation invariants. *)
  val rep_ok : t  -> t

  (* [empty] is the empty set. *)
  val empty : t

  (* [is_empty s] is [true] iff [s] is empty. *)
  val is_empty : t -> bool

  (* [size s] is the number of elements in [s]. *
   * [size empty] is [0]. *)
  val size : t -> int

  (* [insert x s] is a set containing all the elements of
   * [s] as well as element [x]. *)
  val insert : elt -> t -> t

  (* [member x s] is [true] iff [x] is an element of [s]. *)
  val member : elt -> t -> bool

  (* [remove x s] contains all the elements of [s] except
   * [x].  If [x] is not an element of [s], then
   * [remove] returns a set with the same elements as [s]. *)
  val remove : elt -> t -> t

  (* [union] is set union, that is, [union s1 s2] contains
   * exactly those elements that are elements of [s1]
   * **or** elements of [s2]. *)
  val union : t -> t -> t

  (* [intersect] is set intersection, that is, [intersect s1 s2]
   * contains exactly those elements that are elements of [s1]
   * **and** elements of [s2]. *)
  val intersect : t -> t -> t

  (* [difference] is set difference, that is, [difference s1 s2]
   * contains exactly those elements that are elements of [s1]
   * **and not** elements of [s2]. *)
  val difference : t -> t -> t

  (* [choose s] is [Some x], where [x] is an unspecified
   * element of [s].  If [s] is empty, then [choose s] is [None]. *)
  val choose : t -> elt option

  (* [fold f init s] is [f xn (f ... (f x1 init) ...)],
   * if [s] contains [x1]..[xn].  Elements are processed
   * in order from least to greatest, where [x1] is the
   * least element and [xn] is the greatest. *)
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  (* [to_list s] is a list containing the same
   * elements as [s].  The order of elements in the list is
   * in order from the least set element to the greatest. *)
  val to_list : t -> elt list

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of a set
   * on the given formatter. *)
  val format : Format.formatter -> t -> unit

end

(* A [SetMaker] is a functor that makes a [Set]
 * out of a [Comparable]. *)
module type SetMaker =
  functor (C : Comparable) -> Set with type Elt.t = C.t

(* [MakeSetOfDictionary] makes a [Set] out of a [Dictionary].
 * The set is implemented as a dictionary, without the functor needing
 * to know about the internal implementation of that dictionary. *)
module MakeSetOfDictionary (D : Dictionary) : Set with type Elt.t = D.key
