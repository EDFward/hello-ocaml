exception Unimplemented

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module MakeEngine
    (S: Data.Set with type Elt.t = string)
    (D: Data.Dictionary with type Key.t = string)
  : Engine
=
struct
  type idx = S.t D.t

  let delimiter_regexp = Str.regexp "[^0-9a-zA-Z]+"

  (** Add (word, file) pairs to the existing index. *)
  let index_file file_name (acc: idx) =
    let chan = open_in file_name in
    (* Split the line and insert to dict. *)
    let index_line line (acc_in_line: idx) =
      List.fold_left
        (
          fun acc w ->
            let files = match D.find w acc with
              | Some files -> files
              | None       -> S.empty
            in D.insert w (S.insert file_name files) acc
        )
        acc_in_line (Str.split delimiter_regexp line)
    in
    let rec index_lines (acc_in_lines: idx) =
      try
        let line = input_line chan in index_lines (index_line line acc_in_lines)
      with
        End_of_file -> (close_in chan; acc_in_lines)
    in index_lines acc

  let index_of_dir d =
    try
      let files = Sys.readdir d in
      Array.fold_left
        (fun acc f -> index_file (d^Filename.dir_sep^f) acc) D.empty files
    with
      Sys_error _ -> raise Not_found

  let to_list idx =
    List.map (fun (w, fileSet) -> (w, S.to_list fileSet)) (D.to_list idx)

  (** Find union / intersection of files with given word list. *)
  let get_files idx (words: string list) f = let open List in
    match words with
    | []            -> S.empty
    | first_word::rest ->
      let get_files_for_word w = match D.find w idx with
        | Some files -> files
        | None       -> S.empty
      in
      let init = get_files_for_word first_word
      in
      fold_left
        (fun acc w -> f acc (get_files_for_word w)) init rest

  let or_not idx ors nots =
    let files_for_or = get_files idx ors S.union
    and files_for_not = get_files idx nots S.union
    in
    S.difference files_for_or files_for_not |> S.to_list

  let and_not idx ands nots =
    let files_for_and = get_files idx ands S.intersect
    and files_for_not = get_files idx nots S.union
    in
    S.difference files_for_and files_for_not |> S.to_list

  let format = D.format S.format
end

module StringComparable = struct
  type t = string
  let compare a b = match String.compare a b with
    | -1 -> `LT
    | 1  -> `GT
    | _  -> `EQ
  let format fmt = Format.fprintf fmt "%s"
end

module StringDictByList = Data.MakeListDictionary(StringComparable)
module StringDictByTree = Data.MakeTreeDictionary(StringComparable)

module StringSetByList = Data.MakeSetOfDictionary(StringDictByList)
module StringSetByTree = Data.MakeSetOfDictionary(StringDictByTree)

module ListEngine = MakeEngine(StringSetByList)(StringDictByList)
module TreeEngine = MakeEngine(StringSetByTree)(StringDictByTree)
