let rec get_lines file =
  try
    let line = input_line file in
    line :: get_lines file
  with _ -> []

let string_to_charlist str =
  let rec aux i = try String.get str i :: aux (i + 1) with _ -> [] in
  aux 0

let rec lines_to_charlist = function
  | []     -> []
  | l :: t -> string_to_charlist l @ lines_to_charlist t

let open_xml filename =
  let file  = open_in filename in
  let lines = get_lines file in
  lines_to_charlist lines

module StrMap = Map.Make (String)

type xml_tag =
  | OpeningTag of {
      tagname    : string;
      attributes : string StrMap.t option;
      content    : string option;
      level      : int;
    }
  | ClosingTag of { tagname : string; level : int }

let rec str_of_clist = function
  | []     -> ""
  | h :: t -> String.make 1 h ^ str_of_clist t

(* : tagname ... > *)
let rec get_tagname clist =
  let rec aux acc = function
    | ' ' :: t as l -> (str_of_clist (List.rev acc), l)
    | '>' :: t as l -> (str_of_clist (List.rev acc), l)
    | c :: t        -> aux (c :: acc) t
    | []            -> failwith "error"
  in
  aux [] clist

open Printf

let rec get_key clist =
  let () = printf "get_key input: %s\n" (str_of_clist clist) in
  let rec aux acc = function
    | '=' :: t -> (str_of_clist (List.rev acc), t)
    | c :: t   -> aux (c :: acc) t
    | []       -> failwith "key"
  in
  aux [] clist

let rec get_value clist =
  let () = printf "get_value input: %s\n" (str_of_clist clist) in
  let rec aux acc = function
    | ' ' :: t as l -> (str_of_clist (List.rev acc), l)
    | '>' :: t as l -> (str_of_clist (List.rev acc), l)
    | c :: t        -> aux (c :: acc) t
    | []            -> failwith "value"
  in
  aux [] clist

let get_attributes clist =
  let rec aux attr = function
    | '>' :: t    -> (attr, t)
    | ' ' :: t    -> aux attr t
    | c :: t as l ->
        let key, rest   = get_key l in
        let value, rest = get_value rest in
        aux (StrMap.add key value attr) rest
    | [] -> failwith "attr"
  in
  aux StrMap.empty clist

(* : <tagname ... > *)
let rec parse_opening_tag level = function
  | '<' :: t ->
      let tagname, rest = get_tagname t in
      let () = printf "parse op tag rest: %s\n" (str_of_clist rest) in
      let attributes, rest =
        match rest with
        | '>' :: t -> (None, t)
        | '<' :: t -> (None, rest)
        | _ :: t   ->
            let map, r = get_attributes rest in (Some map, r)
        | [] -> (None, [])
      in
      (OpeningTag { tagname; attributes; content = None; level }, rest)
  | _ -> failwith "opening"

(* </tagname> *)
let rec parse_closing_tag level = function
  | '<' :: '/' :: t ->
      let tagname, rest = get_tagname t in
      let () = printf "parse clo rest = %s\n\n" (str_of_clist rest) in
      let rest = 
      ( match rest with 
          | '>'::t -> t
          | _ -> rest
      ) in
      (ClosingTag { tagname; level }, rest)
  | _ -> failwith "opening"

open Printf

let str_of_char c = String.make 1 c

let rec parse_content = function
  | '<' :: _ as l -> "", l
  | c :: t -> 
    let res, rest = parse_content t in 
    (str_of_char c) ^ res, rest
  | [] -> failwith "parse content"

let some = function
  | Some x -> x
  | None -> failwith ""

let openingtag = function
    | OpeningTag {attributes; tagname; level; content} -> attributes, tagname, level, content
    | _ -> failwith ""

(* Charlist of all document *)
let rec to_xml_tag_list clist = 
  let rec aux level = function
    | '<' :: '/' :: t as l ->
        let tag, rest = parse_closing_tag (level - 1) l in
        tag :: aux (level - 1) rest
    | '<' :: t as l ->
        let () = printf "Given list: %s\n" (str_of_clist l) in
        let tag, rest = parse_opening_tag level l in
        let () = printf "Rest after parsing op tag: %s\n" (str_of_clist rest) in
        let content, rest = parse_content rest in
        let () = printf "\nContent: %s\n" (content) in
        let () = printf "Rest: %s\n" (str_of_clist rest) in
        let attr, tagn, lev, _ =  openingtag tag in
        let tag = OpeningTag {attributes=attr; level=lev; tagname=tagn; content=Some content;}
        in
        tag :: aux (level + 1) rest
    | [] -> []
    | l  -> 
      let () = printf "\nlast case: %s\n\n" (str_of_clist l) in
      failwith "main"
  in 
  aux 0 clist

(*
let example () = 
  let clist = open_xml "sample2.xml" in
  let tag = parse_opening_tag clist in
  let attr = match tag with OpeningTag record -> record.attributes | _ -> failwith ""
  in
  let map = match attr with Some x -> x | _ -> failwith "" in
  let () = StrMap.iter (fun k v -> Printf.printf "%s, %s\n" k v) map in ()
  *)
