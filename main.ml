module StrMap = Map.Make (String)

open Printf

type xml_tag =
  | OpeningTag of {
      tagname    : string;
      attributes : string StrMap.t option;
      content    : string option;
      level      : int;
    }
  | ClosingTag of { tagname : string; level : int }

let rec file_lines file =
  try
    let line = input_line file in
    line :: file_lines file
  with _ -> []

let charlist_of_str str =
  let rec aux i = try String.get str i :: aux (i + 1) with _ -> [] in
  aux 0

let rec charlist_of_lines = function
  | []     -> []
  | l :: t -> charlist_of_str l @ charlist_of_lines t

let open_xml filename =
  charlist_of_lines (file_lines (open_in filename))

let rec str_of_charlist = function
  | []     -> ""
  | h :: t -> String.make 1 h ^ str_of_charlist t

let parse_until stop clist =
  let rec aux acc = function
    | c :: _ as rest when List.mem c stop -> 
      str_of_charlist (List.rev acc), rest
    | any :: t -> aux (any::acc) t
    | [] -> str_of_charlist (List.rev acc), []
  in aux [] clist

(* Same but does not include the stop char in rest. *)
let parse_until_bis stop clist =
  let rec aux acc = function
    | c :: t when List.mem c stop -> 
      str_of_charlist (List.rev acc), t
    | any :: t -> aux (any::acc) t
    | [] -> str_of_charlist (List.rev acc), []
  in aux [] clist

let parse_tagname clist = parse_until_bis [' '; '>'] clist

let parse_key clist = parse_until_bis ['='] clist

let parse_value clist = parse_until ['"'] clist

let rec attributes_exists = function
  | '>' :: _ -> false
  | '=' :: _ -> true
  | _ :: t -> attributes_exists t
  | [] -> false

let parse_attributes cl =
  let rec aux res clist =
    match clist with
    | '>' :: t -> res, t
    | ' ' :: t -> aux res t
    | c :: t as l ->
        if not (attributes_exists l) then res, t
        else
          let key, rest   = parse_key l in
          let value, rest = parse_value (List.tl rest) in
          aux (StrMap.add key value res) (List.tl rest)
    | [] -> failwith "parse_attributes"
  in
  aux StrMap.empty cl

let handle_attributes = function
  | '>' :: t -> (None, t)
  | '<' :: _ as rest -> (None, rest)
  | _ :: _ as l -> 
    let attributes, rest = parse_attributes l in (Some attributes, rest)
  | [] -> (None, [])

(* : <tagname ... > *)
let rec parse_opening_tag level = function
  | '<' :: t ->
      let tagname, rest = parse_tagname t in
      let attributes, rest = handle_attributes rest in
      (OpeningTag { tagname; attributes; content = None; level }, rest)
  | _ -> failwith "parse_opening_tag"

(* </tagname> *)
let rec parse_closing_tag level = function
  | '<' :: '/' :: t ->
      let tagname, rest = parse_tagname t in
      (ClosingTag { tagname; level }, rest)
  | _ -> failwith "parse_closing_tag"

let str_of_char c = String.make 1 c

let parse_content clist = parse_until ['<'] clist

let some = function
  | Some x -> x
  | None -> failwith ""

let openingtag = function
    | OpeningTag {attributes; tagname; level; content} -> attributes, tagname, level, content
    | _ -> failwith ""

(* Charlist of all document *)
let rec xmltags_of_charlist clist = 
  let rec aux level = function
    | '<' :: '/' :: t as l ->
        let tag, rest = parse_closing_tag (level - 1) l in
        tag :: aux (level - 1) rest
    | '<' :: t as l ->
        let tag, rest = parse_opening_tag level l in
        let content, rest = parse_content rest in
        let attr, tagn, lev, _ =  openingtag tag in
        let tag = OpeningTag {attributes=attr; level=lev; tagname=tagn; content=Some content;}
        in
        tag :: aux (level + 1) rest
    | [] -> []
    | l  -> failwith "xmltags_of_charlist"
  in 
  aux 0 clist