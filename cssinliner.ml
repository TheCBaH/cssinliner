(*
#use "topfind";;
#require "angstrom";;
#require "lambdasoup";;
#require "str";;
#require "ppx_sexp_conv";;
 *)

open Sexplib.Std

let string_of_rev_list str =
  let b = Buffer.create 7 in
  List.iter (Buffer.add_char b) (List.rev str) ;
  Buffer.contents b


(* https://www.w3.org/TR/CSS2/syndata.html*)
let wspace =
  let open Angstrom in
  let wspace_raw =
    skip (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)
  and comment =
    string "/*"
    *> scan_state `Empty (fun state ch ->
           match (state, ch) with
           | `Empty, '*' -> Some `Star
           | `Star, '/' -> Some `Done
           | `Done, _ -> None
           | _, '*' -> Some `Star
           | _ -> Some `Empty )
    >>| ignore
  in
  skip_many (wspace_raw <|> comment)


let identifier =
  (* 4.1.3 Characters and case *)
  let open Angstrom in
  scan_state [] (fun str ch ->
      let take = Some (ch :: str) and skip = None in
      match ch with
      | 'A'..'Z' | 'a'..'z' | '-' -> take
      | '0'..'9' -> ( match str with [] -> skip | _ -> take )
      | '_' -> ( match str with [] | ['_'] -> skip | _ -> take )
      | _ -> skip )
  >>= function
    | [] -> fail "identifier" | str -> string_of_rev_list str |> return


let escaped_string_scanner_seed = ([], `Capture)

let escaped_string_scanner sep (str, state) ch =
  match (state, ch) with
  | `Capture, '\\' -> Some (str, `Escape)
  | `Escape, '\r' -> Some (str, `EscapeNl)
  | `Escape, '\n' -> Some (str, `Capture)
  | `EscapNl, '\n' -> Some (str, `Capture)
  | `Escape, ch -> Some (ch :: str, `Capture)
  | `Capture, ch when ch = sep -> None
  | _, ch -> Some (ch :: str, `Capture)


let parse_escaped_string sep =
  let open Angstrom in
  scan_state escaped_string_scanner_seed (escaped_string_scanner sep)
  >>| fun (str, cb) -> string_of_rev_list str


let parse_quoted_string =
  let open Angstrom in
  let escaped_quoted_string sep =
    char sep *> parse_escaped_string sep <* char sep
  in
  escaped_quoted_string '\'' <|> escaped_quoted_string '"'


let url_resource =
  let open Angstrom in
  string "url(" *> wspace *> (parse_quoted_string <|> parse_escaped_string ')')
  <* wspace <* char ')'


type css_value_atom =
  | Any of string
  | Identifier of string
  | String of char * string
  | Url of string
  [@@deriving sexp]

let css_value_atom_to_string = function
  | Any s -> s
  | Identifier s -> s
  | String (c, s) ->
      let buf = 2 + String.length s |> Buffer.create in
      Buffer.add_char buf c ;
      Buffer.add_string buf s ;
      Buffer.add_char buf c ;
      Buffer.contents buf
  | Url s -> "Url('" ^ s ^ "')"


type css_rule =
  {selectors: string list; ruleset: (string * css_value_atom list) list}
  [@@deriving sexp]

let css_rule_to_string ?(indent= "") t =
  let buf = Buffer.create 127 in
  Buffer.add_string buf indent ;
  String.concat ", " t.selectors |> Buffer.add_string buf ;
  Buffer.add_string buf " {\n" ;
  List.iter
    (fun (p, v) ->
      Buffer.add_string buf indent ;
      Buffer.add_string buf "    " ;
      Buffer.add_string buf "    " ;
      Buffer.add_string buf p ;
      Buffer.add_char buf ':' ;
      Buffer.add_char buf ' ' ;
      List.map css_value_atom_to_string v |> String.concat " " |> Buffer.add_string buf;
      Buffer.add_char buf '\n')
    t.ruleset ;
  Buffer.add_string buf indent ;
  Buffer.add_string buf "}\n" ;
  Buffer.contents buf


type css_at_payload =
  | Block of string
  | Compound of css_rule list
  | Line of string
  [@@deriving sexp]

let css_at_payload_to_string = function
  | Block s -> "{\n" ^ s ^ "\n}"
  | Compound c ->
      let buf = Buffer.create 127 in
      Buffer.add_string buf "{\n" ;
      List.iter
        (fun rule ->
          let indent = "    " in
          css_rule_to_string ~indent rule |> Buffer.add_string buf)
        c ;
      Buffer.add_string buf "}\n" ;
      Buffer.contents buf
  | Line l -> l ^ ";\n"


type css =
  | At of string * css_value_atom list * css_at_payload
  | Import of string * css_value_atom list
  | Rule of css_rule
  | NotParsed of string
  [@@deriving sexp]

let css_to_string = function
  | At (n, v, p) ->
      let buf = Buffer.create 127 in
      Buffer.add_char buf '@' ;
      Buffer.add_string buf n ;
      Buffer.add_char buf ' ' ;
      List.map css_value_atom_to_string v |> String.concat " "
      |> Buffer.add_string buf ;
      Buffer.add_char buf ' ' ;
      css_at_payload_to_string p |> Buffer.add_string buf ;
      Buffer.contents buf
  | Import (n, v) ->
      let buf = Buffer.create 127 in
      Buffer.add_string buf "@import " ;
      Buffer.add_string buf n ;
      List.map css_value_atom_to_string v |> String.concat " "
      |> Buffer.add_string buf ;
      Buffer.contents buf
  | Rule r -> css_rule_to_string r
  | NotParsed s -> s


(*  4.1.1 Tokenization *)
let css_any =
  let open Angstrom in
  let any = take_while1 (function ';' | '{' | '}' -> false | _ -> true) in
  let capture_quoted_string sep =
    char sep
    *> scan_string escaped_string_scanner_seed (escaped_string_scanner sep)
    <* char sep >>| fun s -> String (sep, s)
  in
  let quoted_string =
    capture_quoted_string '"' <|> capture_quoted_string '\''
  in
  many
    ( wspace
      *> choice
           [ quoted_string
           ; (url_resource >>| fun s -> Url s)
           ; (identifier >>| fun s -> Identifier s)
           ; (any >>| fun s -> Any s) ]
    <* wspace )


let declaration =
  let open Angstrom in
  lift2
    (fun id value -> (id, value))
    (wspace *> identifier <* wspace <* char ':')
    css_any


let single_quote_string s =
  let len = String.length s in
  let b = Buffer.create (len + 2) in
  Buffer.add_char b '\'' ;
  for i = 0 to len - 1 do
    let ch = s.[i] in
    match ch with
    | '\'' | '\n' -> Buffer.add_char b '\\' ; Buffer.add_char b ch
    | ch -> Buffer.add_char b ch
  done ;
  Buffer.contents b


let declaration_to_string d =
  d
  |> List.map (function
       | String (sep, s) ->
           let sep = String.make 1 sep in
           sep ^ s ^ sep
       | Url url -> "url('" ^ single_quote_string url ^ "')"
       | Identifier x -> x
       | Any x -> x )
  |> String.concat " "


let ruleset =
  let open Angstrom in
  choice
    [ lift3
        (fun d dl () -> d :: dl)
        declaration
        (many (wspace *> char ';' *> declaration))
        (wspace <* option ' ' (char ';'))
    ; (wspace >>| fun _ -> []) ]


let css_parser =
  let open Angstrom in
  let raw_block =
    char '{' *> wspace
    *> lift2
         (fun s _ -> s)
         (take_while (function '}' -> false | _ -> true))
         (char '}')
  in
  let id =
    take_while (function
      | 'A'..'Z' | 'a'..'z' | '0'..'0' | '_' | '-' -> true
      | _ -> false )
  in
  let semicolon_line =
    wspace *> take_while (function ';' | '{' | '}' -> false | _ -> true)
    <* char ';' <* wspace
  in
  let block = char '{' *> wspace *> ruleset <* wspace <* char '}' in
  let re_trailwspace = Str.regexp "[ \r\n\t]+$" in
  let skip_trailwspace str =
    try Str.search_forward re_trailwspace str 0 |> String.sub str 0
    with Not_found -> str
  in
  let selector =
    take_while (function ',' | '{' | ';' | '}' -> false | _ -> true)
  in
  let selectors = wspace *> sep_by1 (char ',' *> wspace) selector in
  let styles =
    lift2 (fun selectors ruleset -> {selectors; ruleset}) selectors block
  in
  let compound_block =
    char '{' *> many (wspace *> styles) <* wspace <* char '}'
  in
  let at_rule =
    char '@'
    *> lift3
         (fun id any block -> At (id, any, block))
         (id <* wspace) css_any
         ( lift (fun l -> Compound l) compound_block
         <|> lift (fun b -> Block (skip_trailwspace b)) raw_block
         <|> lift (fun l -> Line l) semicolon_line )
  in
  let import =
    char '@' *> string "import" *> wspace
    *> lift2
         (fun url qual -> Import (url, qual))
         (parse_quoted_string <|> url_resource)
         (wspace *> css_any)
    <* wspace <* char ';'
  in
  let not_parsed =
    take_while1 (function '}' -> false | _ -> true)
    <* (char '}' >>| ignore <|> end_of_input) >>| fun s -> NotParsed s
  in
  let css_parser =
    many
      ( wspace
      *> (import <|> at_rule <|> (styles >>| fun s -> Rule s) <|> not_parsed) )
  in
  css_parser <* wspace <* end_of_input


module Css = struct
  type t = css list [@@deriving sexp]

  let of_string s = Angstrom.parse_string css_parser s

  let to_string t = List.map css_to_string t |> String.concat "\n"
end

type state =
  { load: string list * string -> string option
  ; clean_class: bool
  ; verbose: bool
  ; apply_table: bool }

let empty_state =
  { load= (fun _ -> None)
  ; clean_class= false
  ; verbose= false
  ; apply_table= false }


let rec parse_style_aux state stack styles payload =
  match payload with
  | None -> styles
  | Some payload ->
    match Css.of_string payload with
    | Result.Ok style ->
        List.fold_left
          (fun styles s ->
            match s with
            | Import (url, []) ->
                let payload = state.load (stack, url) in
                parse_style_aux state (url :: stack) styles payload
            | _ -> s :: styles)
          styles style
    | Result.Error error ->
        if state.verbose then
          "Can't parse style:'" ^ payload ^ "' (" ^ error ^ ")"
          |> prerr_endline ;
        styles


let parse_one_style state stack styles payload =
  parse_style_aux state stack styles payload


let select_internal_styles html = Soup.(html $$ "style")

let load_internal_style state html styles =
  let open Soup in
  select_internal_styles html
  |> fold (fun s n -> leaf_text n |> parse_one_style state [] s) styles


let select_external_styles html = Soup.(html $$ "link[rel=stylesheet]")

let get_external_styles html =
  let open Soup in
  select_external_styles html
  |> fold
       (fun l n ->
         match attribute "href" n with None -> l | Some s ->  s  :: l)
       []
  |> List.rev


let load_external_style state links style =
  List.fold_left
    (fun s ((stack, url) as l) ->
      state.load l |> parse_one_style state (url :: stack) s)
    style links


let add_style state styles node =
  let open Soup in
  let subst =
    if state.apply_table then
      match name node with
      | "table" ->
          [ ("float", "align")
          ; ("background-color", "bgcolor")
          ; ("width", "width")
          ; ("height", "height") ]
      | "tr" ->
          [ ("background-color", "bgcolor")
          ; ("vertical-align", "valign")
          ; ("text-align", "align") ]
      | "td" | "th" ->
          [ ("background-color", "bgcolor")
          ; ("width", "width")
          ; ("height", "height")
          ; ("vertical-align", "valign")
          ; ("text-align", "align")
          ; ("white-space", "nowrap") ]
      | "tbody" | "thead" | "tfoot" ->
          [("vertical-align", "valign"); ("text-align", "align")]
      | _ -> []
    else []
  in
  let styles =
    match subst with
    | [] -> styles
    | subst ->
        List.filter
          (fun (k, d) ->
            try
                let k' = List.assoc k subst in
                set_attribute k' (declaration_to_string d) node ;
                false;
            with Not_found -> true)
          styles
  in
  match styles with
  | [] -> ()
  | styles ->
      let style =
        ( styles |> List.map (fun (k, d) -> k ^ ":" ^ declaration_to_string d)
        |> String.concat ";" )
        ^ ";"
      in
      let style =
        match attribute "style" node with Some s -> style ^ s | None -> style
      in
      set_attribute "style" style node


let re_pseudos =
  ".*\\("
  ^ ( [ "active"
      ; "after"
      ; "before"
      ; "checked"
      ; "disabled"
      ; "enabled"
      ; "first-letter"
      ; "first-line"
      ; "focus"
      ; "hover"
      ; "indeterminate"
      ; "lang"
      ; "link"
      ; "selection"
      ; "target"
      ; "visited" ]
    |> List.map (fun s -> ":" ^ s) |> String.concat "\\|" )
  ^ "\\).*"
  |> Str.regexp


let re_special = Str.regexp ".*\\(::-\\|\\\\\\)"
let re_partial_comments = Str.regexp ".*\\(\\(\\*/\\)\\|\\(/\\*\\)\\)"

module SoupNode = struct
  type t = Soup.element Soup.node

  let equal s s' = s == s'

  let hash = Hashtbl.seeded_hash
end

module NodeHash = Hashtbl.MakeSeeded (SoupNode)

let assign_style state style hash html =
  let open Soup in
  List.iter
    (function
        | Rule {selectors; ruleset} ->
            selectors
            |> List.iter (fun selector ->
                   if not
                        ( Str.string_match re_pseudos selector 0
                        || Str.string_match re_special selector 0
                        || Str.string_match re_partial_comments selector 0
                      )
                   then
                     let selected =
                       try Some (html $$ selector) with exn ->
                         if true || state.verbose then
                           "Can't handle selector: '" ^ selector ^ "'"
                           |> prerr_endline ;
                       (*  raise exn *)
                       None
                     in
                     match selected with
                     | None -> ()
                     | Some selected ->
                         selected
                         |> iter (fun node ->
                                try
                                  let rules = NodeHash.find hash node in
                                  rules := ruleset :: !rules
                                with Not_found ->
                                  NodeHash.add hash node (ref [ruleset]) )
                   else if state.verbose then
                     "Skipping selector: '" ^ selector ^ "'" |> print_endline )
        | _ -> ())
    style


let apply_style state hash =
  NodeHash.iter
    (fun node ruleset ->
      List.iter (fun rule -> add_style state rule node) (List.rev !ruleset))
    hash

let clean_class html =
  let open Soup in
  let clean n =
    delete_attribute "class" n ;
    delete_attribute "id" n in
  html $$ "*" |> iter clean

let clean state html =
  let open Soup in
  select_internal_styles html |> iter delete ;
  select_external_styles html |> iter delete ;
  if state.clean_class then clean_class html


let inline_css ?(verbose= false) ?(apply_table= false) ?(clean_class= false)
    ?(load= fun _ -> None) html =
  let open Soup in
  let state = {load; clean_class; verbose; apply_table} in
  let ext_styles = get_external_styles html |> List.map (fun s -> [],s) in
  let style = [] in
  let style = load_external_style state ext_styles style in
  let style = load_internal_style state html style in
  (* style *)
  let hash = NodeHash.create 17 in
  assign_style state style hash html ;
  apply_style state hash ;
  clean state html ;
  html

