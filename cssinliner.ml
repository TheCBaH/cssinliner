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


type css_at_payload = Block of string | Line of string [@@deriving sexp]

type css_value_atom =
  | Any of string
  | Identifier of string
  | String of char * string
  | Url of string
  [@@deriving sexp]

type css =
  | At of string * css_at_payload
  | Rule of string list * (string * css_value_atom list) list
  [@@deriving sexp]

module Css = struct
  type t = css list [@@deriving sexp]
end

(*  4.1.1 Tokenization *)
let declaration =
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
  let value =
    many
      ( wspace
        *> choice
             [ quoted_string
             ; (url_resource >>| fun s -> Url s)
             ; (identifier >>| fun s -> Identifier s)
             ; (any >>| fun s -> Any s) ]
      <* wspace )
  in
  lift2
    (fun id value -> (id, value))
    (wspace *> identifier <* wspace <* char ':')
    value


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
  let at_rule =
    char '@'
    *> lift3
         (fun id () block -> At (id, block))
         id wspace
         ( lift (fun b -> Block (skip_trailwspace b)) raw_block
         <|> lift (fun l -> Line l) semicolon_line )
  in
  let rule =
    take_while (function ',' | '{' | ';' | '}' -> false | _ -> true)
  in
  let rules = sep_by1 (char ',' *> wspace) rule in
  let selector = wspace *> rules in
  let rule =
    lift2 (fun selector block -> Rule (selector, block)) selector block
  in
  many (wspace *> (at_rule <|> rule)) <* wspace <* end_of_input


let import_url = Angstrom.(wspace *> (parse_quoted_string <|> url_resource))

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
    match Angstrom.parse_only css_parser (`String payload) with
    | Result.Ok style ->
        List.fold_left
          (fun styles s ->
            match s with
            | At ("import", Line line) -> (
              match Angstrom.parse_only import_url (`String line) with
              | Result.Ok url ->
                  let payload = state.load (stack, url) in
                  parse_style_aux state (url :: stack) styles payload
              | _ ->
                  if state.verbose then
                    "Can't parse import line: '" ^ line ^ "'" |> prerr_endline ;
                  styles )
            | _ -> s :: styles)
          styles style
    | _ ->
        if state.verbose then "Can't parse style:'" ^ payload ^ "'"
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
         match attribute "href" n with None -> l | Some s -> ([], s) :: l)
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
            match List.assoc_opt k subst with
            | Some k' ->
                set_attribute k' (declaration_to_string d) node ;
                false
            | None -> true)
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
  ^ ( ["hover"; "active"; "focus"; "visited"; "link"]
    |> List.map (fun s -> ":" ^ s) |> String.concat "\\|" )
  ^ "\\).*"
  |> Str.regexp


let re_special = Str.regexp ".*::-"

let apply_style state style html =
  let open Soup in
  List.iter
    (function
        | Rule (selector, styles) ->
            selector
            |> List.iter (fun selector ->
                   if not
                        ( Str.string_match re_pseudos selector 0
                        || Str.string_match re_special selector 0 )
                   then
                     let selected =
                       try Some (html $$ selector) with exn ->
                         if state.verbose then
                           "Can't handle selector: '" ^ selector ^ "'"
                           |> prerr_endline ;
                         raise exn
                       (* None *)
                     in
                     match selected with
                     | Some selected ->
                         selected |> iter (add_style state styles) ;
                         if false then "applied style to: '" ^ selector ^ "'"
                           |> print_endline
                     | None -> ()
                   else if state.verbose then
                     "Skipping selector: '" ^ selector ^ "'" |> print_endline )
        | _ -> ())
    style ;
  html


let clean state html =
  let open Soup in
  select_internal_styles html |> iter delete ;
  select_external_styles html |> iter delete ;
  if state.clean_class then html $$ "*"
    |> iter (fun n -> delete_attribute "class" n ; delete_attribute "id" n) ;
  html


let inline_css ?(verbose= false) ?(apply_table= false) ?(clean_class= true)
    ?(load= fun _ -> None) html =
  let open Soup in
  let state = {load; clean_class; verbose; apply_table} in
  let html = parse html in
  let ext_styles = get_external_styles html in
  let style = [] in
  let style = load_external_style state ext_styles style in
  let style = load_internal_style state html style in
  (* style *)
  html |> apply_style state style |> clean state |> pretty_print

