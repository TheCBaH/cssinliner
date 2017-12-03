(*
#use "topfind";;
#require "angstrom";;
#require "lambdasoup";;
#require "str";;
 *)

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
  let wspace = skip_many (wspace_raw <|> comment) in
  wspace


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
  let block = char '{' *> many semicolon_line <* char '}' in
  let re_trailwspace = Str.regexp "[ \r\n\t]+$" in
  let skip_trailwspace str =
    try Str.search_forward re_trailwspace str 0 |> String.sub str 0
    with Not_found -> str
  in
  let at_rule =
    wspace *> char '@'
    *> lift3
         (fun id () block -> `At (id, block))
         id wspace
         ( lift (fun b -> `Block (skip_trailwspace b)) raw_block
         <|> lift (fun l -> `Line l) semicolon_line )
  in
  let rule =
    wspace *> take_while (function ',' | '{' | ';' | '}' -> false | _ -> true)
  in
  let rules = sep_by1 (char ',' *> wspace) rule in
  let selector = wspace *> rules in
  let re_colummn = Str.regexp ":[ \r\n\t]*" in
  let split_style str =
    (* Str.bounded_split re_colummn str 2 *)
    match Str.bounded_split re_colummn str 2 with
    | [n; v] -> (n, v)
    | _ -> failwith "style"
  in
  let rule =
    lift2
      (fun selector block -> `Rule (selector, List.map split_style block))
      selector block
  in
  many (at_rule <|> rule)


let import_url =
  let open Angstrom in
  let single_quoted_string =
    char '\'' *> take_while (function '\'' -> false | _ -> true) <* char '\''
  and double_quoted_string =
    char '"' *> take_while (function '"' -> false | _ -> true) <* char '"'
  in
  let quoted_string = single_quoted_string <|> double_quoted_string in
  let url_resource =
    string "url(" *> wspace *> quoted_string <* wspace <* char ')'
  in
  wspace *> (quoted_string <|> url_resource)


type state =
  { load: string list * string -> string option
  ; clean_class: bool
  ; verbose: bool }

let parse_one_style style str =
  match str with
  | None -> style
  | Some str ->
    match Angstrom.parse_only css_parser (`String str) with
    | Result.Ok l -> List.append l style
    | _ -> style


let rec parse_style_aux state stack styles payload =
  match payload with
  | None -> styles
  | Some payload ->
    match Angstrom.parse_only css_parser (`String payload) with
    | Result.Ok style ->
        List.fold_left
          (fun styles s ->
            match s with
            | `At ("import", `Line line) -> (
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


let add_style styles node =
  let open Soup in
  let subst =
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
  in
  let styles =
    match subst with
    | [] -> styles
    | subst ->
        List.filter
          (fun (k, v) ->
            match List.assoc_opt k subst with
            | Some k' -> set_attribute k' v node ; false
            | None -> true)
          styles
  in
  match styles with
  | [] -> ()
  | styles ->
      let style =
        (styles |> List.map (fun (k, v) -> k ^ ":" ^ v) |> String.concat ";")
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

let apply_style ~verbose style html =
  let open Soup in
  List.iter
    (function
        | `Rule (selector, styles) ->
            selector
            |> List.iter (fun selector ->
                   if not
                        ( Str.string_match re_pseudos selector 0
                        || Str.string_match re_special selector 0 )
                   then
                     try html $$ selector |> iter (add_style styles)
                     with _ ->
                       "Can't handle selector: '" ^ selector ^ "'"
                       |> prerr_endline
                   else if verbose then "Skipping selector: '" ^ selector ^ "'"
                     |> print_endline )
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


let inline_css ?(verbose= false) ?(clean_class= true) ?(load= fun _ -> None)
    html =
  let open Soup in
  let state = {load; clean_class; verbose} in
  let html = parse html in
  let ext_styles = get_external_styles html in
  let style = [] in
  let style = load_external_style state ext_styles style in
  let style = load_internal_style state html style in
  (* style *)
  html |> apply_style ~verbose style |> clean state |> pretty_print

