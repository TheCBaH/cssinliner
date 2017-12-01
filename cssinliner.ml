(*
#use "topfind";;
#require "angstrom";;
#require "lambdasoup";;
#require "str";;
*)


let css_parser =
  let open Angstrom in
  let wspace = skip_while (function
                     '\x20' | '\x0a' | '\x0d' | '\x09' -> true
                     | _ -> false) in
  let raw_block = char '{' *> wspace *> lift2 (fun s _ -> s)
                (take_while (function
                    '}' -> false
                    | _ -> true))  (char '}') in
  let id = take_while (function
                 'A'..'Z' | 'a'..'z' | '0'..'0' | '_' | '-' -> true
                 | _ -> false) in
  let semicolon_line =
    wspace *> take_while (function ';'|'{'|'}' -> false | _ -> true) <* (char ';') <* wspace in
  let block = char '{' *> (many semicolon_line) <* char '}' in
  let re_trailwspace = Str.regexp "[\x20\x0d\x0a\x09]+$" in
  let skip_trailwspace str =
    try
      Str.search_forward re_trailwspace str 0 |> String.sub str 0
    with Not_found -> str in
  let at_rule = wspace *> char '@' *>
                  lift3 (fun id () block -> `At (id,block))
                    id  wspace (
                      (lift (fun b -> `Block (skip_trailwspace b)) raw_block) <|>
                        (lift (fun l -> `Line l) semicolon_line)) in
  let selector = wspace *> take_while (function
                       '{'|';'|'}' -> false
                     | _ -> true
                             ) in
  let re_colummn = Str.regexp ":[\x20\x0d\x0a\x09]*" in
  let split_style str =
    (* Str.bounded_split re_colummn str 2 *)
    match Str.bounded_split re_colummn str 2 with
      [n;v] -> n,v
    | _ -> failwith "style"
  in
  let rule = lift2 (fun selector block ->
                 `Rule ( (skip_trailwspace selector),List.map split_style block)) selector block in
  many (at_rule <|> rule)

type state = {
    load: (string list * string) -> string option;
    clean_class: bool;
}

let parse_one_style style str =
  match str with
    None -> style
  | Some str ->
     match Angstrom.parse_only css_parser (`String str) with
       Result.Ok l ->
        List.append l style
     | _ -> style

let select_internal_styles html =
  let open Soup in
  html $$ "style"

let load_internal_style html styles =
  let open Soup in
  select_internal_styles html |>
    fold (fun s n ->
        leaf_text n |> parse_one_style s
      ) styles

let select_external_styles html =
  let open Soup in
  html $$ "link[rel=stylesheet]"

let get_external_styles html =
  let open Soup in
  select_external_styles html |>
    fold (fun l n ->
        match attribute "href" n  with
          None -> l
        | Some s -> ([],s)::l) [] |> List.rev

let load_external_style state links style =
  List.fold_left (fun s l ->
      state.load l |> parse_one_style s) style links

let add_style styles node =
  let open Soup in
  let subst = match name node with
    "table" -> [
        "float","align";
        "background-color","bgcolor";
        "width","width";
        "height","height"
      ]
    | "tr" -> [
        "background-color", "bgcolor";
        "vertical-align", "valign";
        "text-align", "align"
      ]
    | "td" | "th" -> [
            "background-color", "bgcolor";
            "width", "width";
            "height", "height";
            "vertical-align", "valign";
            "text-align", "align";
            "white-space", "nowrap";
      ]
    |"tbody"|"thead"|"tfoot" -> [
            "vertical-align", "valign";
            "text-align", "align";
        ]
    | _ -> [] in
  let styles =
    match subst with
      [] -> styles
    | subst ->
       List.filter (fun (k,v) ->
           match List.assoc_opt k subst with
             Some k' ->  set_attribute k' v node;false
           | None -> true
         ) styles in

  match styles with
    [] -> ()
  | styles ->
     let style = (styles
                  |> List.map (fun (k,v) -> k ^ ":" ^ v)
                  |> String.concat ";")  ^ ";" in
     let style =
       match attribute "style" node with
         Some s -> style ^ s
       | None -> style in
     set_attribute "style" style node

let re_comawspace = Str.regexp "[\x20\x0d\x0a\x09]*,[\x20\x0d\x0a\x09]*"
let re_pseudos =
    ".*\\(" ^ (["hover"; "active"; "focus"; "visited"; "link"] |> List.map (fun s -> ":" ^ s ) |> String.concat "\\|"  ) ^ "\\).*" |> Str.regexp

let apply_style style html =
  let open Soup in
  List.iter (function
        `Rule (selector,styles) ->
         Str.split re_comawspace selector
         |> List.iter (fun selector ->
            if not (Str.string_match re_pseudos selector 0) then
                try
                 html $$ selector |> iter (add_style styles)
                with _ -> "Can't handle selector:'" ^ selector ^ "'" |> failwith
                 )
      | _ -> ()) style;
  html

let clean state html =
  let open Soup in
  select_internal_styles html |> iter delete;
  select_external_styles html |> iter delete;
  if state.clean_class then
    html $$ "*" |> iter (fun n ->
                       delete_attribute "class" n;
                       delete_attribute "id" n;
                     );
  html

let inline_css ?(clean_class=true) ?(load=(fun _ -> None)) html =
  let open Soup in
  let state = {load;clean_class} in
  let html = parse html in
  let ext_styles = get_external_styles html in
  let style = [] in
  let style = load_external_style state ext_styles style in
  let style = load_internal_style html style in
  (* style *)
  html |> apply_style style |> clean state |> pretty_print
