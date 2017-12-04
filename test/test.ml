open Cssinliner

let tests =
  [ "alpha.html"
  ; "cascading.html"
  ; "character-entities.html"
  ; "css-quotes.html"
  ; "direct-descendents.html"
  ; "direct-descendents.html"
  ; "font-quotes.html"
  ; "id.html"
  ; "identical-important.html"
  ; "ignore-pseudos.html"
  ; "important.html"
  ; "in.html"
  ; "malformed.html"
  ; "media.html"
  ; "no_css.html"
  ; "normalize.html"
  ; "preserve-events.html"
  ; "regression-media.html"
  ; "regression-selector-newline.html"
  ; "remote_url.html"
  ; "remove-html-selectors.html"
  ; "spaces_in_path.html"
  ; "specificity.html"
  ; "style-preservation.html"
  ; "table-attr.html"
  ; "tag.html"
  ; "two_styles.html"
  ; "width-attr.html"
  ; "xhtml.html"
  ; "yui-reset.html" ]


let tests_no_clean = ["class+id.html"; "class.html"]

let root = "test/"

let root = Sys.argv.(1) ^ root

let src_dir = root ^ "inline-css/test/fixtures/"

let dst_dir = root ^ "results/"

let write_result file str =
  let ch_out = dst_dir ^ file |> open_out in
  output_string ch_out str ; close_out ch_out


let test_one ~clean_class file =
  let open Soup in
  "Testing: " ^ file |> print_endline ;
  let load (_, file) =
    let uri = Uri.of_string file in
    match Uri.host uri with
    | None -> Some (read_file (src_dir ^ file))
    | Some _ ->
        "Skipping '" ^ file ^ "'" |> print_endline ;
        None
  in
  src_dir ^ file |> read_file |> inline_css ~verbose:true ~clean_class ~load
  |> write_result file


let _ =
  List.iter (test_one ~clean_class:true) tests ;
  List.iter (test_one ~clean_class:false) tests_no_clean


(*
let _ = test_one ~clean_class:false "class+id.html"
let _ = test_one ~clean_class:false "class.html"


(* failed *)
let _ = test_one "doctype.html"

(* failed ?? *)
let _ = test_one "empty.html"
 *)
(*
let _ =
  let src_dir = "data/" in
  let open Soup in
  let css = src_dir ^ "colors.css" |> read_file in
  let open Angstrom in
  let _css_parser = css_parser >>| List.rev in
  Angstrom.parse_only css_parser (`String css)
(*  parse_one_style empty_state [] [] (Some css) *)
 *)
(*
let _ =
  let src_dir = "data/" in
  let file = "TextFormattingRules" in
  let load (_, file) =
    "Loading " ^ file |> print_endline ;
    match List.rev (Str.split (Str.regexp "/") file) with
    | file :: _ -> Some (Soup.read_file (src_dir ^ file))
    | _ -> None
  in
  let h = src_dir ^ file |> Soup.read_file |> inline_css ~verbose:true ~load in
  let open Soup in
  let html = Soup.parse h in
  html $$ "link" |> iter delete ;
  html $$ "script" |> iter delete ;
  html $$ "meta" |> iter delete ;
  html $$ "base" |> iter delete ;
  let h = Soup.pretty_print html in
  write_result (file ^ ".html") h
 *)
