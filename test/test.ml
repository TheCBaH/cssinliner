open Cssinliner

let css_tests =
  [ "at-namespace"
  ; "colon-space"
  ; "comma-attribute"
  ; "comma-selector-function"
  ; "charset-linebreak"
  ; "charset"
  ; "comma-attribute"
  ; "comment-in"
  ; "comment-url"
  ; "comment"
  ; "custom-media-linebreak"
  ; "custom-media"
  ; "document-linebreak"
  ; "document"
  ; "empty"
  ; "escapes"
  ; "font-face-linebreak"
  ; "font-face"
  ; "hose-linebreak"
  ; "host"
  ; "import-linebreak"
  ; "import-messed"
  ; "import"
  ; "keyframes-advanced"
  ; "keyframes-complex"
  ; "keyframes-linebreak"
  ; "keyframes-messed"
  ; "keyframes-vendor"
  ; "keyframes"
  ; "media-linebreak"
  ; "media-messed"
  ; "media"
  ; "messed-up"
  ; "namespace-linebreak"
  ; "namespace"
  ; "no-semi"
  ; "page-linebreak"
  ; "paged-media"
  ; "props"
  ; "quote-escape"
  ; "quoted"
  ; "rule"
  ; "rules"
  ; "selectors"
  ; "supports-linebreak"
  ; "supports"
  ; "wtf" ]


let css_tests2 =
  [ "alpha"
  ; "cascading"
  ; "character-entities"
  ; "class+id"
  ; "class"
  ; "css-quotes"
  ; "direct-descendents"
  ; "empty"
  ; "file"
  ; "id"
  ; "identical-important"
  ; "ignore-pseudos"
  ; "important"
  ; "malformed"
  ; "media"
  ; "normalize"
  ; "preserve-events"
  ; "regression-media"
  ; "regression-selector-newline"
  ; "remove-html-selectors"
  ; "specificity"
  ; "style-preservation"
  ; "tag"
  ; "two_styles"
  ; "yui-reset" ]


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

let css_src_dir = root ^ "css/test/cases/"

let write_result file str =
  let ch_out = dst_dir ^ file |> open_out in
  output_string ch_out str ; close_out ch_out


let one_css_test get_input test =
  "Testing: " ^ test |> print_endline ;
  let css = get_input test |> Soup.read_file in
  match Css.of_string css with
  | Result.Ok style ->
      Css.sexp_of_t style |> Sexplib.Sexp.to_string_hum
      |> write_result (test ^ ".sexp") ;
      Css.to_string style |> write_result (test ^ ".css")
  | Result.Error x ->
      test ^ " Failed with:" ^ x |> prerr_endline ;
      write_result (test ^ ".sexp") "" ;
      write_result (test ^ ".css") ""


let _ =
  List.iter
    (one_css_test (fun test -> css_src_dir ^ test ^ "/input.css"))
    css_tests


let _ =
  List.iter (one_css_test (fun test -> src_dir ^ test ^ ".css")) css_tests2


let test_one ~clean_class file =
  "Testing: " ^ file |> print_endline ;
  let load (_, file) =
    let uri = Uri.of_string file in
    match Uri.host uri with
    | None -> Some (Soup.read_file (src_dir ^ file))
    | Some _ ->
        "Skipping '" ^ file ^ "'" |> print_endline ;
        None
  in
  src_dir ^ file |> Soup.read_file |> Soup.parse
  |> inline_css ~verbose:true ~apply_table:true ~clean_class ~load
  |> Soup.pretty_print |> write_result file


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
  let h = Soup.to_string html in
  write_result (file ^ ".html") h
 *)
(*
let _ =
  let s = "  /*  */ " in
    Angstrom.parse_only wspace (`String s)


let _ =
  let open Angstrom in
  let rule =
    wspace *> take_while (function ',' | '{' | ';' | '}' -> false | _ -> true)
  in
  let semicolon_line =
    wspace *> take_while (function ';' | '{' | '}' -> false | _ -> true)
    <* char ';' <* wspace
  in
  let block = char '{' *> wspace *> many semicolon_line <* wspace <* char '}' in
  let s = "html body {
        font-size:104%; /* to change the site's font size, change #patternPage below */
        voice-family:\"\\\"}\\\"\";
        voice-family:inherit;
        font-size:small;
           }" in
  let s_ = "html { f:a; }" in
    Angstrom.parse_only css_parser (`String s)

let _ =
  let s = "
th {
        line-height:1.15em;
}
" in
    Angstrom.parse_only css_parser (`String s)

   *)
