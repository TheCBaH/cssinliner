(*
#use "topfind";;
#require "cohttp";;
#require "cohttp-lwt-unix";;
#require "angstrom";;
*)

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Cssinliner

let debug f = Logs_lwt.debug f

let client uri meth' =
  debug (fun d -> d "Client with URI %s" (Uri.to_string uri))
  >>= fun () ->
  let meth = Cohttp.Code.method_of_string meth' in
  debug (fun d -> d "Client %s issued" meth')
  >>= fun () ->
  let headers = Header.init () in
  let headers = Header.add headers "user-agent" Header.user_agent in
  Client.call ~headers meth uri
  >>= fun (resp, body) ->
  let status = Response.status resp in
  debug (fun d ->
      d "Client %s returned: %s" meth' (Code.string_of_status status) )
  >>= fun () ->
  (* TODO follow redirects *)
  match Code.is_success (Code.code_of_status status) with
  | false ->
      prerr_endline (Code.string_of_status status) ;
      Cohttp_lwt.Body.drain_body body
      >|= fun () -> Result.Error (Code.string_of_status status)
  | true ->
      Cohttp_lwt.Body.length body
      >>= fun (len, body) ->
      debug (fun d -> d "Client body length: %Ld" len)
      >>= fun () -> Cohttp_lwt.Body.to_string body >|= fun s -> Result.Ok s


let get verbose uri =
  let meth = "GET" in
  match
    Lwt_main.run
      ( ( if verbose then debug (fun d -> d ">>> Debug active")
            >>= fun () -> return ()
        else return () )
      >>= fun () -> client uri meth )
  with
  | Result.Ok s -> Some s
  | Result.Error _ -> None


let _ =
  let url = "http://twiki.org/cgi-bin/view/TWiki/TextFormattingRules" in
  let uri = Uri.of_string url in
  let load (_, link) =
    let link_uri = Uri.of_string link |> Uri.resolve "http" uri in
    get true link_uri
  in
  match get true uri with
  | Some s ->
      let open Soup in
      let html = Soup.parse s |> inline_css ~verbose:true ~load in
      html $$ "link" |> iter delete ;
      html $$ "script" |> iter delete ;
      html $$ "meta" |> iter delete ;
      html $$ "base" |> iter delete ;
      pretty_print html |> write_file ("test" ^ ".html")
  | None -> ()

