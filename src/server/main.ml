(* ~~~ Irmin Store ~~~ *)
open Lwt.Syntax
open Cohttp_lwt_unix

let info = Irmin_unix.info

[@@@part "0"]

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Sync = Irmin.Sync.Make (Store)
module Http = Irmin_http.Server (Cohttp_lwt_unix.Server) (Store)

[@@@part "1"]

let repo = "/tmp/irmin-adventures"

[@@@part "2"]

let store () =
  let config = Irmin_git.config ~bare:true repo in
  let* repo = Store.Repo.v config in
  let* t = Store.main repo in
  let* () =
    Store.set_exn ~info:(info "commit 1") t [ "db5.json" ] Db5_json.content
  in
  let* () =
    Store.set_exn ~info:(info "commit 2") t [ "db2.json" ] Db2_json.content
  in
  let+ () = Store.set_exn ~info:(info "commit 3") t [ "hola.md" ] "# Hola!" in
  repo

[@@@part "3"]

let callback repo conn req body =
  let uri = Cohttp.Request.resource req in
  match uri with
  | "" | "/" | "/index.html" ->
      Server.respond_string ~status:`OK ~body:Html.html ()
  | "/static/index.js" ->
      let headers =
        Cohttp.Header.add_opt None "Content-type" "text/javascript"
      in
      Server.respond_string ~status:`OK ~headers
        ~body:(Assets.read "index.js" |> Option.get)
        ()
  | "/static/worker.js" ->
      let headers =
        Cohttp.Header.add_opt None "Content-type" "text/javascript"
      in
      Server.respond_string ~status:`OK ~headers
        ~body:(Assets.read "worker.js" |> Option.get)
        ()
  | "/static/tailwind.css" ->
      let headers = Cohttp.Header.add_opt None "Content-type" "text/css" in
      Server.respond_string ~status:`OK ~headers
        ~body:(Assets.read "tailwind.css" |> Option.get)
        ()
  | _irmin_path -> Http.callback repo conn req body

[@@@part "4"]

let serve repo = Server.create (Server.make ~callback:(callback repo) ())

let main () =
  Format.printf "Starting server\n%!";
  let* repo = store () in
  serve repo

let () = Lwt_main.run @@ main ()
