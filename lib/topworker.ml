(* Worker *)

open Js_top_worker_client
open Js_of_ocaml

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

type t = { mutable exec_count : int; rpc : rpc }

let v s callback =
  let ( let* ) = Lwt_result.bind in

  let rpc = start s 100000 callback in
  let* _ =
    W.init rpc Js_top_worker_rpc.Toplevel_api_gen.{ cmas = []; cmi_urls = [] }
  in
  Lwt.return (Ok { rpc; exec_count = 0 })

let start context =
  let ( let* ) = Lwt_result.bind in
  let* o = W.setup context.rpc () in
  Option.iter (fun s -> log "stdout: %s" s) o.stdout;
  Option.iter (fun s -> log "stderr: %s" s) o.stderr;
  Option.iter (fun s -> log "sharp_ppf: %s" s) o.sharp_ppf;
  Option.iter (fun s -> log "caml_ppf: %s" s) o.caml_ppf;

  Lwt.return (Ok o)

let exec context txt =
  let ( let* ) = Lwt_result.bind in
  let* o = W.exec context.rpc txt in
  Option.iter (fun s -> log "stdout: %s" s) o.stdout;
  Option.iter (fun s -> log "stderr: %s" s) o.stderr;
  Option.iter (fun s -> log "sharp_ppf: %s" s) o.sharp_ppf;
  Option.iter (fun s -> log "caml_ppf: %s" s) o.caml_ppf;
  Lwt.return (Ok o)
