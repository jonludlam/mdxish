(* Worker *)

open Js_top_worker_rpc
open Js_of_ocaml

(* open Brr_io *)
open Lwt
module Worker = Brr_webworkers.Worker
module Rpc_lwt = Idl.Make (Lwt)
module Toprpc = Js_top_worker_rpc.Toplevel_api_gen.Make (Rpc_lwt.GenClient ())

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

(* Handy infix bind-style operator for the RPCs *)
let rpc_bind x f =
  x |> Rpc_lwt.T.get >>= function
  | Ok x -> f x
  | Error (Toplevel_api_gen.InternalError s) ->
      log "Rpc failure: %s" s;
      Lwt.fail (Failure (Printf.sprintf "Rpc failure: %s" s))

type t = {
  worker : Worker.t;
  context : Js_top_worker_client.Worker_rpc.context;
  mutable exec_count : int;
  rpc : Rpc.call -> Rpc.response Lwt.t;
}

let timeout_container worker callback () =
  Worker.terminate worker;
  let _ = callback () in
  ()

let v s callback =
  let ( let* ) = rpc_bind in

  let worker =
    try Worker.create (Jstr.v s)
    with Jv.Error _ -> failwith "Failed to created worker"
  in
  let context =
    Js_top_worker_client.Worker_rpc.start worker 100000
      (timeout_container worker callback)
  in
  let rpc = Js_top_worker_client.Worker_rpc.rpc context in
  let* _ = Toprpc.init rpc [] [] in
  Lwt.return { worker; context; rpc; exec_count = 0 }

let start context =
  let ( let* ) = rpc_bind in
  Firebug.console##log (Js.string "doing...");
  let* o = Toprpc.setup context.rpc () in
  Firebug.console##log (Js.string "done...");
  Option.iter (fun s -> log "stdout: %s" s) o.stdout;
  Option.iter (fun s -> log "stderr: %s" s) o.stderr;
  Option.iter (fun s -> log "sharp_ppf: %s" s) o.sharp_ppf;
  Option.iter (fun s -> log "caml_ppf: %s" s) o.caml_ppf;

  Lwt.return o

let exec context txt =
  let ( let* ) = rpc_bind in
  let* o = Toprpc.exec context.rpc txt in
  Option.iter (fun s -> log "stdout: %s" s) o.stdout;
  Option.iter (fun s -> log "stderr: %s" s) o.stderr;
  Option.iter (fun s -> log "sharp_ppf: %s" s) o.sharp_ppf;
  Option.iter (fun s -> log "caml_ppf: %s" s) o.caml_ppf;
  Lwt.return (Ok o)
