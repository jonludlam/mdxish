open Lwt.Infix

[@@@part "0"]

(* ~~~ Irmin Store ~~~~ *)
module Client = struct
  include Cohttp_lwt_jsoo.Client

  let ctx () = None
end

[@@@part "1"]

module Schema = Irmin.Schema.KV (Irmin.Contents.String)

(* A Git-format store. This data can be exported and used with the regular Git
   tools. It can also read data produced by older versions of irmin-indexeddb. *)
module I =
  Irmin_git.Generic_KV
    (Irmin_indexeddb.Content_store)
    (Irmin_indexeddb.Branch_store)

module Store = I.Make (Irmin.Contents.String)
(* (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String) *)

[@@@part "2"]

(* No ocaml-git server... so using HTTP remote... *)
module Remote = Irmin_http.Client (Client) (Store)
module Sync = Irmin.Sync.Make (Store)

[@@@part "3"]

type t = { main : Store.t; staging : Store.t; uri : Uri.t }

let info message () =
  Store.Info.v ~author:"omditor-client" ~message
    (Unix.gettimeofday () |> Int64.of_float)

let local_commit t k v =
  Store.set ~info:(info "some message goes here") t.staging k v >|= function
  | Ok () -> Brr.Console.log [ Jstr.v "Successful commit" ]
  | Error _ -> Brr.Console.warn [ Jstr.v "Set error" ]

let local_get t k = Store.get t.staging k

[@@@part "4"]

let sync ?(merge = true) t =
  let config =
    Irmin_http.config t.uri (Irmin.Backend.Conf.empty Irmin_http.Conf.spec)
  in
  let main = t.main in
  Remote.Repo.v config >>= fun repo ->
  Remote.main repo >>= fun remote ->
  Sync.pull_exn main ~depth:1 (Irmin.remote_store (module Remote) remote) `Set
  >>= fun _ ->
  if merge then (
    Brr.Console.log [ Jstr.v "Merging" ];
    Store.merge_into ~info:(info "update staging") ~into:t.staging main
    >>= function
    | Ok () -> Lwt.return @@ Ok ()
    | Error (`Conflict _s) ->
        (* Of course in practice we'd be more clever here... *)
        Store.Head.get main >>= fun head ->
        Lwt_result.ok @@ Store.Branch.set (Store.repo t.staging) "staging" head)
  else Lwt_result.return ()

[@@@part "5"]

(* We're only using a one-level hierarchy so this is sufficient *)
let list t =
  Store.list t.staging [] >>= fun lst -> Lwt.return @@ List.map fst lst

let push ?(message = "merge") t =
  let config =
    Irmin_http.config t.uri (Irmin.Backend.Conf.empty Irmin_http.Conf.spec)
  in
  Remote.Repo.v config >>= fun repo ->
  Remote.main repo >>= fun remote ->
  sync ~merge:false t >>= fun _ ->
  let main = t.main in
  Store.merge_into ~info:(info message) ~into:main t.staging >>= fun _ ->
  Sync.push_exn main (Irmin.remote_store (module Remote) remote)

let repo = "/tmp/irmin-adventures"

let compare_commit a b =
  let a = Store.Commit.info a in
  let b = Store.Commit.info b in
  Int64.compare (Remote.Schema.Info.date a) (Remote.Schema.Info.date b)

let init uri =
  let config = Irmin_indexeddb.config "plain_db_name" in
  Store.Repo.v config >>= fun repo ->
  Store.main repo >>= fun main ->
  (* Abusing the API here a little for this one off case... *)
  sync ~merge:false { main; staging = main; uri } >>= fun _ ->
  Store.Branch.find repo "staging" >>= fun commit ->
  match commit with
  | None ->
      Store.clone ~src:main ~dst:"staging" >>= fun staging ->
      Lwt.return { main; staging; uri }
  | Some c ->
      Store.of_branch repo "staging" >>= fun staging ->
      Store.Head.get main >>= fun head ->
      if compare_commit head c < 0 then Lwt.return { main; staging; uri }
      else Lwt.return { main; staging; uri }
