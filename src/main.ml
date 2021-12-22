open Js_of_ocaml
open Mdxish

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

type action = [ Notebook.action | `Run of Codeblock.code_cell ]

let do_action : action -> Notebook.t -> Notebook.t =
 fun a ->
  log "do action";
  match a with
  | #Notebook.action as a -> Notebook.execute a
  | `Run cell -> fun n -> n

open Code_mirror

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let markdown () =
  let md = Jv.get Jv.global "__CM__markdown" in
  Jv.apply md [||] |> Extension.of_jv

let editor content ?parent () =
  let open Editor in
  let config =
    State.Config.create ~doc:(Jstr.of_string content)
      ~extensions:[| basic_setup; markdown () |]
      ()
  in
  let state = State.create ~config () in
  let opts = View.opts ~state ?parent () in
  let view : View.t = View.create ~opts () in
  (View.dom view, state, view)

let get_doc view =
  let text = Editor.State.doc @@ Editor.View.state view in
  Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
  |> String.concat "\n"

let render_code_cell (cell : _ Mdxish.Codeblock.cell) :
    action Note.event * Brr.El.t =
  let open Js_top_worker_rpc in
  let to_out x extra_cls =
    Option.to_list x
    |> List.map (fun text ->
           Brr.(
             El.(
               pre
                 [
                   code
                     ~at:[ At.(class' (Jstr.v "language-ocaml")) ]
                     [
                       span
                         ~at:[ At.(class' (Jstr.v extra_cls)) ]
                         [ txt (Jstr.v text) ];
                     ];
                 ])))
  in
  let o = cell.outputs in
  let outputs =
    match o with
    | Some o ->
        let stdout = to_out o.Toplevel_api_gen.stdout "text-white-500" in
        let stderr = to_out o.Toplevel_api_gen.stderr "text-red-500" in
        let sharp_ppf = to_out o.Toplevel_api_gen.sharp_ppf "text-green-500" in
        let caml_ppf = to_out o.Toplevel_api_gen.caml_ppf "text-blue-500" in
        Brr.El.div ([ stdout; stderr; sharp_ppf; caml_ppf ] |> List.flatten)
    | None -> Brr.El.div []
  in

  let my_button = Brr.(El.(button [ txt (Jstr.v "delete") ])) in
  let el, _, view = editor cell.source () in
  let doc =
    Brr_note.Evr.on_el Brr.Ev.click
      (fun _ ->
        log "click!";
        `Set_cell_source (Codeblock.C cell, "moo"))
      my_button
  in
  (doc, Brr.(El.(div [ el; my_button; outputs ])))

let render_markdown_cell (cell : _ Mdxish.Codeblock.cell) :
    action Note.event * Brr.El.t =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let doc = Omd.of_string cell.source in
  let content = Omd_brr.(to_brr (of_doc doc)) in
  let my_button = Brr.(El.(button [ txt (Jstr.v "delete") ])) in
  let elt = Brr.El.div (content @ [ my_button ]) in
  let ev =
    Brr_note.Evr.on_el Brr.Ev.click
      (fun _ ->
        log "click 2!";
        `Set_cell_source (Codeblock.C cell, "moo"))
      my_button
  in
  let ev' =
    Note.E.map
      (fun x ->
        log "ev'";
        x)
      ev
  in
  (ev', elt)

let render_packed_cell :
    Mdxish.Codeblock.packed_cell -> action Note.event * Brr.El.t = function
  | C ({ cell_type = Code; _ } as c) -> render_code_cell c
  | C ({ cell_type = Markdown; _ } as c) -> render_markdown_cell c

let do_notebook : Notebook.t Note.signal -> action Note.event * Brr.El.t =
 fun n ->
  let open Note in
  let handle_cell cell (evs, elts) =
    let ev, elt = render_packed_cell cell in
    (ev :: evs, elt :: elts)
  in
  let fn n = Notebook.fold handle_cell n ([], []) in
  let items = S.l1 ~eq:( == ) fn n in
  let act = E.swap @@ S.map ~eq:( == ) (fun (evs, _) -> E.select evs) items in
  let items = S.map ~eq:( == ) snd items in
  let container = Brr.El.div [] in
  let () = Brr_note.Elr.def_children container items in
  (act, container)

let get_ok = function Ok r -> r | _ -> failwith "bad"

let ui : notebook:Notebook.t -> Notebook.t Note.signal * Brr.El.t =
 fun ~notebook ->
  let def notebook =
    let action, elt = do_notebook notebook in
    let do_action = Note.E.map do_action action in
    let notebook' = Note.S.accum (Note.S.value notebook) do_action in
    (notebook', (notebook', elt))
  in
  Note.S.fix notebook def

let th =
  let ( let* ) = Lwt.bind in
  let brr_p =
    Brr.Document.find_el_by_id Brr.G.document (Jstr.of_string "output")
    |> Option.get
  in
  match Mdxish.Notebook.init Db2_json.content with
  | Ok l ->
      let* w =
        Mdxish.Topworker.v "src/worker.js" (fun () ->
            Firebug.console##log "erk")
      in
      let* _ = Mdxish.Topworker.start w in
      let notes, elt = ui ~notebook:l in
      Note.Logr.hold
        (Note.S.log notes (fun n ->
             log "%d notes" (List.length n.Notebook.cells)));
      Brr.El.append_children brr_p [ elt ];
      Lwt.return ()
  | Error m ->
      Firebug.console##log (Jv.Error.message m);
      Lwt.fail_with "boo"
