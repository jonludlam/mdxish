open Js_of_ocaml
open Mdxish
open Brr
open Brr_note

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

type action = [ Notebook.action | `Run of string * Codeblock.code_cell ]

let button_class =
  List.map
    (fun x -> Brr.At.class' (Jstr.v x))
    [
      "inline-flex";
      "items-center";
      "px-2.5";
      "py-1.5";
      "border";
      "border-transparent";
      "text-xs";
      "font-medium";
      "rounded";
      "shadow-sm";
      "text-white";
      "bg-indigo-600";
      "hover:bg-indigo-700";
      "focus:outline-none";
      "focus:ring-2";
      "focus:ring-offset-2";
      "focus:ring-indigo-500";
    ]

let do_action :
    Topworker.t -> action Note.E.send -> action -> Notebook.t -> Notebook.t =
 fun top sender a v ->
  log "action: before=%d" (List.length v.cells);
  let result =
    match a with
    | #Notebook.action as a -> Notebook.execute a v
    | `Run (contents, cell) ->
        let n' = Notebook.execute (`Set_cell_source (cell.id, contents)) v in
        Lwt.(
          async (fun () ->
              Codeblock.exec top (Codeblock.set_source contents cell)
              >>= fun o ->
              (match o with
              | Ok o ->
                  sender
                    (`Set_code_outputs
                      (cell.id, Codeblock.code_of_exec_result o, 0))
              | Error (`Msg m) -> log "Error executing: %s" m);
              Lwt.return ()));
        n'
  in
  log "      after=%d" (List.length result.cells);
  result

open Code_mirror

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let markdown () =
  let md = Jv.get Jv.global "__CM__markdown" in
  Jv.apply md [||] |> Extension.of_jv

let editor content ?parent () =
  let open Editor in
  let config =
    State.Config.create ~doc:(Jstr.v content)
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

let render_code_cell (cell : _ Mdxish.Codeblock.cell) : action Note.event * El.t
    =
  let open Js_top_worker_rpc in
  let to_out x extra_cls =
    Option.to_list x
    |> List.map (fun text ->
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
               ]))
  in
  let o = cell.outputs in
  let outputs =
    match o with
    | Some o ->
        let stdout = to_out o.Toplevel_api_gen.stdout "text-white-500" in
        let stderr = to_out o.Toplevel_api_gen.stderr "text-red-500" in
        (* let sharp_ppf = to_out o.Toplevel_api_gen.sharp_ppf "text-green-500" in *)
        let caml_ppf = to_out o.Toplevel_api_gen.caml_ppf "text-blue-500" in
        El.div ([ stdout; stderr; caml_ppf ] |> List.flatten)
    | None -> El.div []
  in

  let run_button = El.(button ~at:button_class [ txt (Jstr.v "run") ]) in
  let add_cell_button = El.(button ~at:button_class [ txt (Jstr.v "add") ]) in
  let el, _, view = editor cell.source () in
  let run_evt =
    Evr.on_el Ev.click (fun _ -> `Run (get_doc view, cell)) run_button
  in

  let add_evt =
    Evr.on_el Ev.click (fun _ -> `Add_cell cell.id) add_cell_button
  in
  ( Note.E.select [ run_evt; add_evt ],
    El.(div [ el; run_button; outputs; add_cell_button ]) )

let render_markdown_cell (cell : _ Mdxish.Codeblock.cell) :
    action Note.event * El.t =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let doc = Omd.of_string cell.source in
  let content = Omd_brr.(to_brr (of_doc doc)) in
  let my_button =
    El.(
      button
        ~at:[ At.class' (Jstr.v "bg-indigo-600") ]
        [ txt (Jstr.v "delete") ])
  in
  let elt = El.div (content @ [ my_button ]) in
  let ev = Evr.on_el Ev.click (fun _ -> `Remove_cell cell.id) my_button in
  (ev, elt)

let render_packed_cell :
    Mdxish.Codeblock.packed_cell -> action Note.event * El.t = function
  | C ({ cell_type = Code; _ } as c) -> render_code_cell c
  | C ({ cell_type = Markdown; _ } as c) -> render_markdown_cell c

let do_notebook : Notebook.t Note.signal -> action Note.event * El.t =
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
  let container = El.div [] in
  let () = Elr.def_children container items in
  (act, container)

let get_ok = function Ok r -> r | _ -> failwith "bad"

let ui :
    topworker:Topworker.t ->
    notebook:Notebook.t ->
    Notebook.t Note.signal * El.t =
 fun ~topworker ~notebook ->
  let def notebook =
    let a, sender = Note.E.create () in
    let action, elt = do_notebook notebook in
    let all_actions = Note.E.select [ action; a ] in
    let do_action = Note.E.map (do_action topworker sender) all_actions in
    let notebook' = Note.S.accum (Note.S.value notebook) do_action in
    (notebook', (notebook', elt))
  in
  Note.S.fix notebook def

let th =
  let ( let* ) = Lwt.bind in
  let brr_p =
    Document.find_el_by_id G.document (Jstr.v "output") |> Option.get
  in
  let el =
    Brr.El.div
      ~at:[ Brr.At.class' (Jstr.v "foo"); Brr.At.class' (Jstr.v "bar") ]
      [ Brr.El.txt' "foo" ]
  in
  El.append_children brr_p [ el ];
  match Mdxish.Notebook.init Db2_json.content with
  | Ok l ->
      let* w =
        Mdxish.Topworker.v "src/worker.js" (fun () ->
            Firebug.console##log "erk")
      in
      let* _ = Mdxish.Topworker.start w in
      let notes, elt = ui ~topworker:w ~notebook:l in
      Note.Logr.hold
        (Note.S.log notes (fun n ->
             log "%d notes" (List.length n.Notebook.cells)));
      El.append_children brr_p [ elt ];
      Lwt.return ()
  | Error m ->
      Firebug.console##log (Jv.Error.message m);
      Lwt.fail_with "boo"
