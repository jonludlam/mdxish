open Js_of_ocaml
open Mdxish
module URI = Uri
open Brr
open Brr_note

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

type action = [ Notebook.action | `Run of string * Codeblock.code_cell ]

let button_class = List.map (fun x -> Brr.At.class' (Jstr.v x)) []

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
              | Error (`TopAPI (InternalError s)) -> log "Error executing: %s" s
              | Error (`Msg m) -> log "Error: %s" m);
              Lwt.return ()));
        n'
  in
  log "      after=%d" (List.length result.cells);
  result

open Code_mirror

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let dark_theme_ext () =
  let dark = Jv.get Jv.global "__CM__dark" in
  Code_mirror.Extension.of_jv @@ Jv.get dark "oneDark"

let ml_like () = Jv.get Jv.global "__CM__mllike" |> Code_mirror.Language.of_jv

let markdown () =
  let md = Jv.get Jv.global "__CM__markdown" in
  Jv.apply md [||] |> Extension.of_jv

let editor content ?parent () =
  let open Editor in
  let ml = Stream.Language.define (ml_like ()) in
  let config =
    State.Config.create ~doc:(Jstr.v content)
      ~extensions:[| basic_setup; dark_theme_ext (); ml |]
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

(*
      <div class="mb-5">
       <div class=" border-b">
         <div class="bg-gray-800 text-gray-100 p-2 pt-3 rounded-t-md text-sm flex">
          <div class="w-full">
           <pre>moo a</pre>
          </div>
           <div class=""><button class=" text-blue-300 px-2"><svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
             <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM9.555 7.168A1 1 0 008 8v4a1 1 0 001.555.832l3-2a1 1 0 000-1.664l-3-2z" clip-rule="evenodd" />
             </svg></button>
           </div>
           <div><button class="text-blue-300 px-2"><svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
     <path d="M5 4a1 1 0 00-2 0v7.268a2 2 0 000 3.464V16a1 1 0 102 0v-1.268a2 2 0 000-3.464V4zM11 4a1 1 0 10-2 0v1.268a2 2 0 000 3.464V16a1 1 0 102 0V8.732a2 2 0 000-3.464V4zM16 3a1 1 0 011 1v7.268a2 2 0 010 3.464V16a1 1 0 11-2 0v-1.268a2 2 0 010-3.464V4a1 1 0 011-1z" />
   </svg>
           </div>
         </div>
       </div>
       <div class="bg-gray-800 text-gray-100 p-2 rounded-b-md text-sm">
         <pre>moo</pre>
       </div>
     </div>
*)

module Svg = struct
  let global_document = Jv.get Jv.global "document"

  let set_at e (a, v) =
    match Jstr.equal a At.Name.class' with
    | false -> ignore (Jv.call e "setAttribute" Jv.[| of_jstr a; of_jstr v |])
    | true when Jstr.is_empty v -> ()
    | true -> ignore (Jv.call (Jv.get e "classList") "add" [| Jv.of_jstr v |])

  let v ?(d = global_document) ?(at = []) name cs =
    let e =
      Jv.call d "createElementNS"
        [| Jv.of_jstr (Jstr.v "http://www.w3.org/2000/svg"); Jv.of_jstr name |]
    in
    List.iter (set_at e) at;
    let e = (Obj.magic e : Brr.El.t) in
    Brr.El.append_children e cs;
    (Obj.magic e : Brr.El.t)

  let atv x y = (x, y)
end

let mk_svg path () =
  let open Svg in
  v (Jstr.v "svg")
    ~at:
      [
        atv (Jstr.v "viewBox") (Jstr.v "0 0 20 20");
        atv At.Name.class' (Jstr.v "h-5");
        atv At.Name.class' (Jstr.v "w-5");
        atv (Jstr.v "fill") (Jstr.v "currentColor");
      ]
    [
      v (Jstr.v "path")
        ~at:
          [
            atv (Jstr.v "fill-rule") (Jstr.v "evenodd");
            atv (Jstr.v "d") (Jstr.v path);
            atv (Jstr.v "clip-rule") (Jstr.v "evenodd");
          ]
        [];
    ]

let run_svg =
  mk_svg
    "M10 18a8 8 0 100-16 8 8 0 000 16zM9.555 7.168A1 1 0 008 8v4a1 1 0 \
     001.555.832l3-2a1 1 0 000-1.664l-3-2z"

let opt_svg =
  mk_svg
    "M5 4a1 1 0 00-2 0v7.268a2 2 0 000 3.464V16a1 1 0 102 0v-1.268a2 2 0 \
     000-3.464V4zM11 4a1 1 0 10-2 0v1.268a2 2 0 000 3.464V16a1 1 0 102 \
     0V8.732a2 2 0 000-3.464V4zM16 3a1 1 0 011 1v7.268a2 2 0 010 3.464V16a1 1 \
     0 11-2 0v-1.268a2 2 0 010-3.464V4a1 1 0 011-1z"

let render_code_cell (cell : _ Mdxish.Codeblock.cell) : action Note.event * El.t
    =
  let open Js_top_worker_rpc in
  let skip = List.mem_assoc "skip" cell.metadata in

  let to_out x extra_cls =
    Option.to_list x
    |> List.map (fun text ->
           El.(
             pre
               ~at:[ At.(class' (Jstr.v "")) ]
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
  let extra_cls =
    List.map
      (fun c -> At.class' (Jstr.v c))
      [ "bg-gray-800"; "text-gray-100"; "p-2"; "rounded-b-md"; "text-sm" ]
  in
  let outputs =
    match o with
    | Some o ->
        let stdout = to_out o.Toplevel_api_gen.stdout "text-white-500" in
        let stderr = to_out o.Toplevel_api_gen.stderr "text-red-500" in
        (* let sharp_ppf = to_out o.Toplevel_api_gen.sharp_ppf "text-green-500" in *)
        let caml_ppf = to_out o.Toplevel_api_gen.caml_ppf "text-blue-500" in
        El.div ~at:extra_cls ([ stdout; stderr; caml_ppf ] |> List.flatten)
    | None -> El.div []
  in

  let run_button = El.(button ~at:button_class [ run_svg () ]) in
  let add_cell_button = El.(button ~at:button_class [ opt_svg () ]) in

  let editor_el, _, view = editor cell.source () in
  let run_evt =
    Evr.on_el Ev.click (fun _ -> `Run (get_doc view, cell)) run_button
  in

  let add_evt =
    Evr.on_el Ev.click (fun _ -> `Add_cell cell.id) add_cell_button
  in
  let cls classes = List.map (fun c -> At.class' (Jstr.v c)) classes in
  let buttons =
    if not skip then
      El.
        [
          div ~at:(cls [ "flex-grow"; "overflow-x-auto" ]) [ editor_el ];
          div ~at:(cls [ "flex-none" ]) [ run_button ];
          div ~at:(cls [ "flex-none" ]) [ add_cell_button ];
        ]
    else El.[ div ~at:(cls [ "flex-grow"; "overflow-x-auto" ]) [ editor_el ] ]
  in
  let outputs =
    if skip then []
    else
      El.
        [
          div
            ~at:
              (cls
                 [
                   "bg-gray-800";
                   "text-gray-100";
                   "p-2";
                   "rounded-b-md";
                   "text-sm";
                 ])
            [ outputs ];
        ]
  in
  ( Note.E.select [ run_evt; add_evt ],
    El.(
      div
        ~at:(cls [ "mb-5" ])
        ([
           div
             ~at:(cls [ "border-b" ])
             [
               div
                 ~at:
                   (cls
                      [
                        "bg-gray-800";
                        "text-gray-100";
                        "p-2";
                        "pt-3";
                        (if skip then "rounded-md" else "rounded-t-md");
                        "text-sm";
                        "flex";
                      ])
                 buttons;
             ];
         ]
        @ outputs)) )

let render_markdown_cell (cell : _ Mdxish.Codeblock.cell) :
    action Note.event * El.t =
  let doc = Omd.of_string cell.source in
  let content = Omd_brr.(to_brr (of_doc doc)) in
  let my_button =
    El.(
      button
        ~at:[ At.class' (Jstr.v "bg-indigo-600") ]
        [ txt (Jstr.v "delete") ])
  in
  let elt = El.div content (*@ [ my_button ]*) in
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
  let ( let* ) = Lwt_result.bind in
  let ( let** ) = Lwt.bind in
  let** store = Store.init (URI.of_string "http://localhost:8080") in
  let* () = Store.sync store in
  let brr_p =
    Document.find_el_by_id G.document (Jstr.v "output") |> Option.get
  in
  let** l = Store.list store in
  List.iter (fun l -> log "Got %s" l) l;
  let** _ = Store.push store in
  let** db5 = Store.local_get store [ "db5.json" ] in

  (* let el =
       Brr.El.div
         ~at:[ Brr.At.class' (Jstr.v "foo"); Brr.At.class' (Jstr.v "bar") ]
         [ Brr.El.txt' "foo" ]
     in
     El.append_children brr_p [ el ]; *)
  match Mdxish.Notebook.init db5 with
  | Ok l ->
      let* w =
        Mdxish.Topworker.v "static/worker.js" (fun () ->
            Firebug.console##log "erk")
      in
      let* _ = Mdxish.Topworker.start w in
      let notes, elt = ui ~topworker:w ~notebook:l in
      Note.Logr.hold
        (Note.S.log notes (fun n ->
             log "%d notes" (List.length n.Notebook.cells)));
      El.append_children brr_p [ elt ];
      Lwt.return (Ok ())
  | Error m ->
      Firebug.console##log (Jv.Error.message m);
      Lwt.fail_with "boo"
