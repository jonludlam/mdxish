open Js_of_ocaml
open Mdxish

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

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
  View.dom view, state, view

let get_doc view = 
  let text = Editor.State.doc @@ Editor.View.state view in
  Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list |> String.concat "\n"

let render_code_cell (cell : _ Mdxish.Codeblock.cell) parent =
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
                         ~at:[At.(class' (Jstr.v extra_cls)) ]
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

  let my_button = Brr.(El.(button [ txt (Jstr.v "run") ])) in
  let el, _, view = editor cell.source ~parent () in
  let doc = Brr_note.Evr.on_el Brr.Ev.click (fun _ -> get_doc view) my_button in
  Brr.(El.(div (el :: my_button :: outputs :: []))), doc

let render_markdown_cell (cell : _ Mdxish.Codeblock.cell) =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let doc = Omd.of_string cell.source in
  let content = Omd_brr.(to_brr (of_doc doc)) in
  let elt = Brr.El.div content in
  elt

let get_ok = function Ok r -> r | _ -> failwith "bad"

let th =
  let ( let* ) = Lwt.bind in
  let brr_p =
    Brr.Document.find_el_by_id Brr.G.document (Jstr.of_string "output")
    |> Option.get
  in
  match Mdxish.Notebook.init Db2_json.content with
  | Ok l ->
      Firebug.console##log
        (Js.string (Printf.sprintf "Got %d entries" (List.length l)));
      let* w =
        Mdxish.Topworker.v "src/worker.js" (fun () ->
            Firebug.console##log "erk")
      in
      Firebug.console##log (Js.string "worker made");
      let* _ = Mdxish.Topworker.start w in
      let* () =
        Lwt_list.iter_s
          (fun c ->
            match c with
            | Codeblock.(C ({ cell_type = Code; _ } as cell)) ->
                (* let* o =
                  match List.assoc_opt "skip" cell.metadata with
                  | None ->
                      let* o = Mdxish.Codeblock.exec w cell in
                      Lwt.return (Some (get_ok o))
                  | Some _ ->
                      log "Skipping!";
                      Lwt.return None
                in *)
                let elt, doc = render_code_cell cell brr_p in
                Brr.El.append_children brr_p [ elt ];
                Note.Logr.(may_hold @@ Note.E.log doc (fun s -> Brr.Console.(log [str s])));
                Lwt.return ()
            | Codeblock.(C ({ cell_type = Markdown; _ } as cell)) ->
                Brr.El.append_children brr_p [render_markdown_cell cell];
                Lwt.return ())
          l
      in
      Lwt.return ()
  | Error m ->
      Firebug.console##log (Jv.Error.message m);
      Lwt.fail_with "boo"
