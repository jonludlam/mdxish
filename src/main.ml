open Js_of_ocaml
open Mdxish

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

let render_code_cell (cell : _ Mdxish.Codeblock.cell) o =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let open Js_top_worker_rpc in
  let coerce elt =
    (elt : Html_types.pre Html.elt :> Html_types.flow5 Html.elt)
  in
  let to_out x extra_cls =
    Option.to_list x
    |> List.map (fun text ->
           coerce
             Html.(
               pre
                 [
                   code
                     ~a:[ a_class [ "language-ocaml" ] ]
                     [ span ~a:[ a_class [ extra_cls ] ] [ txt text ] ];
                 ]))
  in
  let stdout = to_out o.Toplevel_api_gen.stdout "text-white-500" in
  let stderr = to_out o.Toplevel_api_gen.stderr "text-red-500" in
  let sharp_ppf = to_out o.Toplevel_api_gen.sharp_ppf "text-green-500" in
  let caml_ppf = to_out o.Toplevel_api_gen.caml_ppf "text-blue-500" in
  To_dom.of_element
    Html.(
      div
        ([
           coerce
           @@ pre
                [ code ~a:[ a_class [ "language-ocaml" ] ] [ txt cell.source ] ];
         ]
        @ stdout @ stderr @ sharp_ppf @ caml_ppf))

let render_markdown_cell (cell : _ Mdxish.Codeblock.cell) =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let doc = Omd.of_string cell.source in
  let content = Omd.to_html doc in
  let elt = To_dom.of_element (Html.div []) in
  elt##.innerHTML := Js_of_ocaml.Js.string content;
  elt

let get_ok = function Ok r -> r | _ -> failwith "bad"

let th =
  let ( let* ) = Lwt.bind in
  let p = Dom_html.getElementById "output" in
  match Mdxish.Codeblock.init Db2_json.content with
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
                let line =
                  Astring.String.cuts ~sep:"\n" cell.source |> List.hd
                in
                let* o = Mdxish.Codeblock.exec w cell in
                let o = get_ok o in
                Dom.appendChild p (render_code_cell cell o);
                log "Executing block starting '%s'" line;
                let* _ = Mdxish.Codeblock.exec w cell in
                Lwt.return ()
            | Codeblock.(C ({ cell_type = Markdown; _ } as cell)) ->
                Dom.appendChild p (render_markdown_cell cell);
                Lwt.return ())
          l
      in
      Lwt.return ()
  | Error m ->
      Firebug.console##log (Jv.Error.message m);
      Lwt.fail_with "boo"
