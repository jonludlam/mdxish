(* Handling code blocks *)
open Js_of_ocaml
module Html = Dom_html
module TopAPI = Js_top_worker_rpc.Toplevel_api_gen

type _ cellty =
  | Markdown : string cellty
  | Code : TopAPI.exec_result cellty


type 'a cell = {
  cell_type : 'a cellty;
  source : string;
  metadata : (string * string) list;
  outputs : 'a option;
  execution_count : int option;
}

type code_cell = TopAPI.exec_result cell
type md_cell = string cell

type packed_cell = C : 'a cell -> packed_cell

let v content =
  C
    {
      cell_type = Code;
      source = content;
      metadata = [];
      outputs = None;
      execution_count = None;
    }

let set_source : 'a. string -> 'a cell -> 'a cell = fun source cell ->
  { cell with source; outputs = None; execution_count = None }

let set_outputs : 'a. 'a -> int -> 'a cell -> 'a cell = fun outputs execution_count cell ->
  {
    cell with
    outputs = Some outputs;
    execution_count = Some execution_count;
  }

let of_json c =
  let cell_type_opt = Jv.find c "cell_type" in
  let source_opt = Jv.find c "source" in
  let outputs = Jv.find c "outputs" in
  let metadata = Jv.find c "metadata" in
  let metadata_props = [ "skip" ] in
  let metadata =
    match metadata with
    | None -> []
    | Some m ->
        List.map
          (fun prop ->
            Jv.find m prop |> Option.map (fun s -> (prop, Jv.to_string s)))
          metadata_props
        |> List.filter_map (fun x -> x)
  in
  let execution_count = Jv.find c "execution_count" in
  match
    (Option.map Jv.to_string cell_type_opt, Option.map Jv.to_string source_opt)
  with
  | Some "code", Some source ->
      let cell =
        {
          cell_type = Code;
          source;
          metadata;
          outputs = None;
          execution_count = Option.map Jv.to_int execution_count;
        }
      in
      Some (C cell)
  | Some "markdown", Some source ->
      let cell =
        {
          cell_type = Markdown;
          source;
          metadata;
          outputs = Option.map Jv.to_string outputs;
          execution_count = Option.map Jv.to_int execution_count;
        }
      in
      Some (C cell)
  | _, _ -> None

let exec : type a. Topworker.t -> a cell -> a cell Lwt.t =
 fun w cell ->
  let open Lwt.Infix in
  match cell.cell_type with
  | Code -> (
    Topworker.exec w cell.source >>= fun res ->
    match res with
    | Ok res ->
      Lwt.return (set_outputs res 0 cell)
    | Error _err ->
      Lwt.return (cell)
    )
  | Markdown ->
    let doc = Omd.of_string cell.source in
    let content = Omd.to_html doc in
    Lwt.return (set_outputs content 0 cell)
