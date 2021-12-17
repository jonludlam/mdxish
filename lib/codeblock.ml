(* Handling code blocks *)
open Js_of_ocaml
module Html = Dom_html
module TopAPI = Js_top_worker_rpc.Toplevel_api_gen

type _ cellty =
  | Markdown : unit cellty
  | Code : (TopAPI.exec_result, TopAPI.err) Result.t cellty

type 'a cell = {
  cell_type : 'a cellty;
  source : string;
  metadata : (string * string) list;
  outputs : string option;
  execution_count : int option;
}

type packed_cell = C : 'a cell -> packed_cell
type t = { cells : packed_cell list }

let init (json : string) =
  let decode_cell c =
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
            outputs = Option.map Jv.to_string outputs;
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
  in
  let jstr = Jstr.of_string json in
  match Brr.Json.decode jstr with
  | Error e -> Error e
  | Ok j -> (
      match Jv.find j "cells" with
      | Some v -> Ok (List.filter_map (fun x -> x) (Jv.to_list decode_cell v))
      | None -> Ok [])

let exec : type a. Topworker.t -> a cell -> a Lwt.t =
 fun w cell ->
  match cell.cell_type with
  | Code -> Topworker.exec w cell.source
  | Markdown -> Lwt.return ()
