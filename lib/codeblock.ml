(* Handling code blocks *)
open Js_of_ocaml
module Html = Dom_html
module TopAPI = Js_top_worker_rpc.Toplevel_api_gen

type code = TopAPI.exec_result
type markdown = string
type _ cellty = Markdown : markdown cellty | Code : code cellty

let log fmt = Format.kasprintf (fun s -> Firebug.console##log (Js.string s)) fmt

module Id : sig
  type t

  val create : unit -> t
  val compare : t -> t -> int
  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = string

  let create () =
    String.init 8 (fun _ -> Char.chr (Random.int 26 + Char.code 'a'))

  let compare = String.compare
  let to_string x = x
  let of_string x = x
end

type 'a cell = {
  cell_type : 'a cellty;
  id : Id.t;
  source : string;
  metadata : (string * string) list;
  outputs : 'a option;
  execution_count : int option;
}

type code_cell = TopAPI.exec_result cell
type md_cell = string cell
type packed_cell = C : 'a cell -> packed_cell

let v ty content metadata =
  {
    cell_type = ty;
    id = Id.create ();
    source = content;
    metadata;
    outputs = None;
    execution_count = None;
  }

let id : packed_cell -> Id.t = function C { id; _ } -> id
let code_of_exec_result : TopAPI.exec_result -> code = fun x -> x
let markdown_of_string : string -> markdown = fun x -> x

let set_source : 'a. string -> 'a cell -> 'a cell =
 fun source cell -> { cell with source; outputs = None; execution_count = None }

let set_outputs : 'a. 'a -> int -> 'a cell -> 'a cell =
 fun outputs execution_count cell ->
  { cell with outputs = Some outputs; execution_count = Some execution_count }

let set_metadata : 'a. string -> string -> 'a cell -> 'a cell =
 fun k v cell -> { cell with metadata = (k, v) :: cell.metadata }

let of_json c =
  let cell_type_opt = Jv.find c "cell_type" in
  let source_opt = Jv.find c "source" in
  (* let outputs = Jv.find c "outputs" in *)
  let metadata = Jv.find c "metadata" in
  let metadata_props = [ "skip" ] in
  let metadata =
    match metadata with
    | None ->
        log "No metadata";
        []
    | Some m ->
        List.map
          (fun prop ->
            log "looking for %s" prop;
            Jv.find m prop
            |> Option.map (fun s ->
                   log "Found it!";
                   (prop, Jv.to_string s)))
          metadata_props
        |> List.filter_map (fun x -> x)
  in
  (* let execution_count = Jv.find c "execution_count" in *)
  match
    (Option.map Jv.to_string cell_type_opt, Option.map Jv.to_string source_opt)
  with
  | Some "code", Some source ->
      let cell = v Code source metadata in
      Some (C cell)
  | Some "markdown", Some source ->
      let cell = v Markdown source metadata in
      Some (C cell)
  | _, _ -> None

type exec_err = [ `TopAPI of TopAPI.err | `Msg of string ]

let exec : type a. Topworker.t -> a cell -> (a, exec_err) Lwt_result.t =
 fun w cell ->
  match cell.cell_type with
  | Code -> (
      match List.assoc_opt "skip" cell.metadata with
      | None ->
          Lwt.(
            bind (Topworker.exec w cell.source) (function
              | Ok x -> return (Ok x)
              | Error x -> return (Error (`TopAPI x))))
      | Some _ -> Lwt.return (Error (`Msg "Not for execution")))
  | Markdown ->
      let doc = Omd.of_string cell.source in
      let content = Omd.to_html doc in
      Lwt.return (Ok content)
