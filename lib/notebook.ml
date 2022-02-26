(* Notebook *)

module TopAPI = Js_top_worker_rpc.Toplevel_api_gen

type t = { cells : Codeblock.packed_cell list }

type action =
  [ `Add_cell of Codeblock.Id.t
  | `Remove_cell of Codeblock.Id.t
  | `Set_cell_source of Codeblock.Id.t * string
  | `Set_code_outputs of Codeblock.Id.t * Codeblock.code * int
  | `Set_markdown_outputs of Codeblock.Id.t * Codeblock.markdown * int
  | `Modify_metadata of Codeblock.Id.t * string * string ]

let modify :
    Codeblock.Id.t ->
    (Codeblock.packed_cell -> Codeblock.packed_cell list) ->
    t ->
    t =
 fun id fn n ->
  let rec inner = function
    | c :: cs when Codeblock.id c = id -> fn c @ cs
    | c :: cs -> c :: inner cs
    | [] -> raise Not_found
  in
  { cells = inner n.cells }

let execute : action -> t -> t =
 fun a v ->
  match a with
  | `Add_cell after ->
      let new_cell = Codeblock.C (Codeblock.v Code "\n\n" []) in
      modify after (fun c -> [ c; new_cell ]) v
  | `Set_cell_source (cell, source) ->
      modify cell (function C c -> [ C (Codeblock.set_source source c) ]) v
  | `Set_code_outputs (cell, outputs, execution_count) ->
      modify cell
        (function
          | C ({ cell_type = Code; _ } as c) ->
              [ C (Codeblock.set_outputs outputs execution_count c) ]
          | _ -> [])
        v
  | `Set_markdown_outputs (cell, outputs, execution_count) ->
      modify cell
        (function
          | C ({ cell_type = Markdown; _ } as c) ->
              [ C (Codeblock.set_outputs outputs execution_count c) ]
          | _ -> [])
        v
  | `Remove_cell id -> modify id (function _ -> []) v
  | `Modify_metadata (id, k, v') ->
      modify id (function C c -> [ C (Codeblock.set_metadata k v' c) ]) v

let init (json : string) =
  let jstr = Jstr.of_string json in
  match Brr.Json.decode jstr with
  | Error e -> Error e
  | Ok j -> (
      match Jv.find j "cells" with
      | Some v ->
          Ok
            {
              cells =
                List.filter_map (fun x -> x) (Jv.to_list Codeblock.of_json v);
            }
      | None -> Ok { cells = [] })

let fold : (Codeblock.packed_cell -> 'a -> 'a) -> t -> 'a -> 'a =
 fun fn notebook init -> List.fold_right fn notebook.cells init
