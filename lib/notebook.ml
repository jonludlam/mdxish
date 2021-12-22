(* Notebook *)

module TopAPI = Js_top_worker_rpc.Toplevel_api_gen

type t = { cells : Codeblock.packed_cell list }

type action =
  [ `Add_cell of Codeblock.packed_cell
  | `Set_cell_source of Codeblock.packed_cell * string
  | `Set_code_outputs of Codeblock.code_cell * TopAPI.exec_result * int
  | `Set_markdown_outputs of Codeblock.md_cell * string * int ]

let execute : action -> t -> t =
 fun a v ->
  match a with
  | `Add_cell after ->
      let new_cell = Codeblock.v "" in
      let rec inner = function
        | c :: cs when c = after -> c :: new_cell :: cs
        | c :: cs -> c :: inner cs
        | [] -> [ new_cell ]
      in
      { cells = inner v.cells }
  | `Set_cell_source (cell, source) ->
      let rec inner = function
        | c :: cs when c = cell -> (
            match c with
            | C cell -> Codeblock.C (Codeblock.set_source source cell) :: cs)
        | c :: cs -> c :: inner cs
        | [] -> []
      in
      { cells = inner v.cells }
  | `Set_code_outputs (cell, outputs, execution_count) ->
      let rec inner = function
        | Codeblock.C ({ cell_type = Code; _ } as c) :: cs when c = cell ->
            Codeblock.C (Codeblock.set_outputs outputs execution_count c) :: cs
        | c :: cs -> c :: inner cs
        | [] -> []
      in
      { cells = inner v.cells }
  | `Set_markdown_outputs (cell, outputs, execution_count) ->
      let rec inner = function
        | Codeblock.C ({ cell_type = Markdown; _ } as c) :: cs when c = cell ->
            Codeblock.C (Codeblock.set_outputs outputs execution_count c) :: cs
        | c :: cs -> c :: inner cs
        | [] -> []
      in
      { cells = inner v.cells }

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
