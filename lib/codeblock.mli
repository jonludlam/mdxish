(* Codeblock *)
module TopAPI = Js_top_worker_rpc.Toplevel_api_gen

type code = TopAPI.exec_result
type markdown = string
type _ cellty = Markdown : markdown cellty | Code : code cellty

module Id : sig
  type t

  val create : unit -> t
  val compare : t -> t -> int
  val to_string : t -> string
  val of_string : string -> t
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

val code_of_exec_result : TopAPI.exec_result -> code
val markdown_of_string : string -> markdown
val v : 'a cellty -> string -> (string * string) list -> 'a cell
val id : packed_cell -> Id.t
val of_json : Jv.t -> packed_cell option
val exec : Topworker.t -> 'a cell -> ('a, [> `Msg of string ]) Result.t Lwt.t
val set_source : string -> 'a cell -> 'a cell
val set_outputs : 'a -> int -> 'a cell -> 'a cell
