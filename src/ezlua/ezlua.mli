type state = Lua_api_lib.state
type 'a to_lua = state -> 'a -> unit
type error = [ `Msg of string ]
type 'a of_lua = state -> int -> ('a, error) result

val pp_error : Format.formatter -> error -> unit

module Encode : sig
  val int : int to_lua
  val float : float to_lua
  val string : string to_lua
  val bool : bool to_lua
  val unit_ : unit to_lua
  val option : 'a to_lua -> 'a option to_lua
  val list : 'a to_lua -> 'a list to_lua
  val array : 'a to_lua -> 'a array to_lua
  val pair : 'a to_lua -> 'b to_lua -> ('a * 'b) to_lua
end

module Decode : sig
  val int : int of_lua
  val float : float of_lua
  val string : string of_lua
  val bool : bool of_lua
  val unit_ : unit of_lua
  val option : 'a of_lua -> 'a option of_lua
  val list : 'a of_lua -> 'a list of_lua
  val array : 'a of_lua -> 'a array of_lua
  val pair : 'a of_lua -> 'b of_lua -> ('a * 'b) of_lua
end

val push_field : state -> string -> 'a to_lua -> 'a -> unit
val get_field : state -> int -> string -> 'a of_lua -> ('a, error) result
val get_index : state -> int -> int -> 'a of_lua -> ('a, error) result
val create : ?stdlib:bool -> unit -> state
val unwrap_err : ('a, error) result -> 'a
val add_function : state -> string -> Lua_api_lib.oCamlFunction -> unit
val set_global : state -> string -> 'a to_lua -> 'a -> unit
val get_global : state -> string -> 'a of_lua -> ('a, error) result
val run : state -> string -> (unit, error) result
val run_file : state -> string -> (unit, error) result
