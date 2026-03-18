type state = Lua_api_lib.state

type 'a codec = {
  to_lua : state -> 'a -> unit;
  of_lua : state -> int -> ('a, string) result;
}

val int    : int codec
val float  : float codec
val string : string codec
val bool   : bool codec
val unit_  : unit codec

val option : 'a codec -> 'a option codec
(** [None] encodes as nil; [Some x] encodes as [{x}] (1-element array).
    This makes [Some None] distinguishable from [None]. *)
val list   : 'a codec -> 'a list codec
val array  : 'a codec -> 'a array codec
val pair   : 'a codec -> 'b codec -> ('a * 'b) codec

val push_field : state -> string -> (state -> 'a -> unit) -> 'a -> unit
val get_field  : state -> int -> string -> 'a codec -> ('a, string) result
val get_index  : state -> int -> int -> 'a codec -> ('a, string) result

val create       : unit -> state
val add_function : state -> string -> Lua_api_lib.oCamlFunction -> unit
val set_global   : 'a codec -> state -> string -> 'a -> unit
val get_global   : 'a codec -> state -> string -> ('a, string) result
val run          : state -> string -> (unit, string) result
val run_file     : state -> string -> (unit, string) result
