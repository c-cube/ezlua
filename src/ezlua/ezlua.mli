(** Helpers to make the [lua] bindings more ergonomic *)

type state = Lua_api_lib.state
(** A lua interpreter instance. Not thread safe. All lua state, globals,
    definitions, etc belongs in a given state. Multiple states can be created.
*)

type 'a to_lua = state -> 'a -> unit
(** Turn a OCaml value into a lua value *)

type error = [ `Msg of string ]

type 'a of_lua = state -> int -> ('a, error) result
(** Convert a lua value into an OCaml value, or fail *)

type 'a string_table = (string * 'a) list
(** A string-keyed dictionary. In OCaml this is a [(string * 'a) list]. *)

val pp_error : Format.formatter -> error -> unit
(** Pretty-print an [error]. *)

(** Encode to lua *)
module Encode : sig
  val int : int to_lua
  (** Push an integer onto the Lua stack. *)

  val float : float to_lua
  (** Push a float onto the Lua stack. *)

  val string : string to_lua
  (** Push a string onto the Lua stack. *)

  val bool : bool to_lua
  (** Push a boolean onto the Lua stack. *)

  val unit_ : unit to_lua
  (** Push [nil] onto the Lua stack. *)

  val option : 'a to_lua -> 'a option to_lua
  (** Push [None] as [nil], or [Some x] as a single-element table [{x}]. *)

  val list : 'a to_lua -> 'a list to_lua
  (** Push a list as a 1-indexed Lua table. *)

  val array : 'a to_lua -> 'a array to_lua
  (** Push an array as a 1-indexed Lua table. *)

  val pair : 'a to_lua -> 'b to_lua -> ('a * 'b) to_lua
  (** Push a pair as a two-element Lua table [{a, b}]. *)

  val string_table : 'a to_lua -> 'a string_table to_lua
  (** Push a [(string * 'a) list] as a string-keyed Lua table
      [\{ key1=v1, key2=v2, ... \}]. *)
end

(** Decode from lua.

    Decoding always take the index of the value to decode in the lua stack. The
    top of the stack is at [(-1)]. *)
module Decode : sig
  val int : int of_lua
  (** Read an integer from stack position [idx]. *)

  val float : float of_lua
  (** Read a float from stack position [idx]. *)

  val string : string of_lua
  (** Read a string from stack position [idx]. *)

  val bool : bool of_lua
  (** Read a boolean from stack position [idx]. *)

  val unit_ : unit of_lua
  (** Always succeeds, ignoring the value at [idx]. *)

  val option : 'a of_lua -> 'a option of_lua
  (** Decode [nil] or none as [None]; a single-element table [{x}] as [Some x].
  *)

  val list : 'a of_lua -> 'a list of_lua
  (** Decode a 1-indexed Lua table as a list, stopping at the first [nil]. *)

  val array : 'a of_lua -> 'a array of_lua
  (** Decode a 1-indexed Lua table as an array, stopping at the first [nil]. *)

  val pair : 'a of_lua -> 'b of_lua -> ('a * 'b) of_lua
  (** Decode a two-element Lua table as a pair. *)

  val string_table : 'a of_lua -> 'a string_table of_lua
  (** Decode a string-keyed Lua table as a [(string * 'a) list]. *)
end

val push_table : state -> unit
(** Push an empty table on top of the stack *)

val push_field : state -> string -> 'a to_lua -> 'a -> unit
(** Push a value with [to_lua] and set it as a named field on the table at the
    top of the stack. *)

val get_field : state -> int -> string -> 'a of_lua -> ('a, error) result
(** Read a named field from the table at stack position [idx]. *)

val get_index : state -> int -> int -> 'a of_lua -> ('a, error) result
(** Read the integer-keyed entry [i] from the table at stack position [idx]. *)

val get_stack : state -> int -> 'a of_lua -> ('a, error) result
(** Read the value at stack position [idx] *)

val create : ?stdlib:bool -> unit -> state
(** Create a new Lua state. [stdlib] defaults to [true]; pass [~stdlib:false] to
    skip loading the standard libraries. *)

val unwrap_err : ('a, error) result -> 'a
(** Return the [Ok] value, or [failwith] the error message. *)

val add_function : state -> string -> Lua_api_lib.oCamlFunction -> unit
(** Register an OCaml function as a Lua global.

    A function is of type [state -> int]. Use [let%lua] with the ppx to easily
    export OCaml functions to lua. *)

val set_global : state -> string -> 'a to_lua -> 'a -> unit
(** Push a value with [to_lua] and assign it to a Lua global variable. *)

val get_global : state -> string -> 'a of_lua -> ('a, error) result
(** Read a Lua global variable and decode it with [of_lua]. *)

val run : state -> string -> (unit, error) result
(** Compile and execute a Lua string. *)

val run_file : state -> string -> (unit, error) result
(** Compile and execute a Lua source file at the given path. *)
