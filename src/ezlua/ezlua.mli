type state = Lua_api_lib.state
type 'a to_lua = state -> 'a -> unit
type error = [ `Msg of string ]
type 'a of_lua = state -> int -> ('a, error) result

val pp_error : Format.formatter -> error -> unit
(** Pretty-print an [error]. *)

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
end

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
  (** Decode [nil] or none as [None]; a single-element table [{x}] as [Some x]. *)

  val list : 'a of_lua -> 'a list of_lua
  (** Decode a 1-indexed Lua table as a list, stopping at the first [nil]. *)

  val array : 'a of_lua -> 'a array of_lua
  (** Decode a 1-indexed Lua table as an array, stopping at the first [nil]. *)

  val pair : 'a of_lua -> 'b of_lua -> ('a * 'b) of_lua
  (** Decode a two-element Lua table as a pair. *)
end

val push_field : state -> string -> 'a to_lua -> 'a -> unit
(** Push a value with [to_lua] and set it as a named field on the table at the
    top of the stack. *)

val get_field : state -> int -> string -> 'a of_lua -> ('a, error) result
(** Read a named field from the table at stack position [idx]. *)

val get_index : state -> int -> int -> 'a of_lua -> ('a, error) result
(** Read the integer-keyed entry [i] from the table at stack position [idx]. *)

val create : ?stdlib:bool -> unit -> state
(** Create a new Lua state. [stdlib] defaults to [true]; pass [~stdlib:false] to
    skip loading the standard libraries. *)

val unwrap_err : ('a, error) result -> 'a
(** Return the [Ok] value, or [failwith] the error message. *)

val add_function : state -> string -> Lua_api_lib.oCamlFunction -> unit
(** Register an OCaml function as a Lua global. *)

val set_global : state -> string -> 'a to_lua -> 'a -> unit
(** Push a value with [to_lua] and assign it to a Lua global variable. *)

val get_global : state -> string -> 'a of_lua -> ('a, error) result
(** Read a Lua global variable and decode it with [of_lua]. *)

val run : state -> string -> (unit, error) result
(** Compile and execute a Lua string. *)

val run_file : state -> string -> (unit, error) result
(** Compile and execute a Lua source file at the given path. *)
