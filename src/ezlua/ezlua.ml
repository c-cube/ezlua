open Lua_api

type state = Lua_api_lib.state
type 'a to_lua = state -> 'a -> unit
type error = [ `Msg of string ]
type 'a of_lua = state -> int -> ('a, error) result
type 'a string_table = (string * 'a) list

let string_of_error (`Msg s) = s
let pp_error out (`Msg s) = Format.pp_print_string out s

let[@inline] unwrap_err = function
  | Ok x -> x
  | Error (`Msg e) -> failwith e

module Encode = struct
  let int state v = Lua.pushinteger state v
  let float state v = Lua.pushnumber state v
  let string state v = Lua.pushstring state v
  let bool state v = Lua.pushboolean state v
  let unit state () = Lua.pushnil state

  let option enc state v =
    match v with
    | None -> Lua.pushnil state
    | Some x ->
      Lua.newtable state;
      enc state x;
      Lua.rawseti state (-2) 1

  let list enc state lst =
    Lua.newtable state;
    List.iteri
      (fun i v ->
        enc state v;
        Lua.rawseti state (-2) (i + 1))
      lst

  let array enc state arr = list enc state (Array.to_list arr)

  let pair enc_a enc_b state (a, b) =
    Lua.newtable state;
    enc_a state a;
    Lua.rawseti state (-2) 1;
    enc_b state b;
    Lua.rawseti state (-2) 2

  let string_table enc state kvs =
    Lua.newtable state;
    List.iter
      (fun (k, v) ->
        enc state v;
        Lua.setfield state (-2) k)
      kvs
end

module Decode = struct
  let int state idx =
    if Lua.isnumber state idx then
      Ok (Lua.tointeger state idx)
    else
      Error
        (`Msg
           (Printf.sprintf "expected number, got %s" (LuaL.typename state idx)))

  let float state idx =
    if Lua.isnumber state idx then
      Ok (Lua.tonumber state idx)
    else
      Error
        (`Msg
           (Printf.sprintf "expected number, got %s" (LuaL.typename state idx)))

  let string state idx =
    match Lua.tostring state idx with
    | Some s -> Ok s
    | None ->
      Error
        (`Msg
           (Printf.sprintf "expected string, got %s" (LuaL.typename state idx)))

  let bool state idx =
    if Lua.isboolean state idx then
      Ok (Lua.toboolean state idx)
    else
      Error
        (`Msg
           (Printf.sprintf "expected boolean, got %s" (LuaL.typename state idx)))

  let unit _state _idx = Ok ()

  let option dec state idx =
    if Lua.isnoneornil state idx then
      Ok None
    else if not (Lua.istable state idx) then
      Error
        (`Msg
           (Printf.sprintf "expected nil or table for option, got %s"
              (LuaL.typename state idx)))
    else (
      Lua.rawgeti state idx 1;
      let result = dec state (-1) in
      Lua.pop state 1;
      match result with
      | Ok x -> Ok (Some x)
      | Error e -> Error e
    )

  let list dec state idx =
    if not (Lua.istable state idx) then
      Error
        (`Msg
           (Printf.sprintf "expected table, got %s" (LuaL.typename state idx)))
    else (
      let rec loop i acc =
        Lua.rawgeti state idx i;
        if Lua.isnil state (-1) then (
          Lua.pop state 1;
          Ok (List.rev acc)
        ) else (
          match dec state (-1) with
          | Error (`Msg e) ->
            Lua.pop state 1;
            Error (`Msg (Printf.sprintf "list[%d]: %s" i e))
          | Ok v ->
            Lua.pop state 1;
            loop (i + 1) (v :: acc)
        )
      in
      loop 1 []
    )

  let array dec state idx =
    match list dec state idx with
    | Ok lst -> Ok (Array.of_list lst)
    | Error e -> Error e

  let pair dec_a dec_b state idx =
    if not (Lua.istable state idx) then
      Error
        (`Msg
           (Printf.sprintf "expected table, got %s" (LuaL.typename state idx)))
    else (
      Lua.rawgeti state idx 1;
      let ra = dec_a state (-1) in
      Lua.pop state 1;
      match ra with
      | Error (`Msg e) -> Error (`Msg ("pair.1: " ^ e))
      | Ok a ->
        Lua.rawgeti state idx 2;
        let rb = dec_b state (-1) in
        Lua.pop state 1;
        (match rb with
        | Error (`Msg e) -> Error (`Msg ("pair.2: " ^ e))
        | Ok b -> Ok (a, b))
    )

  let string_table dec state idx =
    if not (Lua.istable state idx) then
      Error
        (`Msg
           (Printf.sprintf "expected table, got %s" (LuaL.typename state idx)))
    else (
      let abs_idx =
        if idx < 0 then
          Lua.gettop state + idx + 1
        else
          idx
      in
      let result = ref [] in
      let err = ref None in
      Lua.pushnil state;
      let continue_ = ref true in
      while !continue_ do
        if Lua.next state abs_idx = 0 then
          continue_ := false
        else (
          (match Lua.tostring state (-2) with
          | None -> ()
          | Some k ->
            (match dec state (-1) with
            | Ok v -> result := (k, v) :: !result
            | Error (`Msg e) ->
              if !err = None then
                err := Some (Printf.sprintf "string_table key '%s': %s" k e)));
          Lua.pop state 1
        )
      done;
      match !err with
      | Some e -> Error (`Msg e)
      | None -> Ok (List.rev !result)
    )
end

(* Table helpers used by generated code *)

let push_table state = Lua.newtable state

let push_field state name to_lua_fn v =
  to_lua_fn state v;
  Lua.setfield state (-2) name

let get_field state idx name of_lua_fn =
  Lua.getfield state idx name;
  let result = of_lua_fn state (-1) in
  Lua.pop state 1;
  match result with
  | Error (`Msg e) -> Error (`Msg (Printf.sprintf "field '%s': %s" name e))
  | Ok v -> Ok v

let get_index state idx i of_lua_fn =
  Lua.rawgeti state idx i;
  let result = of_lua_fn state (-1) in
  Lua.pop state 1;
  match result with
  | Error (`Msg e) -> Error (`Msg (Printf.sprintf "index %d: %s" i e))
  | Ok v -> Ok v

let get_stack state idx of_lua_fn =
  let result = of_lua_fn state idx in
  match result with
  | Error (`Msg e) -> Error (`Msg (Printf.sprintf "stack %d: %s" idx e))
  | Ok v -> Ok v

(* High-level API *)

let create ?(stdlib = true) () =
  let state = LuaL.newstate () in
  if stdlib then LuaL.openlibs state;
  state

let add_function state name f = Lua.register state name f

let set_global state name to_lua_fn v =
  to_lua_fn state v;
  Lua.setglobal state name

let get_global state name of_lua_fn =
  Lua.getglobal state name;
  let result = of_lua_fn state (-1) in
  Lua.pop state 1;
  result

let pop_error state =
  let msg =
    match Lua.tostring state (-1) with
    | Some s -> s
    | None -> "unknown Lua error"
  in
  Lua.pop state 1;
  Error (`Msg msg)

let run state code =
  if LuaL.dostring state code then
    Ok ()
  else
    pop_error state

let run_file state path =
  if LuaL.dofile state path then
    Ok ()
  else
    pop_error state
