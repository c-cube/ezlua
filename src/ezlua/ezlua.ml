open Lua_api

type state = Lua_api_lib.state

type 'a codec = {
  to_lua: state -> 'a -> unit;
  of_lua: state -> int -> ('a, string) result;
}

(* Primitive codecs *)

let int =
  {
    to_lua = (fun state v -> Lua.pushinteger state v);
    of_lua =
      (fun state idx ->
        if Lua.isnumber state idx then
          Ok (Lua.tointeger state idx)
        else
          Error
            (Printf.sprintf "expected number, got %s" (LuaL.typename state idx)));
  }

let float =
  {
    to_lua = (fun state v -> Lua.pushnumber state v);
    of_lua =
      (fun state idx ->
        if Lua.isnumber state idx then
          Ok (Lua.tonumber state idx)
        else
          Error
            (Printf.sprintf "expected number, got %s" (LuaL.typename state idx)));
  }

let string =
  {
    to_lua = (fun state v -> Lua.pushstring state v);
    of_lua =
      (fun state idx ->
        match Lua.tostring state idx with
        | Some s -> Ok s
        | None ->
          Error
            (Printf.sprintf "expected string, got %s" (LuaL.typename state idx)));
  }

let bool =
  {
    to_lua = (fun state v -> Lua.pushboolean state v);
    of_lua =
      (fun state idx ->
        if Lua.isboolean state idx then
          Ok (Lua.toboolean state idx)
        else
          Error
            (Printf.sprintf "expected boolean, got %s" (LuaL.typename state idx)));
  }

let unit_ =
  {
    to_lua = (fun state () -> Lua.pushnil state);
    of_lua = (fun _state _idx -> Ok ());
  }

(* Combinators *)

let option (c : 'a codec) : 'a option codec =
  {
    to_lua =
      (fun state v ->
        match v with
        | None -> Lua.pushnil state
        | Some x ->
          Lua.newtable state;
          c.to_lua state x;
          Lua.rawseti state (-2) 1);
    of_lua =
      (fun state idx ->
        if Lua.isnoneornil state idx then
          Ok None
        else if not (Lua.istable state idx) then
          Error
            (Printf.sprintf "expected nil or table for option, got %s"
               (LuaL.typename state idx))
        else (
          Lua.rawgeti state idx 1;
          let result = c.of_lua state (-1) in
          Lua.pop state 1;
          match result with
          | Ok x -> Ok (Some x)
          | Error e -> Error e
        ));
  }

let list (c : 'a codec) : 'a list codec =
  {
    to_lua =
      (fun state lst ->
        Lua.newtable state;
        List.iteri
          (fun i v ->
            c.to_lua state v;
            Lua.rawseti state (-2) (i + 1))
          lst);
    of_lua =
      (fun state idx ->
        if not (Lua.istable state idx) then
          Error
            (Printf.sprintf "expected table, got %s" (LuaL.typename state idx))
        else (
          let rec loop i acc =
            Lua.rawgeti state idx i;
            if Lua.isnil state (-1) then (
              Lua.pop state 1;
              Ok (List.rev acc)
            ) else (
              match c.of_lua state (-1) with
              | Error e ->
                Lua.pop state 1;
                Error (Printf.sprintf "list[%d]: %s" i e)
              | Ok v ->
                Lua.pop state 1;
                loop (i + 1) (v :: acc)
            )
          in
          loop 1 []
        ));
  }

let array (c : 'a codec) : 'a array codec =
  let lc = list c in
  {
    to_lua = (fun state arr -> lc.to_lua state (Array.to_list arr));
    of_lua =
      (fun state idx ->
        match lc.of_lua state idx with
        | Ok lst -> Ok (Array.of_list lst)
        | Error e -> Error e);
  }

let pair (ca : 'a codec) (cb : 'b codec) : ('a * 'b) codec =
  {
    to_lua =
      (fun state (a, b) ->
        Lua.newtable state;
        ca.to_lua state a;
        Lua.rawseti state (-2) 1;
        cb.to_lua state b;
        Lua.rawseti state (-2) 2);
    of_lua =
      (fun state idx ->
        if not (Lua.istable state idx) then
          Error
            (Printf.sprintf "expected table, got %s" (LuaL.typename state idx))
        else (
          Lua.rawgeti state idx 1;
          let ra = ca.of_lua state (-1) in
          Lua.pop state 1;
          match ra with
          | Error e -> Error ("pair.1: " ^ e)
          | Ok a ->
            Lua.rawgeti state idx 2;
            let rb = cb.of_lua state (-1) in
            Lua.pop state 1;
            (match rb with
            | Error e -> Error ("pair.2: " ^ e)
            | Ok b -> Ok (a, b))
        ));
  }

(* Table helpers used by generated code *)

let push_field state name to_lua_fn v =
  to_lua_fn state v;
  Lua.setfield state (-2) name

let get_field state idx name codec =
  Lua.getfield state idx name;
  let result = codec.of_lua state (-1) in
  Lua.pop state 1;
  match result with
  | Error e -> Error (Printf.sprintf "field '%s': %s" name e)
  | Ok v -> Ok v

let get_index state idx i codec =
  Lua.rawgeti state idx i;
  let result = codec.of_lua state (-1) in
  Lua.pop state 1;
  match result with
  | Error e -> Error (Printf.sprintf "index %d: %s" i e)
  | Ok v -> Ok v

(* High-level API *)

let create () =
  let state = LuaL.newstate () in
  LuaL.openlibs state;
  state

let add_function state name f = Lua.register state name f

let set_global codec state name v =
  codec.to_lua state v;
  Lua.setglobal state name

let get_global codec state name =
  Lua.getglobal state name;
  let result = codec.of_lua state (-1) in
  Lua.pop state 1;
  result

let pop_error state =
  let msg =
    match Lua.tostring state (-1) with
    | Some s -> s
    | None -> "unknown Lua error"
  in
  Lua.pop state 1;
  Error msg

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
