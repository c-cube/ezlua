# ezlua [![build](https://github.com/c-cube/ezlua/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/ezlua/actions/workflows/main.yml)

Trying to make it easier to use Lua from OCaml.

## ppx

`ppx_ezlua` provides two features: automatic encoder/decoder derivation for
custom types, and a `let%lua` syntax for wrapping OCaml functions as Lua
callbacks.

### `[@@deriving ezlua]`

Annotate a type declaration to generate `<name>_to_lua` and `<name>_of_lua`
functions automatically.

Records become Lua tables keyed by field name. Variants become tables with a
`"tag"` field (and an optional `"value"` field for payloads).

```ocaml
type point = {
  x: float;
  y: float;
}
[@@deriving ezlua]

(* generates:
   val point_to_lua : point Ezlua.to_lua
   val point_of_lua : point Ezlua.of_lua *)

let () =
  let state = Ezlua.create () in
  let p = { x = 1.0; y = 2.5 } in
  Ezlua.set_global state "p" point_to_lua p;
  ignore (Ezlua.run state "assert(p.x == 1.0 and p.y == 2.5)")
```

### `let%lua`

Define an OCaml function and get a matching `<name>_lua` callback suitable for
`Ezlua.add_function`. All parameters and the return type must be annotated.

```ocaml
let%lua add (x : int) (y : int) : int = x + y
(* generates:
   val add     : int -> int -> int
   val add_lua : Lua_api_lib.oCamlFunction *)

let () =
  let state = Ezlua.create () in
  Ezlua.add_function state "add" add_lua;
  ignore (Ezlua.run state "assert(add(1, 2) == 3)")
```

Argument decoding errors are reported back to Lua via `LuaL.error` with a
message that includes the argument position and name.
