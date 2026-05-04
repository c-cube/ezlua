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
`"tag"` field (and an optional `"value"` or `"values"` field for payloads).

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
  Ezlua.init_error_callbacks state;   (* call once *)
  Ezlua.add_function state "add" add_lua;
  ignore (Ezlua.run state "assert(add(1, 2) == 3)")
```

`add_function` automatically wraps the callback with `_ez_check()` so argument
decode errors propagate to the Lua caller transparently. See
[OCaml 5 safety](#ocaml-5-safety) for the full picture.

## OCaml 5 safety

OCaml 5 uses a fiber-based runtime. Calling `lua_error` (which does a C
`longjmp`) from inside an OCaml callback skips OCaml activation records on the
stack, corrupting the GC and crashing the program.

**The safe pattern**: never call `lua_error` / `LuaL.error` from OCaml code.
Instead:

1. Call `Ezlua.init_error_callbacks state` once after creating the state.
   This installs two globals: `_ez_pending_error` (initially `nil`) and
   `_ez_check()`.

2. When an OCaml callback needs to signal an error, call
   `Ezlua.signal_error state msg` and return `0` immediately.
   This stores the message in `_ez_pending_error` without any longjmp.

3. Wrap each OCaml-backed Lua function in a thin Lua shim that calls
   `_ez_check()` after the OCaml callback returns:

   ```lua
   mylib = {
     foo = function(...)
       local r = _raw_foo(...)  -- calls OCaml via caml_callback
       _ez_check()              -- re-raises from pure Lua (safe)
       return r
     end,
   }
   ```

   `_ez_check()` is pure Lua; it calls Lua's own `error()` which performs the
   longjmp with no OCaml frames on the stack.

The `let%lua` ppx follows this convention: generated `<name>_lua` wrappers call
`signal_error` (not `LuaL.error`) so they are safe to use inside the pattern
above. You still need to provide the Lua-side shim that calls `_ez_check()`.

## Encoding conventions

This table describes how OCaml values map to Lua values.

| OCaml type | Lua representation |
|---|---|
| `int` | number (integer) |
| `float` | number |
| `string` | string |
| `bool` | boolean |
| `unit` | `nil` |
| `'a list` / `'a array` | table `{v1, v2, ...}` (1-indexed) |
| `'a option` | `nil` for `None`; `{v}` (single-element table) for `Some v` |
| `'a * 'b` (pair) | table `{a, b}` via `Encode.pair`/`Decode.pair` |
| N-tuple (`'a * 'b * 'c * ...`) | table `{v1, v2, ..., vN}` (1-indexed, inline) |
| record | table keyed by field name, e.g. `{x=1.0, y=2.5}` |
| variant (no payload) | `{tag="Ctor"}` |
| variant (single payload) | `{tag="Ctor", value=v}` |
| variant (tuple payload) | `{tag="Ctor", value={v1, v2, ...}}` |

Pairs use the `Encode.pair`/`Decode.pair` library functions. Larger tuples
generate inline table code directly.
