open Lua_api

type person = {
  name: string;
  age: int;
  active: bool;
}
[@@deriving ezlua]

type color =
  | Red
  | Green
  | Blue
[@@deriving ezlua]

type shape =
  | Circle of float
  | Rect of float * float
  | Point
[@@deriving ezlua]

type 'a wrapped = {
  value: 'a;
  label: string;
}
[@@deriving ezlua]

type score = {
  player: string;
  points: int list;
}
[@@deriving ezlua]

type opt_test = { maybe: string option } [@@deriving ezlua]

type triple = int * string * bool [@@deriving ezlua]

(* ------------------------------------------------------------------ *)

let check_ok msg = function
  | Ok x -> x
  | Error e -> Alcotest.failf "%s: Error: %a" msg Ezlua.pp_error e

(* ------------------------------------------------------------------ *)

let test_person_roundtrip () =
  let state = Ezlua.create () in
  let p = { name = "Alice"; age = 30; active = true } in
  Ezlua.set_global state "p" person_to_lua p;
  let p2 =
    check_ok "person roundtrip" (Ezlua.get_global state "p" person_of_lua)
  in
  Alcotest.(check string) "name" p.name p2.name;
  Alcotest.(check int) "age" p.age p2.age;
  Alcotest.(check bool) "active" p.active p2.active

let test_person_from_lua () =
  let state = Ezlua.create () in
  let ok = LuaL.dostring state {|p = {name="Bob", age=25, active=false}|} in
  if not ok then Alcotest.fail "lua error";
  let p =
    check_ok "person from lua" (Ezlua.get_global state "p" person_of_lua)
  in
  Alcotest.(check string) "name" "Bob" p.name;
  Alcotest.(check int) "age" 25 p.age;
  Alcotest.(check bool) "active" false p.active

let test_color_roundtrip () =
  let state = Ezlua.create () in
  let colors = [ Red; Green; Blue ] in
  List.iter
    (fun c ->
      Ezlua.set_global state "c" to_lua_color c;
      let c2 =
        check_ok "color roundtrip" (Ezlua.get_global state "c" of_lua_color)
      in
      match c, c2 with
      | Red, Red | Green, Green | Blue, Blue -> ()
      | _ -> Alcotest.fail "color mismatch")
    colors

let test_shape_roundtrip () =
  let state = Ezlua.create () in
  let shapes = [ Circle 3.14; Rect (2.0, 5.0); Point ] in
  List.iter
    (fun s ->
      Ezlua.set_global state "s" to_lua_shape s;
      let s2 =
        check_ok "shape roundtrip" (Ezlua.get_global state "s" of_lua_shape)
      in
      match s, s2 with
      | Circle r, Circle r2 -> Alcotest.(check (float 1e-9)) "circle r" r r2
      | Rect (w, h), Rect (w2, h2) ->
        Alcotest.(check (float 1e-9)) "rect w" w w2;
        Alcotest.(check (float 1e-9)) "rect h" h h2
      | Point, Point -> ()
      | _ -> Alcotest.fail "shape mismatch")
    shapes

let test_shape_from_lua () =
  let state = Ezlua.create () in
  let ok = LuaL.dostring state {|s = {tag="Circle", value=3.14}|} in
  if not ok then Alcotest.fail "lua error";
  let s = check_ok "shape from lua" (Ezlua.get_global state "s" of_lua_shape) in
  match s with
  | Circle r -> Alcotest.(check (float 0.01)) "circle from lua" 3.14 r
  | _ -> Alcotest.fail "expected Circle"

let test_wrapped_roundtrip () =
  let state = Ezlua.create () in
  let w = { value = 42; label = "answer" } in
  Ezlua.set_global state "w" (wrapped_to_lua Ezlua.Encode.int) w;
  let w2 =
    check_ok "wrapped roundtrip"
      (Ezlua.get_global state "w" (wrapped_of_lua Ezlua.Decode.int))
  in
  Alcotest.(check int) "value" w.value w2.value;
  Alcotest.(check string) "label" w.label w2.label

let test_score_roundtrip () =
  let state = Ezlua.create () in
  let s = { player = "player1"; points = [ 10; 20; 30 ] } in
  Ezlua.set_global state "s" score_to_lua s;
  let s2 =
    check_ok "score roundtrip" (Ezlua.get_global state "s" score_of_lua)
  in
  Alcotest.(check string) "player" s.player s2.player;
  Alcotest.(check (list int)) "points" s.points s2.points

let test_opt_roundtrip () =
  let state = Ezlua.create () in
  let t1 = { maybe = Some "hello" } in
  Ezlua.set_global state "t" opt_test_to_lua t1;
  let t2 = check_ok "opt some" (Ezlua.get_global state "t" opt_test_of_lua) in
  Alcotest.(check (option string)) "some" t1.maybe t2.maybe;
  let t3 = { maybe = None } in
  Ezlua.set_global state "t" opt_test_to_lua t3;
  let t4 = check_ok "opt none" (Ezlua.get_global state "t" opt_test_of_lua) in
  Alcotest.(check (option string)) "none" None t4.maybe

let test_nested_option () =
  let enc = Ezlua.Encode.(option (option string)) in
  let dec = Ezlua.Decode.(option (option string)) in
  let state = Ezlua.create () in
  let cases =
    [ None, "None"; Some None, "Some None"; Some (Some "x"), "Some (Some x)" ]
  in
  List.iter
    (fun (v, label) ->
      Ezlua.set_global state "v" enc v;
      let v2 = check_ok label (Ezlua.get_global state "v" dec) in
      Alcotest.(check (option (option string))) label v v2)
    cases

let test_triple_roundtrip () =
  let state = Ezlua.create () in
  let t = (42, "hello", true) in
  Ezlua.set_global state "t" to_lua_triple t;
  let (n, s, b) =
    check_ok "triple roundtrip" (Ezlua.get_global state "t" of_lua_triple)
  in
  Alcotest.(check int) "int" 42 n;
  Alcotest.(check string) "string" "hello" s;
  Alcotest.(check bool) "bool" true b

let%lua add (x : int) (y : int) : int = x + y
let%lua greet (name : string) : string = "Hello, " ^ name

let test_let_lua () =
  let state = Ezlua.create () in
  Ezlua.add_function state "add" add_lua;
  Ezlua.add_function state "greet" greet_lua;
  let result = Ezlua.run state {|result = add(10, 32)|} in
  (match result with
  | Error e -> Alcotest.failf "run error: %a" Ezlua.pp_error e
  | Ok () -> ());
  let n =
    check_ok "get result" (Ezlua.get_global state "result" Ezlua.Decode.int)
  in
  Alcotest.(check int) "add result" 42 n;
  let result2 = Ezlua.run state {|msg = greet("World")|} in
  (match result2 with
  | Error e -> Alcotest.failf "run error: %a" Ezlua.pp_error e
  | Ok () -> ());
  let msg =
    check_ok "get msg" (Ezlua.get_global state "msg" Ezlua.Decode.string)
  in
  Alcotest.(check string) "greet result" "Hello, World" msg

let test_ezlua_smoke () =
  let state = Ezlua.create () in
  Ezlua.set_global state "x" Ezlua.Encode.int 42;
  let result = Ezlua.run state {|assert(x == 42)|} in
  (match result with
  | Error e -> Alcotest.failf "smoke test: %a" Ezlua.pp_error e
  | Ok () -> ());
  let v = check_ok "get x" (Ezlua.get_global state "x" Ezlua.Decode.int) in
  Alcotest.(check int) "x" 42 v

type foo = {
  x: int;
  y: int;
  swap: bool;
}
[@@deriving ezlua, eq, show]

let%lua swap_foo (self : foo) : foo =
  if self.swap then
    { self with x = self.y; y = self.x }
  else
    self

(* test function takes a complex type *)
let test_foo () =
  let st = Ezlua.create () in
  Ezlua.add_function st "swap" swap_foo_lua;
  Ezlua.run st "return swap({x=41, y=5; swap=true})" |> Ezlua.unwrap_err;
  let x = Ezlua.get_stack st (-1) foo_of_lua |> Ezlua.unwrap_err in
  Alcotest.(check (testable pp_foo equal_foo))
    "x"
    { x = 5; y = 41; swap = true }
    x

let test_stress () =
  let t_start = Unix.gettimeofday () in
  let minor_start = Gc.minor_words () in

  let state = Ezlua.create () in
  Ezlua.set_global state "x" Ezlua.Encode.int 0;
  for _i = 1 to 1_000_000 do
    Ezlua.run state "x = x+1" |> Ezlua.unwrap_err
  done;
  let x = Ezlua.get_global state "x" Ezlua.Decode.int |> Ezlua.unwrap_err in
  Alcotest.(check int) "x" 1_000_000 x;

  let elapsed = Unix.gettimeofday () -. t_start in
  let alloc = Gc.minor_words () -. minor_start in
  Printf.eprintf "stress test in %.3fs (%.0f minor words)\n%!" elapsed alloc;
  ()

let () =
  let open Alcotest in
  run
    ~verbose:(Sys.getenv_opt "VERBOSE" = Some "1")
    "ezlua"
    [
      ( "person",
        [
          test_case "roundtrip" `Quick test_person_roundtrip;
          test_case "from_lua" `Quick test_person_from_lua;
        ] );
      "color", [ test_case "roundtrip" `Quick test_color_roundtrip ];
      ( "shape",
        [
          test_case "roundtrip" `Quick test_shape_roundtrip;
          test_case "from_lua" `Quick test_shape_from_lua;
        ] );
      "wrapped", [ test_case "roundtrip" `Quick test_wrapped_roundtrip ];
      "score", [ test_case "roundtrip" `Quick test_score_roundtrip ];
      ( "option",
        [
          test_case "roundtrip" `Quick test_opt_roundtrip;
          test_case "nested" `Quick test_nested_option;
        ] );
      "tuple", [ test_case "roundtrip" `Quick test_triple_roundtrip ];
      "let_lua", [ test_case "callback" `Quick test_let_lua ];
      "ezlua_smoke", [ test_case "smoke" `Quick test_ezlua_smoke ];
      "ezlua_foo", [ test_case "foo" `Quick test_foo ];
      "stress", [ test_case "stress" `Quick test_stress ];
    ]
