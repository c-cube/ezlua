type state = { l: string list } [@@deriving show, ezlua]

let rec fib n =
  if n <= 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

let%lua add_fib (state : state) (n : int) : state =
  let f = fib n in
  { l = string_of_int f :: state.l }

let () =
  let st = Ezlua.create () in
  Ezlua.add_function st "add_fib" add_fib_lua;
  Ezlua.set_global st "state0" state_to_lua { l = [] };
  Ezlua.run st {|for i = 1, 28 do state0 = add_fib(state0, i) end|}
  |> Ezlua.unwrap_err;

  let last_st = Ezlua.get_global st "state0" state_of_lua |> Ezlua.unwrap_err in
  Printf.printf "final state: %s\n%!" (show_state last_st);

  ()
