open Lwt.Infix
open Lua_api

(* ── derived types ──────────────────────────────────────────────────────── *)

type room_info = {
  room_name: string;
  members:   string list;
}
[@@deriving ezlua]

(* ── shared server state (no mutex: cooperative Lwt) ────────────────────── *)

let clients : (string, Lwt_io.output_channel) Hashtbl.t = Hashtbl.create 16
let rooms   : (string, string list)            Hashtbl.t = Hashtbl.create 8
let guest_counter = ref 0

(* ── per-connection record ──────────────────────────────────────────────── *)

type client = {
  mutable nick: string;
  mutable room: string option;
  oc:           Lwt_io.output_channel;
}

(* ── drive loop ─────────────────────────────────────────────────────────── *)
(* When the Lua script calls coroutine.yield() the coroutine suspends and
   Lua.resume returns LUA_YIELD.  We then wait for the next line from the
   network and resume with it (or with nil on EOF). *)

let rec drive coro ic n =
  match Lua.resume coro n with
  | Lua.LUA_OK    -> Lwt.return ()
  | Lua.LUA_YIELD ->
    Lwt_io.read_line_opt ic >>= fun opt ->
    let n' = match opt with
      | None   -> Lua.pushnil    coro;   1
      | Some s -> Lua.pushstring coro s; 1
    in
    drive coro ic n'
  | _status ->
    let msg = Option.value ~default:"unknown" (Lua.tostring coro (-1)) in
    Lua.pop coro 1;
    Lwt_io.eprintlf "lua error: %s" msg

(* ── OCaml callbacks exposed to Lua ─────────────────────────────────────── *)
(* IMPORTANT: when a callback is invoked from a coroutine, the `state`
   argument is the *main* state, not the coroutine.  Arguments and results
   both live on the *coroutine* stack, so every callback closes over `coro`. *)

(* send_line(msg): fire-and-forget write; no yield needed. *)
let make_send_line coro client : Lua.oCamlFunction =
  fun _s ->
    (match Lua.tostring coro 1 with
     | Some msg -> Lwt.async (fun () -> Lwt_io.write_line client.oc msg)
     | None     -> ());
    0

(* get_nick() → string *)
let make_get_nick coro client : Lua.oCamlFunction =
  fun _s -> Lua.pushstring coro client.nick; 1

(* set_nick(name) → bool *)
let make_set_nick coro client : Lua.oCamlFunction =
  fun _s ->
    (match Lua.tostring coro 1 with
     | None          -> Lua.pushboolean coro false
     | Some new_nick ->
       let ok = not (Hashtbl.mem clients new_nick) in
       if ok then begin
         Hashtbl.remove  clients client.nick;
         Hashtbl.replace clients new_nick client.oc;
         (match client.room with
          | None   -> ()
          | Some r ->
            let ms = try Hashtbl.find rooms r with Not_found -> [] in
            Hashtbl.replace rooms r
              (new_nick :: List.filter (fun n -> n <> client.nick) ms));
         client.nick <- new_nick
       end;
       Lua.pushboolean coro ok);
    1

(* join_room(name): leaves old room, joins new one. *)
let make_join_room coro client : Lua.oCamlFunction =
  fun _s ->
    (match Lua.tostring coro 1 with
     | None           -> ()
     | Some room_name ->
       (match client.room with
        | None   -> ()
        | Some r ->
          let ms =
            List.filter (fun n -> n <> client.nick)
              (try Hashtbl.find rooms r with Not_found -> [])
          in
          if ms = [] then Hashtbl.remove  rooms r
          else             Hashtbl.replace rooms r ms);
       let ms = try Hashtbl.find rooms room_name with Not_found -> [] in
       Hashtbl.replace rooms room_name (client.nick :: ms);
       client.room <- Some room_name);
    0

(* get_names() → room_info table | nil  (uses [@@deriving ezlua] encoder) *)
let make_get_names coro client : Lua.oCamlFunction =
  fun _s ->
    (match client.room with
     | None   -> Lua.pushnil coro
     | Some r ->
       let members = try Hashtbl.find rooms r with Not_found -> [] in
       to_lua_room_info coro { room_name = r; members });
    1

(* broadcast(msg): send to all members of current room. *)
let make_broadcast coro client : Lua.oCamlFunction =
  fun _s ->
    (match Lua.tostring coro 1 with
     | None     -> ()
     | Some msg ->
       let targets =
         match client.room with
         | None   -> []
         | Some r ->
           List.filter_map
             (fun nick -> Hashtbl.find_opt clients nick)
             (try Hashtbl.find rooms r with Not_found -> [])
       in
       Lwt.async (fun () ->
         Lwt_list.iter_p
           (fun oc ->
              Lwt.catch
                (fun () -> Lwt_io.write_line oc msg)
                (fun _  -> Lwt.return ()))
           targets));
    0

(* ── Lua state setup ────────────────────────────────────────────────────── *)

let register_fns main_st coro client =
  let add name fn = Ezlua.add_function main_st name fn in
  add "send_line" (make_send_line coro client);
  add "get_nick"  (make_get_nick  coro client);
  add "set_nick"  (make_set_nick  coro client);
  add "join_room" (make_join_room coro client);
  add "get_names" (make_get_names coro client);
  add "broadcast" (make_broadcast coro client)

(* read_line() is a thin Lua wrapper around coroutine.yield(): the OCaml
   drive loop resumes the coroutine with each new line from the socket. *)
let chat_script = {|
function read_line() return coroutine.yield() end

send_line("Welcome! Nick: " .. get_nick())
send_line("Commands: /nick <n>  /join <room>  /names")

while true do
  local line = read_line()
  if line == nil then break end

  if line:sub(1, 1) == "/" then
    local cmd, rest = line:match("^/(%S+)%s*(.*)")
    if not cmd then
      send_line("Bad command")
    elseif cmd == "nick" then
      local name = rest:match("^(%S+)")
      if not name then
        send_line("Usage: /nick <name>")
      elseif set_nick(name) then
        send_line("Nick: " .. name)
      else
        send_line("Nick taken: " .. name)
      end
    elseif cmd == "join" then
      local room = rest:match("^(%S+)")
      if not room then
        send_line("Usage: /join <room>")
      else
        join_room(room)
        send_line("Joined: " .. room)
      end
    elseif cmd == "names" then
      local info = get_names()
      if not info then
        send_line("Not in a room")
      else
        send_line("Room #" .. info.room_name .. ": "
                  .. table.concat(info.members, ", "))
      end
    else
      send_line("Unknown: /" .. cmd)
    end
  else
    if not get_names() then
      send_line("Join a room first")
    else
      broadcast("[" .. get_nick() .. "] " .. line)
    end
  end
end
|}

(* ── cleanup ─────────────────────────────────────────────────────────────── *)

let cleanup client =
  Hashtbl.remove clients client.nick;
  (match client.room with
   | None   -> ()
   | Some r ->
     let ms =
       List.filter (fun n -> n <> client.nick)
         (try Hashtbl.find rooms r with Not_found -> [])
     in
     if ms = [] then Hashtbl.remove  rooms r
     else             Hashtbl.replace rooms r ms);
  Lwt.return ()

(* ── connection handler ─────────────────────────────────────────────────── *)

let handle_connection fd =
  let ic = Lwt_io.(of_fd ~mode:Input  fd) in
  let oc = Lwt_io.(of_fd ~mode:Output fd) in
  incr guest_counter;
  let nick = Printf.sprintf "guest_%d" !guest_counter in
  Hashtbl.replace clients nick oc;
  let client  = { nick; room = None; oc } in
  let main_st = Ezlua.create () in
  let coro    = Lua.newthread main_st in
  register_fns main_st coro client;
  (match LuaL.loadstring coro chat_script with
   | Lua.LUA_OK -> ()
   | _          ->
     let msg = Option.value ~default:"?" (Lua.tostring coro (-1)) in
     failwith ("failed to load chat script: " ^ msg));
  drive coro ic 0 >>= fun () ->
  cleanup client >>= fun () ->
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return ())

(* ── TCP server loop ─────────────────────────────────────────────────────── *)

let server_loop port =
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  let sock  = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  Lwt_unix.bind sock addr >>= fun () ->
  Lwt_unix.listen sock 16;
  Printf.printf "Chat server on :%d\n%!" port;
  let rec loop () =
    Lwt_unix.accept sock >>= fun (fd, _addr) ->
    Lwt.async (fun () -> handle_connection fd);
    loop ()
  in
  loop ()

let () =
  let port =
    if Array.length Sys.argv > 1
    then int_of_string Sys.argv.(1)
    else 4000
  in
  Lwt_main.run (server_loop port)
