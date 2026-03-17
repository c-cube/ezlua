open Ppxlib
open Ast_builder.Default

(* ------------------------------------------------------------------ *)
(* Helpers *)
(* ------------------------------------------------------------------ *)

(* Build an expression that refers to the Ezlua codec for a core_type *)
let rec codec_expr_of_type ~loc (ct : core_type) : expression =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
    [%expr Ezlua.int]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
    [%expr Ezlua.float]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
    [%expr Ezlua.string]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
    [%expr Ezlua.bool]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) ->
    [%expr Ezlua.unit_]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) ->
    let inner = codec_expr_of_type ~loc arg in
    [%expr Ezlua.list [%e inner]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [arg]) ->
    let inner = codec_expr_of_type ~loc arg in
    [%expr Ezlua.array [%e inner]]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
    let inner = codec_expr_of_type ~loc arg in
    [%expr Ezlua.option [%e inner]]
  | Ptyp_constr ({ txt = Lident name; _ }, args) ->
    (* user-defined type: use codec_<name>, passing codec args for params *)
    let base =
      pexp_ident ~loc { loc; txt = Lident ("codec_" ^ name) }
    in
    List.fold_left
      (fun acc arg ->
        let c = codec_expr_of_type ~loc arg in
        pexp_apply ~loc acc [ (Nolabel, c) ])
      base args
  | Ptyp_var name ->
    (* type variable 'a -> passed as argument codec_a *)
    pexp_ident ~loc { loc; txt = Lident ("codec_" ^ name) }
  | Ptyp_tuple elems ->
    (match elems with
    | [ a; b ] ->
      let ca = codec_expr_of_type ~loc a in
      let cb = codec_expr_of_type ~loc b in
      [%expr Ezlua.pair [%e ca] [%e cb]]
    | _ ->
      Location.raise_errorf ~loc "ppx_ezlua: tuples with arity != 2 not supported")
  | _ ->
    Location.raise_errorf ~loc "ppx_ezlua: unsupported type"

(* to_lua function for a core_type, given expression e *)
let to_lua_fn_of_type ~loc (ct : core_type) : expression =
  let codec = codec_expr_of_type ~loc ct in
  [%expr [%e codec].Ezlua.to_lua]

(* of_lua function for a core_type *)
let of_lua_fn_of_type ~loc (ct : core_type) : expression =
  let codec = codec_expr_of_type ~loc ct in
  [%expr [%e codec].Ezlua.of_lua]

(* ------------------------------------------------------------------ *)
(* Shared deriver helpers *)
(* ------------------------------------------------------------------ *)

let extract_param_names params =
  List.filter_map
    (fun (ct, _) ->
      match ct.ptyp_desc with
      | Ptyp_var name -> Some name
      | _ -> None)
    params

let wrap_with_params ~loc param_names body =
  List.fold_right
    (fun pname acc ->
      let pat = ppat_var ~loc { loc; txt = "codec_" ^ pname } in
      pexp_fun ~loc Nolabel None pat acc)
    param_names body

(* Build the codec_<name> structure item that bundles to_lua and of_lua.
   For polymorphic types the generated names are applied to their codec args. *)
let build_codec_stri ~loc ~to_lua_name ~of_lua_name ~codec_name ~param_names =
  let args_fwd =
    List.map (fun pname -> pexp_ident ~loc { loc; txt = Lident ("codec_" ^ pname) }) param_names
  in
  let apply_args base =
    List.fold_left (fun acc a -> pexp_apply ~loc acc [ (Nolabel, a) ]) base args_fwd
  in
  let body =
    [%expr
      {
        Ezlua.to_lua =
          [%e apply_args (pexp_ident ~loc { loc; txt = Lident to_lua_name })];
        of_lua = [%e apply_args (pexp_ident ~loc { loc; txt = Lident of_lua_name })];
      }]
  in
  [%stri let [%p ppat_var ~loc { loc; txt = codec_name }] = [%e wrap_with_params ~loc param_names body]]

(* ------------------------------------------------------------------ *)
(* Record deriver *)
(* ------------------------------------------------------------------ *)

let derive_record ~loc ~type_name ~params (fields : label_declaration list) =
  let param_names = extract_param_names params in
  (* to_lua *)
  let to_lua_body =
    let field_pushes =
      List.map
        (fun ld ->
          let fname = ld.pld_name.txt in
          let ftype = ld.pld_type in
          let to_fn = to_lua_fn_of_type ~loc ftype in
          let fexpr = pexp_field ~loc [%expr v] { loc; txt = Lident fname } in
          [%expr Ezlua.push_field state [%e estring ~loc fname] [%e to_fn] [%e fexpr]])
        fields
    in
    List.fold_right
      (fun push_expr acc -> [%expr [%e push_expr]; [%e acc]])
      field_pushes
      [%expr ()]
  in
  let to_lua_fn_body =
    [%expr
      fun state v ->
        Lua_api.Lua.newtable state;
        [%e to_lua_body]]
  in
  (* of_lua *)
  let result_expr =
    let record_fields =
      List.map
        (fun ld ->
          let fname = ld.pld_name.txt in
          ({ loc; txt = Lident fname }, pexp_ident ~loc { loc; txt = Lident fname }))
        fields
    in
    pexp_record ~loc record_fields None
  in
  let of_lua_body =
    List.fold_right
      (fun ld inner ->
        let fname = ld.pld_name.txt in
        let ftype = ld.pld_type in
        let codec = codec_expr_of_type ~loc ftype in
        [%expr
          match Ezlua.get_field state idx [%e estring ~loc fname] [%e codec] with
          | Error e -> Error e
          | Ok [%p ppat_var ~loc { loc; txt = fname }] -> [%e inner]])
      fields
      [%expr Ok [%e result_expr]]
  in
  let of_lua_fn_body =
    [%expr
      fun state idx ->
        if not (Lua_api.Lua.istable state idx) then
          Error
            (Printf.sprintf "expected table for %s, got %s"
               [%e estring ~loc type_name]
               (Lua_api.LuaL.typename state idx))
        else [%e of_lua_body]]
  in
  let to_lua_name = "to_lua_" ^ type_name in
  let of_lua_name = "of_lua_" ^ type_name in
  let codec_name = "codec_" ^ type_name in
  [
    [%stri let [%p ppat_var ~loc { loc; txt = to_lua_name }] = [%e wrap_with_params ~loc param_names to_lua_fn_body]];
    [%stri let [%p ppat_var ~loc { loc; txt = of_lua_name }] = [%e wrap_with_params ~loc param_names of_lua_fn_body]];
    build_codec_stri ~loc ~to_lua_name ~of_lua_name ~codec_name ~param_names;
  ]

(* ------------------------------------------------------------------ *)
(* Variant deriver *)
(* ------------------------------------------------------------------ *)

let derive_variant ~loc ~type_name ~params (constrs : constructor_declaration list) =
  let param_names = extract_param_names params in
  (* to_lua match arms *)
  let to_lua_cases =
    List.map
      (fun cd ->
        let cname = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          (* No-arg: {tag = "Foo"} *)
          let pat = ppat_construct ~loc { loc; txt = Lident cname } None in
          let body =
            [%expr
              Lua_api.Lua.newtable state;
              Ezlua.push_field state "tag" Ezlua.string.Ezlua.to_lua [%e estring ~loc cname]]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple [ single ] ->
          (* Single arg: {tag="Foo", value=<enc>} *)
          let pat =
            ppat_construct ~loc
              { loc; txt = Lident cname }
              (Some (ppat_var ~loc { loc; txt = "v0" }))
          in
          let to_fn = to_lua_fn_of_type ~loc single in
          let body =
            [%expr
              Lua_api.Lua.newtable state;
              Ezlua.push_field state "tag" Ezlua.string.Ezlua.to_lua [%e estring ~loc cname];
              Ezlua.push_field state "value" [%e to_fn] v0]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple multi ->
          (* Multi-arg: {tag="Foo", value={v0, v1, ...}} *)
          let nvars = List.length multi in
          let varnames = List.init nvars (fun i -> Printf.sprintf "v%d" i) in
          let pat =
            ppat_construct ~loc
              { loc; txt = Lident cname }
              (Some
                 (ppat_tuple ~loc
                    (List.map (fun vn -> ppat_var ~loc { loc; txt = vn }) varnames)))
          in
          let push_elems =
            List.mapi
              (fun i (ct, vn) ->
                let to_fn = to_lua_fn_of_type ~loc ct in
                let v = pexp_ident ~loc { loc; txt = Lident vn } in
                [%expr
                  [%e to_fn] state [%e v];
                  Lua_api.Lua.rawseti state (-2) [%e eint ~loc (i + 1)]])
              (List.combine multi varnames)
          in
          let push_tuple =
            List.fold_right
              (fun e acc -> [%expr [%e e]; [%e acc]])
              push_elems
              [%expr ()]
          in
          let body =
            [%expr
              Lua_api.Lua.newtable state;
              Ezlua.push_field state "tag" Ezlua.string.Ezlua.to_lua [%e estring ~loc cname];
              Lua_api.Lua.newtable state;
              [%e push_tuple];
              Lua_api.Lua.setfield state (-2) "value"]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_record _ ->
          Location.raise_errorf ~loc "ppx_ezlua: inline record variants not supported")
      constrs
  in
  let to_lua_fn_body =
    [%expr fun state v -> [%e pexp_match ~loc [%expr v] to_lua_cases]]
  in
  (* of_lua match arms on tag string *)
  let of_lua_cases =
    List.map
      (fun cd ->
        let cname = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          let pat = ppat_constant ~loc (Pconst_string (cname, loc, None)) in
          let body =
            pexp_construct ~loc { loc; txt = Lident cname } None
            |> fun e -> [%expr Ok [%e e]]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple [ single ] ->
          let pat = ppat_constant ~loc (Pconst_string (cname, loc, None)) in
          let codec = codec_expr_of_type ~loc single in
          let body =
            [%expr
              match Ezlua.get_field state idx "value" [%e codec] with
              | Error e -> Error e
              | Ok v ->
                Ok
                  [%e
                    pexp_construct ~loc
                      { loc; txt = Lident cname }
                      (Some [%expr v])]]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple multi ->
          let pat = ppat_constant ~loc (Pconst_string (cname, loc, None)) in
          let nvars = List.length multi in
          let varnames = List.init nvars (fun i -> Printf.sprintf "v%d" i) in
          let get_elems =
            List.fold_right
              (fun (i, (ct, vn)) inner ->
                let codec = codec_expr_of_type ~loc ct in
                [%expr
                  match Ezlua.get_index state arr_idx__ [%e eint ~loc i] [%e codec] with
                  | Error e -> Error e
                  | Ok [%p ppat_var ~loc { loc; txt = vn }] -> [%e inner]])
              (List.mapi (fun i x -> (i + 1, x)) (List.combine multi varnames))
              [%expr
                Ok
                  [%e
                    pexp_construct ~loc
                      { loc; txt = Lident cname }
                      (Some
                         (pexp_tuple ~loc
                            (List.map
                               (fun vn -> pexp_ident ~loc { loc; txt = Lident vn })
                               varnames)))]]
          in
          let body =
            [%expr
              Lua_api.Lua.getfield state idx "value";
              let arr_idx__ = Lua_api.Lua.gettop state in
              let result__ = [%e get_elems] in
              Lua_api.Lua.pop state 1;
              result__]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_record _ ->
          Location.raise_errorf ~loc "ppx_ezlua: inline record variants not supported")
      constrs
  in
  let wildcard_case =
    case
      ~lhs:(ppat_var ~loc { loc; txt = "s" })
      ~guard:None
      ~rhs:[%expr Error (Printf.sprintf "unknown variant tag: %s" s)]
  in
  let all_of_cases = of_lua_cases @ [ wildcard_case ] in
  let of_lua_fn_body =
    [%expr
      fun state idx ->
        if not (Lua_api.Lua.istable state idx) then
          Error
            (Printf.sprintf "expected table for %s, got %s"
               [%e estring ~loc type_name]
               (Lua_api.LuaL.typename state idx))
        else
          match Ezlua.get_field state idx "tag" Ezlua.string with
          | Error e -> Error ("missing tag: " ^ e)
          | Ok tag__ -> [%e pexp_match ~loc [%expr tag__] all_of_cases]]
  in
  let to_lua_name = "to_lua_" ^ type_name in
  let of_lua_name = "of_lua_" ^ type_name in
  let codec_name = "codec_" ^ type_name in
  [
    [%stri let [%p ppat_var ~loc { loc; txt = to_lua_name }] = [%e wrap_with_params ~loc param_names to_lua_fn_body]];
    [%stri let [%p ppat_var ~loc { loc; txt = of_lua_name }] = [%e wrap_with_params ~loc param_names of_lua_fn_body]];
    build_codec_stri ~loc ~to_lua_name ~of_lua_name ~codec_name ~param_names;
  ]

(* ------------------------------------------------------------------ *)
(* Type alias deriver *)
(* ------------------------------------------------------------------ *)

let derive_alias ~loc ~type_name ~params (manifest : core_type) =
  let param_names = extract_param_names params in
  let delegate = codec_expr_of_type ~loc manifest in
  let codec_name = "codec_" ^ type_name in
  let to_lua_name = "to_lua_" ^ type_name in
  let of_lua_name = "of_lua_" ^ type_name in
  [
    [%stri
      let [%p ppat_var ~loc { loc; txt = to_lua_name }] =
        [%e wrap_with_params ~loc param_names [%expr [%e delegate].Ezlua.to_lua]]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = of_lua_name }] =
        [%e wrap_with_params ~loc param_names [%expr [%e delegate].Ezlua.of_lua]]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = codec_name }] =
        [%e wrap_with_params ~loc param_names delegate]];
  ]

(* ------------------------------------------------------------------ *)
(* Deriver registration *)
(* ------------------------------------------------------------------ *)

let generate_impl ~loc ~path:_ (_rec_flag, type_decls) =
  List.concat_map
    (fun td ->
      let type_name = td.ptype_name.txt in
      let params = td.ptype_params in
      match td.ptype_kind with
      | Ptype_record fields -> derive_record ~loc ~type_name ~params fields
      | Ptype_variant constrs -> derive_variant ~loc ~type_name ~params constrs
      | Ptype_abstract ->
        (match td.ptype_manifest with
        | Some manifest -> derive_alias ~loc ~type_name ~params manifest
        | None ->
          Location.raise_errorf ~loc "ppx_ezlua: abstract type without manifest: %s"
            type_name)
      | Ptype_open ->
        Location.raise_errorf ~loc "ppx_ezlua: open types not supported")
    type_decls

let generate_intf ~loc:_ ~path:_ (_rec_flag, _type_decls) = []

let ezlua_deriver =
  Deriving.add "ezlua"
    ~str_type_decl:(Deriving.Generator.make Deriving.Args.empty generate_impl)
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.empty generate_intf)

(* ------------------------------------------------------------------ *)
(* let%lua extension *)
(* ------------------------------------------------------------------ *)

(* Extract one Pparam_val param from a function_param, or fail *)
let extract_one_param fp =
  let loc = fp.pparam_loc in
  match fp.pparam_desc with
  | Pparam_val (Nolabel, None, pat) ->
    (match pat.ppat_desc with
    | Ppat_constraint (inner_pat, ct) ->
      let pname =
        match inner_pat.ppat_desc with
        | Ppat_var { txt; _ } -> txt
        | _ -> Location.raise_errorf ~loc "let%%lua: expected simple variable pattern"
      in
      (pname, ct)
    | _ ->
      Location.raise_errorf ~loc "let%%lua: all arguments must have type annotations")
  | _ -> Location.raise_errorf ~loc "let%%lua: unexpected parameter kind"

(* Extract (name, type) list and (body, ret_type) from a fun expression.
   In OCaml 5, fun params and return type are bundled in Pexp_function. *)
let extract_params expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_function (params, type_constraint, body) ->
    let named_params = List.map extract_one_param params in
    let ret_ct =
      match type_constraint with
      | Some (Pconstraint ct) -> ct
      | Some (Pcoerce _) ->
        Location.raise_errorf ~loc "let%%lua: coerce constraint not supported"
      | None ->
        Location.raise_errorf ~loc "let%%lua: return type annotation required (e.g., : int)"
    in
    let body_expr =
      match body with
      | Pfunction_body e -> e
      | Pfunction_cases _ ->
        Location.raise_errorf ~loc "let%%lua: function cases body not supported"
    in
    (named_params, (body_expr, ret_ct))
  | Pexp_constraint (inner, ret_ct) ->
    (* no params, just a typed body *)
    ([], (inner, ret_ct))
  | _ ->
    Location.raise_errorf ~loc "let%%lua: expected fun expression with type annotations"

let expand_lua_let ~loc ~path:_ pat expr =
  let fn_name =
    match pat.ppat_desc with
    | Ppat_var { txt; _ } -> txt
    | _ -> Location.raise_errorf ~loc "let%%lua: expected a simple name binding"
  in
  let params, (body, ret_type) = extract_params expr in
  (* Build the original function *)
  let orig_fn =
    List.fold_right
      (fun (pname, ct) acc ->
        let p = ppat_constraint ~loc (ppat_var ~loc { loc; txt = pname }) ct in
        pexp_fun ~loc Nolabel None p acc)
      params
      (pexp_constraint ~loc body ret_type)
  in
  (* Build the Lua wrapper *)
  let result_bind =
    [%expr
      let result__ = [%e pexp_apply ~loc (pexp_ident ~loc { loc; txt = Lident fn_name })
        (List.map (fun (pname, _) ->
          (Nolabel, pexp_ident ~loc { loc; txt = Lident pname })) params)] in
      [%e
        let ret_codec = codec_expr_of_type ~loc ret_type in
        match ret_type.ptyp_desc with
        | Ptyp_constr ({ txt = Lident "unit"; _ }, []) ->
          [%expr ignore result__; 0]
        | _ ->
          [%expr
            [%e ret_codec].Ezlua.to_lua lua_state result__;
            1]]]
  in
  let wrapper_body =
    List.fold_right
      (fun (i, (pname, ct)) inner ->
        let codec = codec_expr_of_type ~loc ct in
        [%expr
          match [%e codec].Ezlua.of_lua lua_state [%e eint ~loc i] with
          | Error msg ->
            LuaL.error lua_state "%s"
              [%e
                estring ~loc (Printf.sprintf "bad arg %d (%s): " i pname)
                |> fun prefix ->
                [%expr [%e prefix] ^ msg]];
            0
          | Ok [%p ppat_var ~loc { loc; txt = pname }] -> [%e inner]])
      (List.mapi (fun i x -> (i + 1, x)) params)
      result_bind
  in
  let wrapper_fn = [%expr fun lua_state -> [%e wrapper_body]] in
  let wrapper_name = fn_name ^ "_lua" in
  [
    [%stri let [%p ppat_var ~loc { loc; txt = fn_name }] = [%e orig_fn]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = wrapper_name }]
          : Lua_api_lib.oCamlFunction =
        [%e wrapper_fn]];
  ]

let lua_ext =
  Extension.declare_inline "lua" Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive (value_binding ~pat:__ ~expr:__ ~constraint_:drop ^:: nil) ^:: nil))
    expand_lua_let

let () =
  Driver.register_transformation "lua"
    ~extensions:[ lua_ext ]

let _ = ezlua_deriver
