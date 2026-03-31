open Ppxlib
open Ast_builder.Default

(* ------------------------------------------------------------------ *)
(* Helpers *)
(* ------------------------------------------------------------------ *)

let rec encode_expr_of_type (ct : core_type) : expression =
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr Ezlua.Encode.int]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> [%expr Ezlua.Encode.float]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
    [%expr Ezlua.Encode.string]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr Ezlua.Encode.bool]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> [%expr Ezlua.Encode.unit]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ arg ]) ->
    let inner = encode_expr_of_type arg in
    [%expr Ezlua.Encode.list [%e inner]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ arg ]) ->
    let inner = encode_expr_of_type arg in
    [%expr Ezlua.Encode.array [%e inner]]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]) ->
    let inner = encode_expr_of_type arg in
    [%expr Ezlua.Encode.option [%e inner]]
  | Ptyp_constr ({ txt = Lident "string_table"; _ }, [ arg ]) ->
    let inner = encode_expr_of_type arg in
    [%expr Ezlua.Encode.string_table [%e inner]]
  | Ptyp_constr ({ txt = Lident name; _ }, args) ->
    let base = pexp_ident ~loc { loc; txt = Lident (name ^ "_to_lua") } in
    List.fold_left
      (fun acc arg -> pexp_apply ~loc acc [ Nolabel, encode_expr_of_type arg ])
      base args
  | Ptyp_var name -> pexp_ident ~loc { loc; txt = Lident ("encode_" ^ name) }
  | Ptyp_tuple elems ->
    (match elems with
    | [ a; b ] ->
      [%expr
        Ezlua.Encode.pair [%e encode_expr_of_type a] [%e encode_expr_of_type b]]
    | _ ->
      let n = List.length elems in
      let varnames = List.init n (fun i -> Printf.sprintf "v%d" i) in
      let pat =
        ppat_tuple ~loc
          (List.map (fun vn -> ppat_var ~loc { loc; txt = vn }) varnames)
      in
      let pushes =
        List.mapi
          (fun i (ct, vn) ->
            let enc = encode_expr_of_type ct in
            let v = pexp_ident ~loc { loc; txt = Lident vn } in
            [%expr
              [%e enc] state [%e v];
              Lua_api.Lua.rawseti state (-2) [%e eint ~loc (i + 1)]])
          (List.combine elems varnames)
      in
      let body =
        List.fold_right
          (fun e acc ->
            [%expr
              [%e e];
              [%e acc]])
          ([%expr Lua_api.Lua.newtable state] :: pushes)
          [%expr ()]
      in
      [%expr fun state [%p pat] -> [%e body]])
  | _ -> Location.raise_errorf ~loc "ppx_ezlua: unsupported type"

let rec decode_expr_of_type (ct : core_type) : expression =
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr Ezlua.Decode.int]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> [%expr Ezlua.Decode.float]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
    [%expr Ezlua.Decode.string]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr Ezlua.Decode.bool]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> [%expr Ezlua.Decode.unit]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ arg ]) ->
    let inner = decode_expr_of_type arg in
    [%expr Ezlua.Decode.list [%e inner]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ arg ]) ->
    let inner = decode_expr_of_type arg in
    [%expr Ezlua.Decode.array [%e inner]]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]) ->
    let inner = decode_expr_of_type arg in
    [%expr Ezlua.Decode.option [%e inner]]
  | Ptyp_constr ({ txt = Lident "string_table"; _ }, [ arg ]) ->
    let inner = decode_expr_of_type arg in
    [%expr Ezlua.Decode.string_table [%e inner]]
  | Ptyp_constr ({ txt = Lident name; _ }, args) ->
    let base = pexp_ident ~loc { loc; txt = Lident (name ^ "_of_lua") } in
    List.fold_left
      (fun acc arg -> pexp_apply ~loc acc [ Nolabel, decode_expr_of_type arg ])
      base args
  | Ptyp_var name -> pexp_ident ~loc { loc; txt = Lident ("decode_" ^ name) }
  | Ptyp_tuple elems ->
    (match elems with
    | [ a; b ] ->
      [%expr
        Ezlua.Decode.pair [%e decode_expr_of_type a] [%e decode_expr_of_type b]]
    | _ ->
      let n = List.length elems in
      let varnames = List.init n (fun i -> Printf.sprintf "v%d" i) in
      let result =
        [%expr
          Ok
            [%e
              pexp_tuple ~loc
                (List.map
                   (fun vn -> pexp_ident ~loc { loc; txt = Lident vn })
                   varnames)]]
      in
      let body =
        List.fold_right
          (fun (i, (ct, vn)) inner ->
            let dec = decode_expr_of_type ct in
            [%expr
              match Ezlua.get_index state idx [%e eint ~loc i] [%e dec] with
              | Error e -> Error e
              | Ok [%p ppat_var ~loc { loc; txt = vn }] -> [%e inner]])
          (List.mapi (fun i x -> i + 1, x) (List.combine elems varnames))
          result
      in
      [%expr
        fun state idx ->
          if not (Lua_api.Lua.istable state idx) then
            Error (`Msg "expected table for tuple")
          else
            [%e body]])
  | _ -> Location.raise_errorf ~loc "ppx_ezlua: unsupported type"

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

let wrap_with_encode_params ~loc param_names body =
  List.fold_right
    (fun pname acc ->
      let pat = ppat_var ~loc { loc; txt = "encode_" ^ pname } in
      pexp_fun ~loc Nolabel None pat acc)
    param_names body

let wrap_with_decode_params ~loc param_names body =
  List.fold_right
    (fun pname acc ->
      let pat = ppat_var ~loc { loc; txt = "decode_" ^ pname } in
      pexp_fun ~loc Nolabel None pat acc)
    param_names body

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
          let loc = ld.pld_loc in
          let fname = ld.pld_name.txt in
          let ftype = ld.pld_type in
          let enc = encode_expr_of_type ftype in
          let fexpr = pexp_field ~loc [%expr v] { loc; txt = Lident fname } in
          [%expr
            Ezlua.push_field state [%e estring ~loc fname] [%e enc] [%e fexpr]])
        fields
    in
    List.fold_right
      (fun push_expr acc ->
        [%expr
          [%e push_expr];
          [%e acc]])
      field_pushes [%expr ()]
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
          let loc = ld.pld_loc in
          let fname = ld.pld_name.txt in
          ( { loc; txt = Lident fname },
            pexp_ident ~loc { loc; txt = Lident fname } ))
        fields
    in
    pexp_record ~loc record_fields None
  in
  let of_lua_body =
    List.fold_right
      (fun ld inner ->
        let loc = ld.pld_loc in
        let fname = ld.pld_name.txt in
        let dec = decode_expr_of_type ld.pld_type in
        [%expr
          match Ezlua.get_field state idx [%e estring ~loc fname] [%e dec] with
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
            (`Msg
               (Printf.sprintf "expected table for %s, got %s"
                  [%e estring ~loc type_name]
                  (Lua_api.LuaL.typename state idx)))
        else
          [%e of_lua_body]]
  in
  let to_lua_name = type_name ^ "_to_lua" in
  let of_lua_name = type_name ^ "_of_lua" in
  [
    [%stri
      let [%p ppat_var ~loc { loc; txt = to_lua_name }] =
        [%e wrap_with_encode_params ~loc param_names to_lua_fn_body]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = of_lua_name }] =
        [%e wrap_with_decode_params ~loc param_names of_lua_fn_body]];
  ]

(* ------------------------------------------------------------------ *)
(* Variant deriver *)
(* ------------------------------------------------------------------ *)

let derive_variant ~loc ~type_name ~params
    (constrs : constructor_declaration list) =
  let param_names = extract_param_names params in
  (* to_lua match arms *)
  let to_lua_cases =
    List.map
      (fun cd ->
        let loc = cd.pcd_loc in
        let cname = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          let pat = ppat_construct ~loc { loc; txt = Lident cname } None in
          let body =
            [%expr
              Lua_api.Lua.newtable state;
              Ezlua.push_field state "tag" Ezlua.Encode.string
                [%e estring ~loc cname]]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple [ single ] ->
          let pat =
            ppat_construct ~loc
              { loc; txt = Lident cname }
              (Some (ppat_var ~loc { loc; txt = "v0" }))
          in
          let enc = encode_expr_of_type single in
          let body =
            [%expr
              Lua_api.Lua.newtable state;
              Ezlua.push_field state "tag" Ezlua.Encode.string
                [%e estring ~loc cname];
              Ezlua.push_field state "value" [%e enc] v0]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple multi ->
          let nvars = List.length multi in
          let varnames = List.init nvars (fun i -> Printf.sprintf "v%d" i) in
          let pat =
            ppat_construct ~loc
              { loc; txt = Lident cname }
              (Some
                 (ppat_tuple ~loc
                    (List.map
                       (fun vn -> ppat_var ~loc { loc; txt = vn })
                       varnames)))
          in
          let push_elems =
            List.mapi
              (fun i (ct, vn) ->
                let enc = encode_expr_of_type ct in
                let v = pexp_ident ~loc { loc; txt = Lident vn } in
                [%expr
                  [%e enc] state [%e v];
                  Lua_api.Lua.rawseti state (-2) [%e eint ~loc (i + 1)]])
              (List.combine multi varnames)
          in
          let push_tuple =
            List.fold_right
              (fun e acc ->
                [%expr
                  [%e e];
                  [%e acc]])
              push_elems [%expr ()]
          in
          let body =
            [%expr
              Lua_api.Lua.newtable state;
              Ezlua.push_field state "tag" Ezlua.Encode.string
                [%e estring ~loc cname];
              Lua_api.Lua.newtable state;
              [%e push_tuple];
              Lua_api.Lua.setfield state (-2) "values"]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_record _ ->
          Location.raise_errorf ~loc
            "ppx_ezlua: inline record variants not supported")
      constrs
  in
  let to_lua_fn_body =
    [%expr fun state v -> [%e pexp_match ~loc [%expr v] to_lua_cases]]
  in
  (* of_lua match arms on tag string *)
  let of_lua_cases =
    List.map
      (fun cd ->
        let loc = cd.pcd_loc in
        let cname = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          let pat = ppat_constant ~loc (Pconst_string (cname, loc, None)) in
          let body =
            pexp_construct ~loc { loc; txt = Lident cname } None |> fun e ->
            [%expr Ok [%e e]]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_tuple [ single ] ->
          let pat = ppat_constant ~loc (Pconst_string (cname, loc, None)) in
          let dec = decode_expr_of_type single in
          let body =
            [%expr
              match Ezlua.get_field state idx "value" [%e dec] with
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
                let dec = decode_expr_of_type ct in
                [%expr
                  match
                    Ezlua.get_index state arr_idx__ [%e eint ~loc i] [%e dec]
                  with
                  | Error e -> Error e
                  | Ok [%p ppat_var ~loc { loc; txt = vn }] -> [%e inner]])
              (List.mapi (fun i x -> i + 1, x) (List.combine multi varnames))
              [%expr
                Ok
                  [%e
                    pexp_construct ~loc
                      { loc; txt = Lident cname }
                      (Some
                         (pexp_tuple ~loc
                            (List.map
                               (fun vn ->
                                 pexp_ident ~loc { loc; txt = Lident vn })
                               varnames)))]]
          in
          let body =
            [%expr
              Lua_api.Lua.getfield state idx "values";
              let arr_idx__ = Lua_api.Lua.gettop state in
              let result__ = [%e get_elems] in
              Lua_api.Lua.pop state 1;
              result__]
          in
          case ~lhs:pat ~guard:None ~rhs:body
        | Pcstr_record _ ->
          Location.raise_errorf ~loc
            "ppx_ezlua: inline record variants not supported")
      constrs
  in
  let wildcard_case =
    case
      ~lhs:(ppat_var ~loc { loc; txt = "s" })
      ~guard:None
      ~rhs:[%expr Error (`Msg (Printf.sprintf "unknown variant tag: %s" s))]
  in
  let all_of_cases = of_lua_cases @ [ wildcard_case ] in
  let of_lua_fn_body =
    [%expr
      fun state idx ->
        if not (Lua_api.Lua.istable state idx) then
          Error
            (`Msg
               (Printf.sprintf "expected table for %s, got %s"
                  [%e estring ~loc type_name]
                  (Lua_api.LuaL.typename state idx)))
        else (
          match Ezlua.get_field state idx "tag" Ezlua.Decode.string with
          | Error (`Msg e) -> Error (`Msg ("missing tag: " ^ e))
          | Ok tag__ -> [%e pexp_match ~loc [%expr tag__] all_of_cases]
        )]
  in
  let to_lua_name = "to_lua_" ^ type_name in
  let of_lua_name = "of_lua_" ^ type_name in
  [
    [%stri
      let [%p ppat_var ~loc { loc; txt = to_lua_name }] =
        [%e wrap_with_encode_params ~loc param_names to_lua_fn_body]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = of_lua_name }] =
        [%e wrap_with_decode_params ~loc param_names of_lua_fn_body]];
  ]

(* ------------------------------------------------------------------ *)
(* Type alias deriver *)
(* ------------------------------------------------------------------ *)

let derive_alias ~loc ~type_name ~params (manifest : core_type) =
  let param_names = extract_param_names params in
  let to_lua_name = "to_lua_" ^ type_name in
  let of_lua_name = "of_lua_" ^ type_name in
  [
    [%stri
      let [%p ppat_var ~loc { loc; txt = to_lua_name }] =
        [%e
          wrap_with_encode_params ~loc param_names
            (encode_expr_of_type manifest)]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = of_lua_name }] =
        [%e
          wrap_with_decode_params ~loc param_names
            (decode_expr_of_type manifest)]];
  ]

(* ------------------------------------------------------------------ *)
(* Deriver registration *)
(* ------------------------------------------------------------------ *)

let generate_impl ~loc:_ ~path:_ (_rec_flag, type_decls) =
  List.concat_map
    (fun td ->
      let loc = td.ptype_loc in
      let type_name = td.ptype_name.txt in
      let params = td.ptype_params in
      match td.ptype_kind with
      | Ptype_record fields -> derive_record ~loc ~type_name ~params fields
      | Ptype_variant constrs -> derive_variant ~loc ~type_name ~params constrs
      | Ptype_abstract ->
        (match td.ptype_manifest with
        | Some manifest -> derive_alias ~loc ~type_name ~params manifest
        | None ->
          Location.raise_errorf ~loc
            "ppx_ezlua: abstract type without manifest: %s" type_name)
      | Ptype_open ->
        Location.raise_errorf ~loc "ppx_ezlua: open types not supported")
    type_decls

let generate_intf ~loc:_ ~path:_ (_rec_flag, type_decls) =
  List.concat_map
    (fun td ->
      let loc = td.ptype_loc in
      let type_name = td.ptype_name.txt in
      let param_types =
        List.filter_map
          (fun (ct, _) ->
            match ct.ptyp_desc with
            | Ptyp_var _ -> Some ct
            | _ -> None)
          td.ptype_params
      in
      let applied =
        ptyp_constr ~loc { loc; txt = Lident type_name } param_types
      in
      let ezlua name arg =
        ptyp_constr ~loc { loc; txt = Ldot (Lident "Ezlua", name) } [ arg ]
      in
      let add_params base_type codec_name =
        List.fold_right
          (fun ct acc -> ptyp_arrow ~loc Nolabel (ezlua codec_name ct) acc)
          param_types
          (ezlua codec_name base_type)
      in
      let mk name type_ =
        psig_value ~loc
          (value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
      in
      [
        mk ("to_lua_" ^ type_name) (add_params applied "to_lua");
        mk ("of_lua_" ^ type_name) (add_params applied "of_lua");
      ])
    type_decls

let ezlua_deriver =
  Deriving.add "ezlua"
    ~str_type_decl:(Deriving.Generator.make Deriving.Args.empty generate_impl)
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.empty generate_intf)

(* ------------------------------------------------------------------ *)
(* let%lua extension *)
(* ------------------------------------------------------------------ *)

let extract_one_param fp =
  let loc = fp.pparam_loc in
  match fp.pparam_desc with
  | Pparam_val (Nolabel, None, pat) ->
    (match pat.ppat_desc with
    | Ppat_constraint (inner_pat, ct) ->
      let pname =
        match inner_pat.ppat_desc with
        | Ppat_var { txt; _ } -> txt
        | _ ->
          Location.raise_errorf ~loc
            "let%%lua: expected simple variable pattern"
      in
      pname, ct
    | _ ->
      Location.raise_errorf ~loc
        "let%%lua: all arguments must have type annotations")
  | _ -> Location.raise_errorf ~loc "let%%lua: unexpected parameter kind"

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
        Location.raise_errorf ~loc
          "let%%lua: return type annotation required (e.g., : int)"
    in
    let body_expr =
      match body with
      | Pfunction_body e -> e
      | Pfunction_cases _ ->
        Location.raise_errorf ~loc "let%%lua: function cases body not supported"
    in
    named_params, (body_expr, ret_ct)
  | Pexp_constraint (inner, ret_ct) -> [], (inner, ret_ct)
  | _ ->
    Location.raise_errorf ~loc
      "let%%lua: expected fun expression with type annotations"

let expand_lua_let ~loc ~path:_ pat expr =
  let fn_name =
    match pat.ppat_desc with
    | Ppat_var { txt; _ } -> txt
    | _ -> Location.raise_errorf ~loc "let%%lua: expected a simple name binding"
  in
  let params, (body, ret_type) = extract_params expr in
  let orig_fn =
    List.fold_right
      (fun (pname, ct) acc ->
        let ploc = ct.ptyp_loc in
        let p =
          ppat_constraint ~loc:ploc
            (ppat_var ~loc:ploc { loc = ploc; txt = pname })
            ct
        in
        pexp_fun ~loc Nolabel None p acc)
      params
      (pexp_constraint ~loc body ret_type)
  in
  let result_bind =
    [%expr
      let result__ =
        [%e
          pexp_apply ~loc
            (pexp_ident ~loc { loc; txt = Lident fn_name })
            (List.map
               (fun (pname, _) ->
                 Nolabel, pexp_ident ~loc { loc; txt = Lident pname })
               params)]
      in
      [%e
        match ret_type.ptyp_desc with
        | Ptyp_constr ({ txt = Lident "unit"; _ }, []) ->
          [%expr
            ignore result__;
            0]
        | _ ->
          let ret_enc = encode_expr_of_type ret_type in
          [%expr
            [%e ret_enc] lua_state result__;
            1]]]
  in
  let wrapper_body =
    List.fold_right
      (fun (i, (pname, ct)) inner ->
        let dec = decode_expr_of_type ct in
        let ploc = ct.ptyp_loc in
        [%expr
          match [%e dec] lua_state [%e eint ~loc:ploc i] with
          | Error (`Msg msg) ->
            Lua_api.LuaL.error lua_state "%s"
              [%e
                estring ~loc:ploc (Printf.sprintf "bad arg %d (%s): " i pname)
                |> fun prefix -> [%expr [%e prefix] ^ msg]]
          | Ok [%p ppat_var ~loc:ploc { loc = ploc; txt = pname }] -> [%e inner]])
      (List.mapi (fun i x -> i + 1, x) params)
      result_bind
  in
  let wrapper_fn = [%expr fun lua_state -> [%e wrapper_body]] in
  let wrapper_name = fn_name ^ "_lua" in
  [
    [%stri let [%p ppat_var ~loc { loc; txt = fn_name }] = [%e orig_fn]];
    [%stri
      let [%p ppat_var ~loc { loc; txt = wrapper_name }] :
          Lua_api_lib.oCamlFunction =
        [%e wrapper_fn]];
  ]

let lua_ext =
  Extension.declare_inline "lua" Extension.Context.structure_item
    Ast_pattern.(
      pstr
        (pstr_value nonrecursive
           (value_binding ~pat:__ ~expr:__ ~constraint_:drop ^:: nil)
        ^:: nil))
    expand_lua_let

let () = Driver.register_transformation "lua" ~extensions:[ lua_ext ]
let _ = ezlua_deriver
