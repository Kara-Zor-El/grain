open Grain_typed;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let value_kind = desc =>
  switch (desc.Types.val_repr) {
  | ReprFunction(_) => Function
  | _ => Value
  };

let env_values = (~context, env) =>
  Env.fold_values(
    (name, _path, desc, acc) => {
      [
        make_candidate(
          ~source=Typedtree,
          ~label=name,
          ~kind=value_kind(desc),
          ~detail=
            Printtyp.string_of_value_description(
              ~ident=Ident.create(name),
              desc,
            ),
          ~sort_group=Sort_group.Typed,
          ~context,
          (),
        ),
        ...acc,
      ]
    },
    None,
    env,
    [],
  );

let type_kind = decl =>
  switch (decl.Types.type_kind) {
  | TDataRecord(_) => Class
  | TDataAbstract
  | TDataOpen
  | TDataVariant(_) => TypeParameter
  };

let env_types = (~context, env) =>
  Env.fold_types(
    (name, _path, (decl, _descrs), acc) =>
      switch (decl.Types.type_kind) {
      | TDataVariant(_) => acc
      | _ => [
          make_candidate(
            ~source=Typedtree,
            ~label=name,
            ~kind=type_kind(decl),
            ~detail=
              Printtyp.string_of_type_declaration(
                ~ident=Ident.create(name),
                decl,
              ),
            ~sort_group=Sort_group.Typed,
            ~context,
            (),
          ),
          ...acc,
        ]
      },
    None,
    env,
    [],
  );

let env_modules = (~context, ~sort_group=Sort_group.Typed, env) =>
  Env.fold_modules(
    (name, _path, _decl, acc) => {
      [
        make_candidate(
          ~source=Typedtree,
          ~label=name,
          ~kind=Module,
          ~sort_group,
          ~context,
          (),
        ),
        ...acc,
      ]
    },
    None,
    env,
    [],
  );

let constructor_matches = (~env, ~expected_type, cstr) =>
  switch (expected_type) {
  | None => true
  | Some(expected_type) =>
    try(Env.same_constr^(env, expected_type, cstr.Types.cstr_res)) {
    | Not_found => false
    | exn =>
      Trace.log(
        "Completion typed_env constructor match failed: "
        ++ Printexc.to_string(exn),
      );
      false;
    }
  };

let label_matches = (~env, ~expected_type, label) =>
  switch (expected_type) {
  | None => true
  | Some(expected_type) =>
    try(Env.same_constr^(env, expected_type, label.Types.lbl_res)) {
    | Not_found => false
    | exn =>
      Trace.log(
        "Completion typed_env label match failed: " ++ Printexc.to_string(exn),
      );
      false;
    }
  };

let env_constructors = (~context, ~expected_type=?, env) =>
  Env.fold_constructors(
    (cstr, acc) =>
      if (constructor_matches(~env, ~expected_type, cstr)) {
        [
          make_candidate(
            ~source=Typedtree,
            ~label=cstr.Types.cstr_name,
            ~kind=Constructor,
            ~detail=Printtyp.string_of_type_scheme(cstr.Types.cstr_res),
            ~sort_group=Sort_group.Typed,
            ~context,
            (),
          ),
          ...acc,
        ];
      } else {
        acc;
      },
    None,
    env,
    [],
  );

let env_labels = (~context, ~expected_type=?, env) =>
  Env.fold_labels(
    (label, acc) =>
      if (label_matches(~env, ~expected_type, label)) {
        [
          make_candidate(
            ~source=Typedtree,
            ~label=label.Types.lbl_name,
            ~kind=Field,
            ~detail=Printtyp.string_of_type_scheme(label.Types.lbl_arg),
            ~sort_group=Sort_group.Typed,
            ~context,
            (),
          ),
          ...acc,
        ];
      } else {
        acc;
      },
    None,
    env,
    [],
  );

let in_scope_candidates = (~context, env) =>
  env_values(~context, env)
  @ env_types(~context, env)
  @ env_modules(~context, env)
  @ env_constructors(~context, env)
  @ env_labels(~context, env);

let pattern_candidates = (~context, ~expected_type=?, env) =>
  env_constructors(~context, ~expected_type?, env)
  @ env_labels(~context, ~expected_type?, env);

let import_path_candidates = (~context, env) =>
  env_modules(~context, ~sort_group=Sort_group.Import, env);
