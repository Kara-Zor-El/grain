open Grain_typed;
open Protocol;
open Sourcetree;

type compiler_query = {
  env: Env.t,
  sourcetree_results: list(Sourcetree.node),
};

type t = {
  uri,
  position,
  source: string,
  context: Syntax.Types.t,
  local_bindings: list(Syntax.Types.local_binding),
  callee_fallback: option(string),
  typed: option(compiler_query),
};

let env_from_results =
    (
      ~typed_program: Typedtree.typed_program,
      ~results: list(Sourcetree.node),
    ) => {
  let rec find_env_in_results = results =>
    switch (results) {
    | [] => typed_program.env
    | [Sourcetree.Value({env}), ..._] => env
    | [Sourcetree.Pattern({env}), ..._] => env
    | [Sourcetree.Application({fun_expr}), ..._] =>
      fun_expr.Typedtree.exp_env
    | [_, ...rest] => find_env_in_results(rest)
    };
  find_env_in_results(results);
};

let make =
    (
      ~uri,
      ~lsp_position: position,
      ~source,
      ~tree,
      ~compiled_code: Hashtbl.t(uri, Lsp_types.code),
    ) => {
  let ts_pos: Grain_tree_sitter.position = {
    line: lsp_position.line,
    character: lsp_position.character,
  };
  let context = Syntax.At_point.at(tree, ts_pos);
  let local_bindings = Syntax.Bindings.local_bindings_before(tree, ts_pos);
  let callee_fallback = Syntax.Calls.callee_name_before_call(tree, ts_pos);
  let typed =
    switch (Hashtbl.find_opt(compiled_code, uri)) {
    | None => None
    | Some({program: compiled_program, sourcetree}) =>
      let sourcetree_results = Sourcetree.query(lsp_position, sourcetree);
      Some({
        env:
          env_from_results(
            ~typed_program=compiled_program,
            ~results=sourcetree_results,
          ),
        sourcetree_results,
      });
    };
  {
    uri,
    position: lsp_position,
    source,
    context,
    local_bindings,
    callee_fallback,
    typed,
  };
};

let expected_type = request =>
  switch (request.typed) {
  | None => None
  | Some({sourcetree_results}) =>
    let rec find_expected_type_in_results = results =>
      switch (results) {
      | [] => None
      | [Sourcetree.Pattern({expected_type: Some(expected_type)}), ..._] =>
        Some(expected_type)
      | [_, ...rest] => find_expected_type_in_results(rest)
      };
    find_expected_type_in_results(sourcetree_results);
  };

let application = request =>
  switch (request.typed) {
  | None => None
  | Some({sourcetree_results}) =>
    let rec find_application_in_results = results =>
      switch (results) {
      | [] => None
      | [Sourcetree.Application({fun_expr, args, loc: _}), ..._] =>
        Some((fun_expr, args))
      | [_, ...rest] => find_application_in_results(rest)
      };
    find_application_in_results(sourcetree_results);
  };
