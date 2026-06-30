open Grain_typed;
open Lsp_types;
open Completion_types;
open Pipeline;
open Request;

module RequestParams = Completion_types.RequestParams;
module ResponseResult = Completion_types.ResponseResult;

let send_completion = (~id: Protocol.message_id, ~is_incomplete, items) =>
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({
      is_incomplete,
      items,
    }),
  );

let local_candidates = (request: Request.t) =>
  Sources.Locals.local_candidates(
    ~context=request.context,
    request.local_bindings,
  );

let collect_candidates = (request: Request.t) => {
  switch (request.context.kind) {
  | DocblockContext =>
    Sources.Docblock.docblock_attribute_candidates(~context=request.context)
  | MemberAccess(qualifier) =>
    switch (request.typed) {
    | None => []
    | Some({env}) =>
      Sources.Member_access.module_candidates(
        ~context=request.context,
        ~qualifier,
        env,
      )
    }
  | CallArgument =>
    switch (request.typed) {
    | None => []
    | Some({env}) =>
      Sources.Call_arguments.argument_candidates(
        ~context=request.context,
        ~env,
        ~callee_fallback=request.callee_fallback,
        Request.application(request),
      )
    }
  | PatternContext =>
    local_candidates(request)
    @ (
      switch (request.typed) {
      | None => []
      | Some({env}) =>
        Sources.Typed_env.pattern_candidates(
          ~context=request.context,
          ~expected_type=?Request.expected_type(request),
          env,
        )
      }
    )
  | ImportPath =>
    local_candidates(request)
    @ (
      switch (request.typed) {
      | None => []
      | Some({env}) =>
        Sources.Typed_env.import_path_candidates(
          ~context=request.context,
          env,
        )
      }
    )
  | ImportFilePath =>
    Sources.Import_paths.file_path_candidates(
      ~context=request.context,
      ~uri=request.uri,
    )
  | ImportModuleName(import_path) =>
    Sources.Import_paths.include_module_candidates(
      ~context=request.context,
      ~uri=request.uri,
      ~import_path,
    )
  | KeywordContext
  | InScope =>
    local_candidates(request)
    @ (
      switch (request.typed) {
      | None => []
      | Some({env}) =>
        Sources.Typed_env.in_scope_candidates(~context=request.context, env)
      }
    )
    @ Sources.Keywords.keyword_candidates(~context=request.context)
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      ~parse_trees:
         Hashtbl.t(Protocol.uri, (int, Grain_tree_sitter.parse_tree)),
      params: RequestParams.t,
    ) => {
  let uri = params.text_document.uri;
  switch (Hashtbl.find_opt(documents, uri)) {
  | None => send_completion(~id, ~is_incomplete=false, [])
  | Some(source) =>
    let tree = Grain_tree_sitter.cached_tree(~parse_trees, uri, source);
    let request =
      Request.make(
        ~uri,
        ~lsp_position=params.position,
        ~source,
        ~tree,
        ~compiled_code,
      );
    let candidates = collect_candidates(request);
    let (is_incomplete, items) =
      finalize_candidates(~prefix=request.context.prefix, candidates);
    send_completion(~id, ~is_incomplete, items);
  };
};
