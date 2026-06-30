open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let local_candidates = (~context, bindings) =>
  List.map(
    (binding: Syntax.Types.local_binding) =>
      make_candidate(
        ~source=TreeSitter,
        ~label=binding.name,
        ~kind=Variable,
        ~sort_group=Sort_group.Local,
        ~context,
        (),
      ),
    bindings,
  );
