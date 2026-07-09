open Grain_parsing;
open Grain_typed;
open Grain_diagnostics;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let member_candidates =
    (~context, ~qualifier, ~include_export, ~completion_kind, env) =>
  try({
    let path =
      Env.lookup_module(~load=false, Identifier.parse(qualifier), None, env);
    let md = Env.find_module(path, None, env);
    Modules.get_provides(md)
    |> List.filter((provide: Modules.export) =>
         include_export(provide.kind)
       )
    |> List.map((provide: Modules.export) => {
         let name = provide.name;
         let is_operator =
           String.length(name) == 0 || !Syntax.Text.is_ident_char(name.[0]);
         make_candidate(
           ~source=Typedtree,
           ~label=is_operator ? Printf.sprintf("(%s)", name) : name,
           ~kind=completion_kind(provide.kind),
           ~detail=
             is_operator
               ? Printf.sprintf(
                   "%s.(%s) %s",
                   qualifier,
                   name,
                   provide.signature,
                 )
               : provide.signature,
           ~sort_group=
             if (is_operator) {Sort_group.Operator} else {Sort_group.Typed},
           ~filter_text=name,
           ~context,
           (),
         );
       });
  }) {
  | Not_found => []
  | exn =>
    Trace.log(
      "Completion member_access failed for qualifier `"
      ++ qualifier
      ++ "`: "
      ++ Printexc.to_string(exn),
    );
    [];
  };

let module_candidates = (~context, ~qualifier, env) =>
  member_candidates(
    ~context,
    ~qualifier,
    ~include_export=
      kind =>
        switch (kind) {
        | Modules.Enum => false
        | _ => true
        },
    ~completion_kind=
      kind =>
        switch (kind) {
        | Modules.Function => Function
        | Modules.Value => Value
        | Modules.Enum => Enum
        | Modules.Record => Struct
        | Modules.Abstract => TypeParameter
        | Modules.Exception => Constructor
        | Modules.Module => Module
        },
    env,
  );

let type_candidates = (~context, ~qualifier, env) =>
  member_candidates(
    ~context,
    ~qualifier,
    ~include_export=
      kind =>
        switch (kind) {
        | Modules.Record
        | Modules.Enum
        | Modules.Abstract
        | Modules.Module => true
        | Modules.Function
        | Modules.Value
        | Modules.Exception => false
        },
    ~completion_kind=
      kind =>
        switch (kind) {
        | Modules.Record => Class
        | Modules.Enum
        | Modules.Abstract => TypeParameter
        | Modules.Module => Module
        | Modules.Function => Function
        | Modules.Value => Value
        | Modules.Exception => Constructor
        },
    env,
  );
