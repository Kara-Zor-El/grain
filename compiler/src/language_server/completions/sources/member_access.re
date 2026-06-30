open Grain_parsing;
open Grain_typed;
open Grain_diagnostics;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let is_ident = name =>
  switch (name.[0]) {
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' => true
  | _ => false
  };

let module_candidates = (~context, ~qualifier, env) =>
  try({
    let path =
      Env.lookup_module(~load=false, Identifier.parse(qualifier), None, env);
    let md = Env.find_module(path, None, env);
    Modules.get_provides(md)
    |> List.filter((provide: Modules.export) => provide.kind != Modules.Enum)
    |> List.map((provide: Modules.export) => {
         let name = provide.name;
         let label = is_ident(name) ? name : Printf.sprintf("(%s)", name);
         let completion_kind =
           switch (provide.kind) {
           | Modules.Function => Function
           | Modules.Value => Value
           | Modules.Enum => Enum
           | Modules.Record => Struct
           | Modules.Abstract => TypeParameter
           | Modules.Exception => Constructor
           | Modules.Module => Module
           };
         make_candidate(
           ~source=Typedtree,
           ~label,
           ~kind=completion_kind,
           ~detail=
             is_ident(name)
               ? provide.signature
               : Printf.sprintf(
                   "%s.(%s) %s",
                   qualifier,
                   name,
                   provide.signature,
                 ),
           ~sort_group=
             is_ident(name)
               ? Sort_group.Typed
               : (
                 switch (provide.kind) {
                 | Modules.Module => Sort_group.ModuleMember
                 | _ => Sort_group.Typed
                 }
               ),
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
