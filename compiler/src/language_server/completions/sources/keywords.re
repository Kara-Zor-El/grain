open Grain_parsing;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let keyword_candidates = (~context) => {
  Keywords.all
  |> List.map(label =>
       make_candidate(
         ~source=KeywordSource,
         ~label,
         ~kind=Keyword,
         ~sort_group=Sort_group.Keyword,
         ~context,
         (),
       )
     );
};
