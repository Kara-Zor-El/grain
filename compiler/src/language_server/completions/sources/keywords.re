open Grain_typed;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;
open Syntax.Types;

type keyword_entry = {
  label: string,
  snippet: option(string),
  sort_label: option(string),
};

let is_word_keyword = label =>
  String.length(label) > 0
  && String.for_all(Syntax.Text.is_ident_char, label);

let toplevel_keywords = [
  {
    label: "from",
    snippet: Some("from \"$1\" include $2"),
    sort_label: None,
  },
  {
    label: "let",
    snippet: None,
    sort_label: None,
  },
  {
    label: "provide",
    snippet: None,
    sort_label: None,
  },
  {
    label: "module",
    snippet: None,
    sort_label: None,
  },
  {
    label: "foreign",
    snippet: None,
    sort_label: None,
  },
  {
    label: "primitive",
    snippet: None,
    sort_label: None,
  },
  {
    label: "exception",
    snippet: None,
    sort_label: None,
  },
  {
    label: "enum",
    snippet: None,
    sort_label: None,
  },
  {
    label: "record",
    snippet: None,
    sort_label: None,
  },
  {
    label: "type",
    snippet: None,
    sort_label: None,
  },
  {
    label: "include",
    snippet: None,
    sort_label: None,
  },
  {
    label: "if",
    snippet: None,
    sort_label: None,
  },
  {
    label: "match",
    snippet: None,
    sort_label: None,
  },
  {
    label: "while",
    snippet: None,
    sort_label: None,
  },
  {
    label: "for",
    snippet: None,
    sort_label: None,
  },
  {
    label: "assert",
    snippet: None,
    sort_label: None,
  },
];

let let_header_keywords = [
  {
    label: "rec",
    snippet: None,
    sort_label: None,
  },
  {
    label: "mut",
    snippet: None,
    sort_label: None,
  },
];

let block_statement_keywords = [
  {
    label: "let",
    snippet: None,
    sort_label: None,
  },
  {
    label: "return",
    snippet: None,
    sort_label: None,
  },
  {
    label: "if",
    snippet: None,
    sort_label: None,
  },
  {
    label: "match",
    snippet: None,
    sort_label: None,
  },
  {
    label: "while",
    snippet: None,
    sort_label: None,
  },
  {
    label: "for",
    snippet: None,
    sort_label: None,
  },
  {
    label: "assert",
    snippet: None,
    sort_label: None,
  },
];

let loop_body_keywords = [
  {
    label: "break",
    snippet: None,
    sort_label: None,
  },
  {
    label: "continue",
    snippet: None,
    sort_label: None,
  },
  ...block_statement_keywords,
];

let expression_start_keywords = [
  {
    label: "if",
    snippet: None,
    sort_label: None,
  },
  {
    label: "match",
    snippet: None,
    sort_label: None,
  },
  {
    label: "while",
    snippet: None,
    sort_label: None,
  },
  {
    label: "for",
    snippet: None,
    sort_label: None,
  },
  {
    label: "assert",
    snippet: None,
    sort_label: None,
  },
  {
    label: "let",
    snippet: None,
    sort_label: None,
  },
  {
    label: "true",
    snippet: None,
    sort_label: None,
  },
  {
    label: "false",
    snippet: None,
    sort_label: None,
  },
  {
    label: "void",
    snippet: None,
    sort_label: None,
  },
];

let import_include_keywords = [
  {
    label: "include",
    snippet: None,
    sort_label: Some("include"),
  },
];

let provide_tail_keywords = [
  {
    label: "foreign",
    snippet: None,
    sort_label: None,
  },
  {
    label: "primitive",
    snippet: None,
    sort_label: None,
  },
  {
    label: "exception",
    snippet: None,
    sort_label: None,
  },
  {
    label: "enum",
    snippet: None,
    sort_label: None,
  },
  {
    label: "record",
    snippet: None,
    sort_label: None,
  },
  {
    label: "type",
    snippet: None,
    sort_label: None,
  },
  {
    label: "module",
    snippet: None,
    sort_label: None,
  },
  {
    label: "let",
    snippet: None,
    sort_label: None,
  },
];

let match_guard_keywords = [
  {
    label: "when",
    snippet: None,
    sort_label: None,
  },
];
let if_tail_keywords = [
  {
    label: "else",
    snippet: None,
    sort_label: None,
  },
];
let record_field_keywords = [
  {
    label: "mut",
    snippet: None,
    sort_label: None,
  },
];

let keywords_for_slot =
  fun
  | ToplevelStatement => toplevel_keywords
  | LetHeader => let_header_keywords
  | LetAfterModifier => []
  | BlockStatement => block_statement_keywords
  | LoopBody => loop_body_keywords
  | ExpressionStart => expression_start_keywords
  | ImportIncludeTail => import_include_keywords
  | ProvideTail => provide_tail_keywords
  | ProvideTypeTail => []
  | MatchGuard => match_guard_keywords
  | IfTail => if_tail_keywords
  | RecordFieldHeader => record_field_keywords;

let expected_matches_path = (~env, expected_type, path) =>
  switch (Ctype.expand_head(env, expected_type).desc) {
  | TTyConstr(expected_path, [], _) => Path.same(expected_path, path)
  | _ => false
  };

let keyword_matches_expected_type = (~env, ~expected_type, label) =>
  switch (expected_type, env) {
  | (Some(expected_type), Some(env)) =>
    switch (label) {
    | "true"
    | "false" =>
      expected_matches_path(~env, expected_type, Builtin_types.path_bool)
    | "void" =>
      expected_matches_path(~env, expected_type, Builtin_types.path_void)
    | "let" => false
    | "assert"
    | "while"
    | "for" =>
      expected_matches_path(~env, expected_type, Builtin_types.path_void)
    | _ => true
    }
  | _ => true
  };

let keyword_candidates =
    (~context, ~env=?, ~expected_type=?, slot: keyword_slot) => {
  keywords_for_slot(slot)
  |> List.filter(entry => is_word_keyword(entry.label))
  |> List.filter(entry =>
       keyword_matches_expected_type(~env, ~expected_type, entry.label)
     )
  |> List.map(entry => {
       let insert_text_format =
         switch (entry.snippet) {
         | Some(_) => Some(SnippetFormat)
         | None => None
         };
       let command =
         switch (entry.snippet) {
         | Some(_) =>
           let command: Protocol.command = {
             title: "",
             command: "editor.action.triggerSuggest",
           };
           Some(command);
         | None => None
         };
       make_candidate(
         ~source=KeywordSource,
         ~label=entry.label,
         ~kind=Keyword,
         ~sort_group=Sort_group.Keyword,
         ~context,
         ~insert_text=?entry.snippet,
         ~insert_text_format?,
         ~command?,
         ~sort_label=?entry.sort_label,
         (),
       );
     });
};
