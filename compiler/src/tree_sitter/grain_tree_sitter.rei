type context_kind =
  | InScope
  | MemberAccess(string)
  | ImportPath
  | ImportFilePath
  | ImportModuleName(string)
  | KeywordContext
  | PatternContext
  | DocblockContext
  | CallArgument;

type position = {
  line: int,
  character: int,
};

type range = {
  range_start: position,
  range_end: position,
};

type context = {
  kind: context_kind,
  prefix: string,
  replace_range: range,
};

type local_binding = {name: string};

type parse_tree;

let parse: string => parse_tree;
let reparse: (~old: parse_tree, string) => parse_tree;
let cached_tree:
  (~parse_trees: Hashtbl.t('key, (int, parse_tree)), 'key, string) =>
  parse_tree;
let context_at: (parse_tree, position) => context;
let callee_name_before_call: (parse_tree, position) => option(string);
let local_bindings_before: (parse_tree, position) => list(local_binding);
let on_ready: (unit => unit) => unit;
