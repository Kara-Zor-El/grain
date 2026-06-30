open Grain_tree_sitter;

module Tree = Grain_tree_sitter.Tree;
type position = Grain_tree_sitter.position;
type parse_tree = Grain_tree_sitter.parse_tree;

type local_binding = {name: string};

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

type t = {
  kind: context_kind,
  prefix: string,
  replace_range: Protocol.range,
};
