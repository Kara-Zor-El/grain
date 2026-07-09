open Grain_tree_sitter;

module Tree = Grain_tree_sitter.Tree;
type position = Grain_tree_sitter.position;
type parse_tree = Grain_tree_sitter.parse_tree;

type local_binding = {name: string};

type keyword_slot =
  | ToplevelStatement
  | LetHeader
  | LetAfterModifier
  | BlockStatement
  | LoopBody
  | ExpressionStart
  | ImportIncludeTail
  | ProvideTail
  | ProvideTypeTail
  | MatchGuard
  | IfTail
  | RecordFieldHeader;

type context_kind =
  | InScope
  | MemberAccess(string)
  | ImportPath
  | ImportFilePath
  | ImportModuleName(string)
  | PatternContext
  | MatchGuardKeyword
  | DocblockContext
  | CallArgument
  | Suppressed
  | TypeReference;

type t = {
  kind: context_kind,
  keyword_slot: option(keyword_slot),
  prefix: string,
  replace_range: Protocol.range,
};
