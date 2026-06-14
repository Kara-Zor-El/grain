include Grain_tree_sitter_core.Grain_tree_sitter;
include Grain_tree_sitter_core.Grain_tree_sitter.Make(Tree_sitter_web);

external on_ready: (unit => unit) => unit = "grain_ts_on_ready";
