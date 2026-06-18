type parser;
type tree;
type node;

module Parser = {
  type t = parser;

  external create: unit => t = "grain_ts_parser_create";
  external parse_string_plain: (t, string) => tree = "grain_ts_parser_parse";
  external parse_string_incremental: (t, string, tree) => tree =
    "grain_ts_parser_parse_incremental";
  let parse_string = (~old=?, parser, source) =>
    switch (old) {
    | None => parse_string_plain(parser, source)
    | Some(old_tree) => parse_string_incremental(parser, source, old_tree)
    };
};

module Tree = {
  type t = tree;

  external root_node: t => node = "grain_ts_tree_root_node";
  external copy: t => t = "grain_ts_tree_copy";
  external edit_raw: (t, int, int, int, int, int, int, int, int, int) => unit =
    "grain_ts_tree_edit";
  let edit = (tree, edit: Edit.t) => {
    edit_raw(
      tree,
      edit.start_byte,
      edit.old_end_byte,
      edit.new_end_byte,
      edit.start_point.row,
      edit.start_point.column,
      edit.old_end_point.row,
      edit.old_end_point.column,
      edit.new_end_point.row,
      edit.new_end_point.column,
    );
  };
};

module Node = {
  type t = node;

  external kind: t => string = "grain_ts_node_kind";
  external start_byte: t => int = "grain_ts_node_start_byte";
  external end_byte: t => int = "grain_ts_node_end_byte";
  external start_point: t => (int, int) = "grain_ts_node_start_point";
  external end_point: t => (int, int) = "grain_ts_node_end_point";
  external parent: t => option(t) = "grain_ts_node_parent";
  external named_child: (t, int) => option(t) = "grain_ts_node_named_child";
  external named_child_count: t => int = "grain_ts_node_named_child_count";
  external child_by_field_name: (t, string) => option(t) =
    "grain_ts_node_child_by_field_name";
  external named_descendant_for_point_raw:
    (t, int, int, int, int) => option(t) =
    "grain_ts_node_named_descendant_for_point";
  external descendant_for_point_raw: (t, int, int, int, int) => option(t) =
    "grain_ts_node_descendant_for_point";

  let named_descendant_for_point = (node, ~row, ~column) =>
    named_descendant_for_point_raw(node, row, column, row, column);
  let descendant_for_point = (node, ~row, ~column) =>
    descendant_for_point_raw(node, row, column, row, column);
};
