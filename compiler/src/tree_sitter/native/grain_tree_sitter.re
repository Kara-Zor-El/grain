open Tree_sitter;

module Backend = {
  type parser = Parser.t;
  type tree = Tree.t;
  type node = Node.t;

  module Parser = {
    type t = parser;

    let create = () => {
      let language = Grammar.language();
      Parser.create(language);
    };

    let parse_string = (~old=?, parser, source) =>
      switch (old) {
      | None => Parser.parse_string(parser, source)
      | Some(old_tree) => Parser.parse_string(~old=old_tree, parser, source)
      };
  };

  module Tree = {
    type t = tree;

    let root_node = Tree.root_node;
    let copy = Tree.copy;

    let edit = (tree, edit: Edit.t) => {
      Tree.edit(
        tree,
        ~start_byte=edit.start_byte,
        ~old_end_byte=edit.old_end_byte,
        ~new_end_byte=edit.new_end_byte,
        ~start_point={
          Tree_sitter.row: edit.start_point.row,
          Tree_sitter.column: edit.start_point.column,
        },
        ~old_end_point={
          Tree_sitter.row: edit.old_end_point.row,
          Tree_sitter.column: edit.old_end_point.column,
        },
        ~new_end_point={
          Tree_sitter.row: edit.new_end_point.row,
          Tree_sitter.column: edit.new_end_point.column,
        },
      );
    };
  };

  module Node = {
    type t = node;

    let kind = Node.kind;
    let start_byte = Node.start_byte;
    let end_byte = Node.end_byte;
    let start_point = node => {
      let point = Node.start_point(node);
      (point.row, point.column);
    };
    let end_point = node => {
      let point = Node.end_point(node);
      (point.row, point.column);
    };
    let parent = Node.parent;
    let named_child = Node.named_child;
    let named_child_count = Node.named_child_count;
    let child_by_field_name = Node.child_by_field_name;
    let named_descendant_for_point = (node, ~row, ~column) => {
      let point = {
        row,
        column,
      };
      Node.named_descendant_for_point_range(node, ~start=point, ~end_=point);
    };
    let descendant_for_point = (node, ~row, ~column) => {
      let point = {
        row,
        column,
      };
      Node.descendant_for_point_range(node, ~start=point, ~end_=point);
    };
  };
};

include Grain_tree_sitter_core.Grain_tree_sitter;
include Grain_tree_sitter_core.Grain_tree_sitter.Make(Backend);

let on_ready = f => f();
