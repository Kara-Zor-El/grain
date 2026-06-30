open Types;

let pattern_name = (tree: parse_tree, node: Tree.node) =>
  switch (Tree.node_kind(node)) {
  | "variable_pattern"
  | "identifier"
  | "upper_identifier" => Some(Tree.node_text(tree, node))
  | _ => None
  };

let binding_from_value_binding = (tree: parse_tree, node: Tree.node) =>
  switch (Tree.node_child_by_field_name(node, "pattern")) {
  | None => None
  | Some(pattern) => pattern_name(tree, pattern)
  };

let binding_from_module = (tree: parse_tree, node: Tree.node) =>
  switch (Tree.node_child_by_field_name(node, "name")) {
  | None => None
  | Some(name_node) => Some(Tree.node_text(tree, name_node))
  };

let local_bindings_before =
    (tree: parse_tree, pos: position): list(local_binding) => {
  let source = Tree.source(tree);
  let cursor_byte = Text.byte_offset(source, pos);
  let root = Tree.root(tree);
  let rec collect =
          (tree, node: Tree.node, cursor_byte, acc: list(local_binding)) => {
    let acc =
      if (Tree.node_start_byte(node) >= cursor_byte) {
        acc;
      } else {
        switch (Tree.node_kind(node)) {
        | "value_binding" =>
          switch (binding_from_value_binding(tree, node)) {
          | Some(name) => [{name: name}, ...acc]
          | None => acc
          }
        | "module_declaration"
        | "module_header" =>
          switch (binding_from_module(tree, node)) {
          | Some(name) => [{name: name}, ...acc]
          | None => acc
          }
        | _ => acc
        };
      };
    let count = Tree.node_named_child_count(node);
    let rec loop = (idx, acc) =>
      if (idx >= count) {
        acc;
      } else {
        switch (Tree.node_named_child(node, idx)) {
        | None => loop(idx + 1, acc)
        | Some(child) =>
          loop(idx + 1, collect(tree, child, cursor_byte, acc))
        };
      };
    loop(0, acc);
  };
  collect(tree, root, cursor_byte, []);
};
