open Types;

let cursor_in_match_body_braces = (pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) != "match_body") {
    false;
  } else {
    let (start_row, start_column) = Tree.node_start_point(node);
    let (end_row, end_column) = Tree.node_end_point(node);
    !Tree.cursor_before(pos, start_row, start_column)
    && !Tree.cursor_after(pos, end_row, end_column);
  };

let cursor_in_branch_pattern = (pos: position, node: Tree.node) =>
  switch (Tree.node_kind(node)) {
  | "match_branch" =>
    switch (Tree.node_child_by_field_name(node, "pattern")) {
    | None => false
    | Some(pattern) =>
      let (start_row, start_column) = Tree.node_start_point(pattern);
      let (end_row, end_column) = Tree.node_end_point(pattern);
      !Tree.cursor_before(pos, start_row, start_column)
      && !Tree.cursor_after(pos, end_row, end_column);
    }
  | _ => false
  };

let in_match_body_from_node = (pos: position, node: Tree.node) =>
  switch (Tree.ancestor(node, n => Tree.node_kind(n) == "match_body")) {
  | Some(_) => true
  | None =>
    switch (Tree.ancestor(node, n => Tree.node_kind(n) == "match_branch")) {
    | Some(branch) => cursor_in_branch_pattern(pos, branch)
    | None => false
    }
  };

let rec tree_contains_match_body_cursor =
        (tree: parse_tree, pos: position, node: Tree.node) => {
  let here =
    switch (Tree.node_kind(node)) {
    | "match_body" => cursor_in_match_body_braces(pos, node)
    | "match_branch" => cursor_in_branch_pattern(pos, node)
    | "match_expression" =>
      switch (Tree.node_child_by_field_name(node, "body")) {
      | Some(body) => cursor_in_match_body_braces(pos, body)
      | None => false
      }
    | _ => false
    };
  if (here) {
    true;
  } else {
    let count = Tree.node_named_child_count(node);
    let rec loop = idx =>
      if (idx >= count) {
        false;
      } else {
        switch (Tree.node_named_child(node, idx)) {
        | None => loop(idx + 1)
        | Some(child) =>
          if (tree_contains_match_body_cursor(tree, pos, child)) {
            true;
          } else {
            loop(idx + 1);
          }
        };
      };
    loop(0);
  };
};
