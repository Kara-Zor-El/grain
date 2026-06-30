open Types;

let cursor_in_doc_comment = (tree: parse_tree, pos: position) => {
  switch (Tree.node_descendant_for_point(tree, pos)) {
  | None => false
  | Some(node) =>
    Tree.node_kind(node) == "doc_comment"
    || (
      switch (Tree.ancestor(node, n => Tree.node_kind(n) == "doc_comment")) {
      | Some(_) => true
      | None => false
      }
    )
  };
};
