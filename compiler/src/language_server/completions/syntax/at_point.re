open Types;

let at = (tree: parse_tree, pos: position) => {
  let source = Tree.source(tree);
  let in_doc_comment = Doc_comment.cursor_in_doc_comment(tree, pos);
  let (doc_prefix, doc_prefix_start, doc_prefix_end) =
    Text.prefix_from_doc_attribute(source, pos);
  let editing_doc_attribute =
    in_doc_comment && String.contains(doc_prefix, '@');
  let import_context =
    editing_doc_attribute
      ? None : Import_context.import_context_from_tree(tree, pos);
  let (prefix, prefix_start, prefix_end) =
    if (editing_doc_attribute) {
      (doc_prefix, doc_prefix_start, doc_prefix_end);
    } else {
      switch (import_context) {
      | Some((_kind, prefix, prefix_start, prefix_end)) => (
          prefix,
          prefix_start,
          prefix_end,
        )
      | None => Text.prefix_from_cursor(source, pos)
      };
    };
  let edit_range = Text.replace_range(pos, prefix_start, prefix_end);
  if (editing_doc_attribute) {
    {
      kind: DocblockContext,
      prefix,
      replace_range: edit_range,
    };
  } else {
    let cursor_byte = Text.byte_offset(source, pos);
    let node = Tree.node_at_point(tree, pos);
    let in_body =
      switch (node) {
      | None =>
        Match_context.tree_contains_match_body_cursor(
          tree,
          pos,
          Tree.root(tree),
        )
      | Some(node) =>
        if (Match_context.in_match_body_from_node(pos, node)) {
          true;
        } else {
          Match_context.tree_contains_match_body_cursor(
            tree,
            pos,
            Tree.root(tree),
          );
        }
      };
    let detected_kind =
      switch (node) {
      | None => if (in_body) {PatternContext} else {InScope}
      | Some(node) =>
        let kind = Detect.detect_kind(tree, pos, source, cursor_byte, node);
        if (kind == InScope && in_body) {
          PatternContext;
        } else {
          kind;
        };
      };
    let kind =
      switch (import_context) {
      | Some((kind, _prefix, _prefix_start, _)) => kind
      | None => detected_kind
      };
    let kind =
      switch (kind) {
      | InScope =>
        switch (Text.member_access_qualifier_from_line(source, pos)) {
        | Some(qualifier) => MemberAccess(qualifier)
        | None => kind
        }
      | _ => kind
      };
    {
      kind,
      prefix,
      replace_range: edit_range,
    };
  };
};
