open Types;

let member_access_qualifier =
    (tree: parse_tree, node: Tree.node, cursor_byte: int) => {
  let text = Tree.node_text(tree, node);
  if (!String.contains(text, '.')) {
    None;
  } else {
    let node_start = Tree.node_start_byte(node);
    let cursor_in_node = cursor_byte - node_start;
    if (cursor_in_node < 0 || cursor_in_node > String.length(text)) {
      None;
    } else {
      let before_cursor = String.sub(text, 0, cursor_in_node);
      switch (String.rindex_opt(before_cursor, '.')) {
      | None => None
      | Some(dot_index) =>
        let qualifier = String.sub(text, 0, dot_index);
        qualifier == "" ? None : Some(qualifier);
      };
    };
  };
};

let rec detect_kind =
        (
          tree: parse_tree,
          pos: position,
          source,
          cursor_byte,
          node: Tree.node,
        ) => {
  let kind = Tree.node_kind(node);
  switch (kind) {
  | "use_expression"
  | "include_declaration" => ImportPath
  | "match_branch"
  | "match_body"
  | "match_expression" => PatternContext
  | "qualified_identifier" =>
    switch (member_access_qualifier(tree, node, cursor_byte)) {
    | Some(qualifier) => MemberAccess(qualifier)
    | None =>
      switch (Tree.node_parent(node)) {
      | None => InScope
      | Some(parent) => detect_kind(tree, pos, source, cursor_byte, parent)
      }
    }
  | "identifier"
  | "upper_identifier" =>
    switch (
      Tree.ancestor(node, n => Tree.node_kind(n) == "qualified_identifier")
    ) {
    | Some(qualified) =>
      switch (member_access_qualifier(tree, qualified, cursor_byte)) {
      | Some(qualifier) => MemberAccess(qualifier)
      | None => detect_kind(tree, pos, source, cursor_byte, qualified)
      }
    | None =>
      switch (Tree.ancestor(node, n => Tree.node_kind(n) == "use_expression")) {
      | Some(_) => ImportPath
      | None =>
        if (Calls.in_call_argument_label_position(
              tree,
              pos,
              cursor_byte,
              node,
            )) {
          CallArgument;
        } else {
          InScope;
        }
      }
    }
  | _ =>
    switch (Tree.ancestor(node, n => Tree.node_kind(n) == "use_expression")) {
    | Some(_) => ImportPath
    | None =>
      switch (
        Tree.ancestor(node, n => Tree.node_kind(n) == "include_declaration")
      ) {
      | Some(_) => ImportPath
      | None =>
        switch (Tree.ancestor(node, n => Tree.node_kind(n) == "match_branch")) {
        | Some(_) => PatternContext
        | None =>
          if (Match_context.in_match_body_from_node(pos, node)) {
            PatternContext;
          } else if (Calls.in_call_argument_label_position(
                       tree,
                       pos,
                       cursor_byte,
                       node,
                     )) {
            CallArgument;
          } else {
            InScope;
          }
        }
      }
    }
  };
};
