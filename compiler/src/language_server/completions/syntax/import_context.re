open Types;

let rec import_context_node = (tree: parse_tree, pos: position) => {
  switch (Tree.node_at_point(tree, pos)) {
  | Some(node) =>
    switch (Tree.node_kind(node)) {
    | "program" when pos.character > 0 =>
      import_context_node(
        tree,
        {
          ...pos,
          character: pos.character - 1,
        },
      )
    | _ => Some(node)
    }
  | None =>
    if (pos.character > 0) {
      import_context_node(
        tree,
        {
          ...pos,
          character: pos.character - 1,
        },
      );
    } else {
      None;
    }
  };
};

let import_module_context = (~tree, ~source, ~pos, path_node) => {
  let text = Tree.node_text(tree, path_node);
  let len = String.length(text);
  let import_path =
    if (len >= 2 && text.[0] == '"' && text.[len - 1] == '"') {
      String.sub(text, 1, len - 2);
    } else if (len >= 1 && text.[0] == '"') {
      String.sub(text, 1, len - 1);
    } else {
      "";
    };
  let (prefix, prefix_start, prefix_end) =
    Text.prefix_from_cursor(source, pos);
  let (prefix, prefix_start, prefix_end) =
    prefix == "include"
      ? ("", prefix_end, prefix_end) : (prefix, prefix_start, prefix_end);
  Some((ImportModuleName(import_path), prefix, prefix_start, prefix_end));
};

let import_context_from_tree = (tree: parse_tree, pos: position) => {
  let source = Tree.source(tree);
  switch (import_context_node(tree, pos)) {
  | None => None
  | Some(node) =>
    let include_decl =
      if (Tree.node_kind(node) == "include_declaration") {
        Some(node);
      } else {
        Tree.ancestor(node, n => Tree.node_kind(n) == "include_declaration");
      };
    switch (include_decl) {
    | None => None
    | Some(include_decl) =>
      let path_node = Tree.node_child_by_field_name(include_decl, "path");
      let module_node = Tree.node_child_by_field_name(include_decl, "module");
      let in_path =
        switch (path_node) {
        | Some(path_node) => Tree.cursor_in_node(pos, node, path_node)
        | None => false
        };
      if (in_path) {
        switch (path_node) {
        | Some(path_node) =>
          let line = Text.line_at(source, pos);
          let character =
            Edit.clamp(
              ~min_value=0,
              ~max_value=String.length(line),
              pos.character,
            );
          let (start_row, start_column) = Tree.node_start_point(path_node);
          if (pos.line != start_row) {
            None;
          } else {
            let prefix_start = start_column + 1;
            if (character < prefix_start) {
              None;
            } else {
              Some((
                ImportFilePath,
                String.sub(line, prefix_start, character - prefix_start),
                prefix_start,
                character,
              ));
            };
          };
        | None => None
        };
      } else {
        switch (module_node, path_node) {
        | (Some(_), Some(path_node)) =>
          import_module_context(~tree, ~source, ~pos, path_node)
        | (None, Some(path_node))
            when
              Tree.node_end_byte(include_decl)
              > Tree.node_end_byte(path_node) =>
          import_module_context(~tree, ~source, ~pos, path_node)
        | _ => None
        };
      };
    };
  };
};
