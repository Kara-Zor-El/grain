type context_kind =
  | InScope
  | MemberAccess(string)
  | ImportPath
  | KeywordContext
  | PatternContext;

type position = {
  line: int,
  character: int,
};

type range = {
  range_start: position,
  range_end: position,
};

type context = {
  kind: context_kind,
  prefix: string,
  replace_range: range,
};

type local_binding = {name: string};

module type S = {
  type parser;
  type tree;
  type node;

  module Parser: {
    type t = parser;
    let create: unit => t;
    let parse_string: (~old: tree=?, t, string) => tree;
  };

  module Tree: {
    type t = tree;
    let root_node: t => node;
    let copy: t => t;
    let edit: (t, Edit.t) => unit;
  };

  module Node: {
    type t = node;
    let kind: t => string;
    let start_byte: t => int;
    let end_byte: t => int;
    let start_point: t => (int, int);
    let end_point: t => (int, int);
    let parent: t => option(t);
    let named_child: (t, int) => option(t);
    let named_child_count: t => int;
    let child_by_field_name: (t, string) => option(t);
    let named_descendant_for_point: (t, ~row: int, ~column: int) => option(t);
  };
};

let byte_offset = (source, pos: position) => {
  let rec line_offset = (idx, lines, acc) =>
    switch (lines) {
    | [] => acc
    | [line, ...rest] =>
      if (idx == pos.line) {
        acc
        + Edit.clamp(
            ~min_value=0,
            ~max_value=String.length(line),
            pos.character,
          );
      } else {
        line_offset(idx + 1, rest, acc + String.length(line) + 1);
      }
    };
  line_offset(0, String.split_on_char('\n', source), 0);
};

let replace_range = (pos: position, start_character, end_character): range => {
  {
    range_start: {
      line: pos.line,
      character: start_character,
    },
    range_end: {
      line: pos.line,
      character: end_character,
    },
  };
};

let line_at = (source, pos: position) => {
  switch (String.split_on_char('\n', source)) {
  | [] => ""
  | lines =>
    let rec at = (idx, lines) =>
      switch (lines) {
      | [] => ""
      | [line, ...rest] => idx == pos.line ? line : at(idx + 1, rest)
      };
    at(0, lines);
  };
};

let prefix_from_cursor = (source, pos: position) => {
  let line = line_at(source, pos);
  let character =
    Edit.clamp(~min_value=0, ~max_value=String.length(line), pos.character);
  let is_ident_char = c =>
    c == '_'
    || c >= 'a'
    && c <= 'z'
    || c >= 'A'
    && c <= 'Z'
    || c >= '0'
    && c <= '9';
  let rec find_start = idx =>
    if (idx <= 0) {
      0;
    } else if (is_ident_char(line.[idx - 1])) {
      find_start(idx - 1);
    } else {
      idx;
    };
  let start = find_start(character);
  (String.sub(line, start, character - start), start, character);
};

let member_access_qualifier_from_line = (source, pos: position) => {
  let line = line_at(source, pos);
  let character =
    Edit.clamp(~min_value=0, ~max_value=String.length(line), pos.character);
  let before_cursor = String.sub(line, 0, character);
  switch (String.rindex_opt(before_cursor, '.')) {
  | None => None
  | Some(dot_index) =>
    let qualifier = String.sub(before_cursor, 0, dot_index);
    if (String.length(qualifier) == 0
        || !(qualifier.[0] >= 'A' && qualifier.[0] <= 'Z')) {
      None;
    } else {
      Some(qualifier);
    };
  };
};

module Make = (Backend: S) => {
  module Parser = Backend.Parser;
  module Tree = Backend.Tree;
  module Node = Backend.Node;

  type parse_tree = {
    source: string,
    tree: Tree.t,
  };

  let parser = ref(None);

  let get_parser = () => {
    switch (parser^) {
    | Some(p) => p
    | None =>
      let p = Parser.create();
      parser := Some(p);
      p;
    };
  };

  let parse = (source: string): parse_tree => {
    let tree = Parser.parse_string(get_parser(), source);
    {
      source,
      tree,
    };
  };

  let reparse = (~old: parse_tree, source: string): parse_tree =>
    if (old.source == source) {
      old;
    } else {
      switch (Edit.compute(old.source, source)) {
      | None => parse(source)
      | Some(edit) =>
        let tree = Tree.copy(old.tree);
        Tree.edit(tree, edit);
        let tree = Parser.parse_string(~old=tree, get_parser(), source);
        {
          source,
          tree,
        };
      };
    };

  let cached_tree = (~parse_trees, key, source: string): parse_tree => {
    let version = Hashtbl.hash(source);
    switch (Hashtbl.find_opt(parse_trees, key)) {
    | Some((cached_version, tree)) when cached_version == version => tree
    | Some((_, old)) =>
      let tree = reparse(~old, source);
      Hashtbl.replace(parse_trees, key, (version, tree));
      tree;
    | None =>
      let tree = parse(source);
      Hashtbl.replace(parse_trees, key, (version, tree));
      tree;
    };
  };

  let node_text = (source, node: Node.t) => {
    let start = Node.start_byte(node);
    let end_ = Node.end_byte(node);
    if (end_ <= start || end_ > String.length(source)) {
      "";
    } else {
      String.sub(source, start, end_ - start);
    };
  };

  let node_at_point = (source, tree, pos: position) => {
    let root = Tree.root_node(tree);
    let line = line_at(source, pos);
    let column =
      Edit.clamp(
        ~min_value=0,
        ~max_value=max(0, String.length(line) - 1),
        pos.character,
      );
    Node.named_descendant_for_point(root, ~row=pos.line, ~column);
  };

  let rec ancestor = (node, pred) =>
    switch (Node.parent(node)) {
    | None => None
    | Some(parent) =>
      if (pred(parent)) {
        Some(parent);
      } else {
        ancestor(parent, pred);
      }
    };

  let cursor_before = (pos: position, row, column) =>
    pos.line < row || pos.line == row && pos.character < column;

  let cursor_after = (pos: position, row, column) =>
    pos.line > row || pos.line == row && pos.character > column;

  let cursor_in_match_body_braces = (pos: position, node: Node.t) =>
    if (Node.kind(node) != "match_body") {
      false;
    } else {
      let (start_row, start_column) = Node.start_point(node);
      let (end_row, end_column) = Node.end_point(node);
      !cursor_before(pos, start_row, start_column)
      && !cursor_after(pos, end_row, end_column);
    };

  let cursor_in_branch_pattern = (pos: position, node: Node.t) =>
    switch (Node.kind(node)) {
    | "match_branch" =>
      switch (Node.child_by_field_name(node, "pattern")) {
      | None => false
      | Some(pattern) =>
        let (start_row, start_column) = Node.start_point(pattern);
        let (end_row, end_column) = Node.end_point(pattern);
        !cursor_before(pos, start_row, start_column)
        && !cursor_after(pos, end_row, end_column);
      }
    | _ => false
    };

  let in_match_body_from_node = (pos: position, node: Node.t) =>
    switch (ancestor(node, n => Node.kind(n) == "match_body")) {
    | Some(_) => true
    | None =>
      switch (ancestor(node, n => Node.kind(n) == "match_branch")) {
      | Some(branch) => cursor_in_branch_pattern(pos, branch)
      | None => false
      }
    };

  let rec tree_contains_match_body_cursor = (pos: position, node: Node.t) => {
    let here =
      switch (Node.kind(node)) {
      | "match_body" => cursor_in_match_body_braces(pos, node)
      | "match_branch" => cursor_in_branch_pattern(pos, node)
      | "match_expression" =>
        switch (Node.child_by_field_name(node, "body")) {
        | Some(body) => cursor_in_match_body_braces(pos, body)
        | None => false
        }
      | _ => false
      };
    if (here) {
      true;
    } else {
      let count = Node.named_child_count(node);
      let rec loop = idx =>
        if (idx >= count) {
          false;
        } else {
          switch (Node.named_child(node, idx)) {
          | None => loop(idx + 1)
          | Some(child) =>
            if (tree_contains_match_body_cursor(pos, child)) {
              true;
            } else {
              loop(idx + 1);
            }
          };
        };
      loop(0);
    };
  };

  let member_access_qualifier = (source, node: Node.t, cursor_byte: int) => {
    let text = node_text(source, node);
    if (!String.contains(text, '.')) {
      None;
    } else {
      let node_start = Node.start_byte(node);
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

  let rec detect_kind = (pos: position, source, cursor_byte, node: Node.t) => {
    let kind = Node.kind(node);
    switch (kind) {
    | "use_expression"
    | "include_declaration" => ImportPath
    | "match_branch"
    | "match_body"
    | "match_expression" => PatternContext
    | "qualified_identifier" =>
      switch (member_access_qualifier(source, node, cursor_byte)) {
      | Some(qualifier) => MemberAccess(qualifier)
      | None =>
        switch (Node.parent(node)) {
        | None => InScope
        | Some(parent) => detect_kind(pos, source, cursor_byte, parent)
        }
      }
    | "identifier"
    | "upper_identifier" =>
      switch (ancestor(node, n => Node.kind(n) == "qualified_identifier")) {
      | Some(qualified) =>
        switch (member_access_qualifier(source, qualified, cursor_byte)) {
        | Some(qualifier) => MemberAccess(qualifier)
        | None => detect_kind(pos, source, cursor_byte, qualified)
        }
      | None =>
        switch (ancestor(node, n => Node.kind(n) == "use_expression")) {
        | Some(_) => ImportPath
        | None => InScope
        }
      }
    | _ =>
      switch (ancestor(node, n => Node.kind(n) == "use_expression")) {
      | Some(_) => ImportPath
      | None =>
        switch (ancestor(node, n => Node.kind(n) == "include_declaration")) {
        | Some(_) => ImportPath
        | None =>
          switch (ancestor(node, n => Node.kind(n) == "match_branch")) {
          | Some(_) => PatternContext
          | None =>
            if (in_match_body_from_node(pos, node)) {
              PatternContext;
            } else {
              InScope;
            }
          }
        }
      }
    };
  };

  let context_at = ({source, tree}: parse_tree, pos: position) => {
    let (prefix, prefix_start, prefix_end) = prefix_from_cursor(source, pos);
    let edit_range = replace_range(pos, prefix_start, prefix_end);
    let cursor_byte = byte_offset(source, pos);
    let in_body =
      switch (node_at_point(source, tree, pos)) {
      | None => tree_contains_match_body_cursor(pos, Tree.root_node(tree))
      | Some(node) =>
        if (in_match_body_from_node(pos, node)) {
          true;
        } else {
          tree_contains_match_body_cursor(pos, Tree.root_node(tree));
        }
      };
    let kind =
      switch (node_at_point(source, tree, pos)) {
      | None => if (in_body) {PatternContext} else {InScope}
      | Some(node) =>
        let kind = detect_kind(pos, source, cursor_byte, node);
        if (kind == InScope && in_body) {
          PatternContext;
        } else {
          kind;
        };
      };
    let kind =
      switch (kind) {
      | InScope =>
        switch (member_access_qualifier_from_line(source, pos)) {
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

  let pattern_name = (source, node: Node.t) =>
    switch (Node.kind(node)) {
    | "variable_pattern" => Some(node_text(source, node))
    | "identifier" => Some(node_text(source, node))
    | "upper_identifier" => Some(node_text(source, node))
    | _ => None
    };

  let binding_from_value_binding = (source, node: Node.t) =>
    switch (Node.child_by_field_name(node, "pattern")) {
    | None => None
    | Some(pattern) => pattern_name(source, pattern)
    };

  let binding_from_module = (source, node: Node.t) =>
    switch (Node.child_by_field_name(node, "name")) {
    | None => None
    | Some(name_node) => Some(node_text(source, name_node))
    };

  let local_bindings_before = ({source, tree}: parse_tree, pos: position) => {
    let cursor_byte = byte_offset(source, pos);
    let root = Tree.root_node(tree);
    let rec collect_bindings_before = (source, node: Node.t, cursor_byte, acc) => {
      let acc =
        if (Node.start_byte(node) >= cursor_byte) {
          acc;
        } else {
          switch (Node.kind(node)) {
          | "value_binding" =>
            switch (binding_from_value_binding(source, node)) {
            | Some(name) => [{name: name}, ...acc]
            | None => acc
            }
          | "module_declaration"
          | "module_header" =>
            switch (binding_from_module(source, node)) {
            | Some(name) => [{name: name}, ...acc]
            | None => acc
            }
          | _ => acc
          };
        };
      let count = Node.named_child_count(node);
      let rec loop = (idx, acc) =>
        if (idx >= count) {
          acc;
        } else {
          switch (Node.named_child(node, idx)) {
          | None => loop(idx + 1, acc)
          | Some(child) =>
            loop(
              idx + 1,
              collect_bindings_before(source, child, cursor_byte, acc),
            )
          };
        };
      loop(0, acc);
    };
    collect_bindings_before(source, root, cursor_byte, []);
  };
};
