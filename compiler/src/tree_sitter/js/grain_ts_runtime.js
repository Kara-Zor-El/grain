//Provides: grain_ts_on_ready
function grain_ts_on_ready(callback) {
  if (!globalThis.__grainTreeSitterInitPromise) {
    globalThis.__grainTreeSitterInitPromise = (async function () {
      if (globalThis.__grainTreeSitterRuntime) {
        return;
      }
      var fs = require("fs");
      var path = require("path");
      var TreeSitter = require("web-tree-sitter");

      await TreeSitter.Parser.init();
      var wasmPath = path.join(__dirname, "tree-sitter-grain.wasm");
      var wasm = fs.readFileSync(wasmPath);
      var language = await TreeSitter.Language.load(wasm);
      globalThis.__grainTreeSitterRuntime = {
        Parser: TreeSitter.Parser,
        language: language,
      };
    })();
  }
  globalThis.__grainTreeSitterInitPromise
    .then(function () {
      callback();
    })
    .catch(function (err) {
      var message = err && err.stack ? err.stack : String(err);
      if (typeof console !== "undefined" && console.error) {
        console.error(message);
      }
      if (typeof process !== "undefined" && process.exit) {
        process.exit(1);
      }
    });
}

//Provides: grain_ts_parser_create
function grain_ts_parser_create() {
  var runtime = globalThis.__grainTreeSitterRuntime;
  if (!runtime) {
    throw new Error(
      "Tree-sitter is not initialized yet. The grammar WASM is still loading.",
    );
  }
  var parser = new runtime.Parser();
  parser.setLanguage(runtime.language);
  return parser;
}

//Provides: grain_ts_parser_parse
function grain_ts_parser_parse(parser, source) {
  if (typeof source !== "string" && source && typeof source.toUtf16 === "function") {
    source = source.toUtf16();
  }
  return parser.parse(source);
}

//Provides: grain_ts_parser_parse_incremental
function grain_ts_parser_parse_incremental(parser, source, oldTree) {
  if (typeof source !== "string" && source && typeof source.toUtf16 === "function") {
    source = source.toUtf16();
  }
  return parser.parse(source, oldTree);
}

//Provides: grain_ts_tree_copy
function grain_ts_tree_copy(tree) {
  if (tree == null || tree === 0) {
    return 0;
  }
  return tree.copy();
}

//Provides: grain_ts_tree_edit
function grain_ts_tree_edit(
  tree,
  start_byte,
  old_end_byte,
  new_end_byte,
  start_row,
  start_column,
  old_end_row,
  old_end_column,
  new_end_row,
  new_end_column,
) {
  if (tree == null || tree === 0) {
    return;
  }
  tree.edit({
    startIndex: start_byte,
    oldEndIndex: old_end_byte,
    newEndIndex: new_end_byte,
    startPosition: { row: start_row, column: start_column },
    oldEndPosition: { row: old_end_row, column: old_end_column },
    newEndPosition: { row: new_end_row, column: new_end_column },
  });
}

//Provides: grain_ts_node_kind
function grain_ts_node_kind(node) {
  if (node == null || node === 0) {
    return "";
  }
  return node.type;
}

//Provides: grain_ts_node_start_byte
function grain_ts_node_start_byte(node) {
  if (node == null || node === 0) {
    return 0;
  }
  return node.startIndex;
}

//Provides: grain_ts_node_end_byte
function grain_ts_node_end_byte(node) {
  if (node == null || node === 0) {
    return 0;
  }
  return node.endIndex;
}

//Provides: grain_ts_node_start_point
function grain_ts_node_start_point(node) {
  if (node == null || node === 0) {
    return [0, 0];
  }
  var pos = node.startPosition;
  return [pos.row, pos.column];
}

//Provides: grain_ts_node_end_point
function grain_ts_node_end_point(node) {
  if (node == null || node === 0) {
    return [0, 0];
  }
  var pos = node.endPosition;
  return [pos.row, pos.column];
}

//Provides: grain_ts_node_parent
function grain_ts_node_parent(node) {
  if (node == null || node === 0) {
    return 0;
  }
  var parent = node.parent;
  return parent == null ? 0 : parent;
}

//Provides: grain_ts_node_named_child
function grain_ts_node_named_child(node, index) {
  if (node == null || node === 0) {
    return 0;
  }
  var child = node.namedChild(index);
  return child == null ? 0 : child;
}

//Provides: grain_ts_node_named_child_count
function grain_ts_node_named_child_count(node) {
  if (node == null || node === 0) {
    return 0;
  }
  return node.namedChildCount;
}

//Provides: grain_ts_node_child_by_field_name
function grain_ts_node_child_by_field_name(node, field) {
  if (node == null || node === 0) {
    return 0;
  }
  if (typeof field !== "string" && field && typeof field.toUtf16 === "function") {
    field = field.toUtf16();
  }
  var child = node.childForFieldName(field);
  return child == null ? 0 : child;
}

//Provides: grain_ts_tree_root_node
function grain_ts_tree_root_node(tree) {
  if (tree == null || tree === 0) {
    return 0;
  }
  return tree.rootNode;
}

//Provides: grain_ts_node_named_descendant_for_point
function grain_ts_node_named_descendant_for_point(
  node,
  start_row,
  start_column,
  end_row,
  end_column,
) {
  if (node == null || node === 0) {
    return 0;
  }
  var start = { row: start_row, column: start_column };
  var end = { row: end_row, column: end_column };
  var descendant = node.namedDescendantForPosition(start, end);
  return descendant == null ? 0 : descendant;
}

//Provides: grain_ts_node_descendant_for_point
function grain_ts_node_descendant_for_point(
  node,
  start_row,
  start_column,
  end_row,
  end_column,
) {
  if (node == null || node === 0) {
    return 0;
  }
  var start = { row: start_row, column: start_column };
  var end = { row: end_row, column: end_column };
  var descendant = node.descendantForPosition(start, end);
  return descendant == null ? 0 : descendant;
}
