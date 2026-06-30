// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = Protocol.text_document_position_params;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem
module ResponseResult = {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
  [@deriving (enum, yojson)]
  type completion_item_kind =
    | [@value 1] Text
    | [@value 2] Method
    | [@value 3] Function
    | [@value 4] Constructor
    | [@value 5] Field
    | [@value 6] Variable
    | [@value 7] Class
    | [@value 8] Interface
    | [@value 9] Module
    | [@value 10] Property
    | [@value 11] Unit
    | [@value 12] Value
    | [@value 13] Enum
    | [@value 14] Keyword
    | [@value 15] Snippet
    | [@value 16] Color
    | [@value 17] File
    | [@value 18] Reference
    | [@value 19] Folder
    | [@value 20] EnumMember
    | [@value 21] Constant
    | [@value 22] Struct
    | [@value 23] Event
    | [@value 24] Operator
    | [@value 25] TypeParameter;
  let completion_item_kind_to_yojson = item_kind =>
    completion_item_kind_to_enum(item_kind) |> [%to_yojson: int];
  let completion_item_kind_of_yojson = json =>
    Result.bind(json |> [%of_yojson: int], value => {
      switch (completion_item_kind_of_enum(value)) {
      | Some(item_kind) => Ok(item_kind)
      | None => Result.Error("Invalid enum value")
      }
    });

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#insertTextFormat
  [@deriving (enum, yojson)]
  type insert_text_format =
    | [@value 1] PlainText
    | [@value 2] SnippetFormat;
  let insert_text_format_to_yojson = insert_text_format =>
    insert_text_format_to_enum(insert_text_format) |> [%to_yojson: int];
  let insert_text_format_of_yojson = json =>
    Result.bind(json |> [%of_yojson: int], value => {
      switch (insert_text_format_of_enum(value)) {
      | Some(insert_text_format) => Ok(insert_text_format)
      | None => Result.Error("Invalid enum value")
      }
    });

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemTag
  [@deriving (enum, yojson)]
  type completion_item_tag =
    | [@value 1] Deprecated;
  let completion_item_tag_to_yojson = tag =>
    completion_item_tag_to_enum(tag) |> [%to_yojson: int];
  let completion_item_tag_of_yojson = json =>
    Result.bind(json |> [%of_yojson: int], value => {
      switch (completion_item_tag_of_enum(value)) {
      | Some(tag) => Ok(tag)
      | None => Result.Error("Invalid enum value")
      }
    });

  [@deriving yojson({strict: false})]
  type item = {
    label: string,
    kind: completion_item_kind,
    [@default None]
    detail: option(string),
    [@key "sortText"]
    sort_text: string,
    [@default None] [@key "filterText"]
    filter_text: option(string),
    [@default None] [@key "insertText"]
    insert_text: option(string),
    [@default None] [@key "insertTextFormat"]
    insert_text_format: option(insert_text_format),
    [@default None] [@key "textEdit"]
    text_edit: option(Protocol.text_edit),
    [@default None]
    tags: option(list(completion_item_tag)),
  };

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
  [@deriving yojson({strict: false})]
  type t = {
    [@key "isIncomplete"]
    is_incomplete: bool,
    items: list(item),
  };
};

type source =
  | TreeSitter
  | Filesystem
  | Typedtree
  | KeywordSource
  | DocblockAttributeSource;

// NOTE: these rankings are for deduplication purposes only
let source_rank =
  fun
  // Since compiler results are more authoritative, they should be highest
  | Typedtree => 0
  // Docblocks are domain specific and are not guesses so also share this priority
  | DocblockAttributeSource => 0
  // Tree sitter results are primarily to attempt to complete incomplete or missing information and carries less metadata
  | TreeSitter => 1
  // Filesystem results come from directory listings rather than the parse tree
  | Filesystem => 1
  // Keywords are language tokens so shouldn't collide with other sources as they don't share the same kind
  | KeywordSource => 2;

// Completion item ordering for LSP sortText and sorting. Lower ranks appear first.
module Sort_group = {
  type t =
    | Typed
    | Import
    | ModuleMember
    | Operator
    | Local
    | Keyword;

  let ordered: list(t) = [
    Typed,
    Import,
    ModuleMember,
    Operator,
    Local,
    Keyword,
  ];

  let rank = group =>
    switch (List.find_index((==)(group), ordered)) {
    | Some(index) => index
    | None => Int.max_int
    };

  let rank_width =
    max(
      2,
      String.length(string_of_int(max(0, List.length(ordered) - 1))),
    );

  let to_sort_text = (~group: t, label: string) =>
    Printf.sprintf("%0*d_%s", rank_width, rank(group), label);
};

type candidate = {
  item: ResponseResult.item,
  source,
  sort_group: Sort_group.t,
};
