open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_tests_utils.Lsp_test_utils;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

let string_contains = (haystack, needle) =>
  try(
    {
      ignore(Str.search_forward(Str.regexp_string(needle), haystack, 0));
      true;
    }
  ) {
  | Not_found => false
  };

describe("grainlsp_completions", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  test_or_skip("completion_local_value", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let apple = 1
let banana = app
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"apple"|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_docblock_attribute", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
/**
 * Retrieves an element.
 * @pa
 */
let get = (index, array) => array[index]
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 5),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"@param"|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_optional_argument", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = (required, optional=1) => required + optional
f(1, )
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"optional"|})).toBe(true);
    expect.bool(string_contains(result, {|"newText":"optional="|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_optional_argument_imported", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
print(1, )
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 8),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"suffix"|})).toBe(true);
    expect.bool(string_contains(result, {|"newText":"suffix="|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_relative_path", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./prov
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"./provideAll.gr"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_stdlib_path", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "arr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"array"|})).toBe(true);
    expect.bool(string_contains(result, {|"kind":17|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_module_name", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" include Prov
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 35),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"ProvideAll"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":9|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_import_module_name_empty_prefix", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" include
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 30),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"ProvideAll"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"kind":9|})).toBe(true);
    expect.bool(string_contains(result, {|"isIncomplete":false|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_toplevel_statement", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
pr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"provide"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"primitive"|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_from_snippet", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
fr
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"from"|})).toBe(true);
    expect.bool(string_contains(result, {|"newText":"from \"$0\""|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"insertTextFormat":2|})).toBe(
      true,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_block_statement", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = () => {
  re
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"return"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"record"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_header", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"record"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_provide_start", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide f
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"foreign"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"for"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"(%)"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_flow", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" inc
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 26),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"include"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_after_path", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 22),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"include"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_include_priority", ({expect}) => {
    let code_uri = make_test_utils_uri("a.gr");
    let code = {|module A
from "./provideAll.gr" i
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 24),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"include"|})).toBe(true);
    expect.bool(string_contains(result, {|"sortText":"00_include"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"sortText":"00_identify"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_if_else", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let x = if (true) 1 el
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 22),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"else"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_body_print", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => print("idk"),
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_body_print_partial", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => pr
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 8),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_guard_print_partial", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A, B }
let x = A
match (x) {
  A when pr => true,
  _ => false,
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 4, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_let_binding_name_not_header", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let r
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 5),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_local", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => {
    let r = "idk"
    print(r)
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 6, 10),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"r"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_print", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => {
    let r = "idk"
    print(r)
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 6, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"print"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_let", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
enum X { A }
let x = A
match (x) {
  A => {
    let r = "idk"
    print(r)
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"let"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_match_branch_block_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "string" include String
enum X { A }
let x = A
match (x) {
  A => {
    let z = String.concat("", "")
  },
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 6, 19),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"concat"|})).toBe(true);
    expect.bool(string_contains(result, {|"(%)"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_string_literal_no_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "string" include String
let s = "String.|
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_string_literal_suppressed", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let s = "Failure()"
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 16),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_pattern_no_member_access", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "string" include String
enum X { A }
let x = A
match (x) {
  String.|
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 5, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"concat"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_match_when", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = x => match (x) {
  Some(y) wh
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 12),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"when"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_record_field_mut", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
record R {
  m
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 3),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_enum_body_no_mut", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_enum_variant_type_reference", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,
  Banana(Number),
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip(
    "completion_enum_variant_type_reference_incomplete", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide enum X {
  Apple,
  Banana(
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 9),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"String"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_rec", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let rec
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"from"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_mut", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let mut
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 7),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"from"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_keyword_let_header", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 3),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"mut"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_context_let_rhs", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let mut x =
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 12),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"break"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"continue"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"&&"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"true"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_context_loop_control", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = () => {
  while (true) {

  }
}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 3, 4),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"break"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"continue"|})).toBe(true);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_context_break_outside_loop", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
let f = () => {

}
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 2),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"break"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"continue"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_provide_data_no_repeat_keywords", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
provide type rec
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 1, 18),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"enum"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"abstract"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"type"|})).toBe(false);
    expect.bool(string_contains(result, {|"label":"rec"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_items_plain", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{  }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 11),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"code"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"type Encoding"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"label":"module Ascii"|})).toBe(
      true,
    );
    expect.bool(string_contains(result, {|"label":"type"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_items_type_keyword", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{ type Encoding }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 19),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Encoding"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"type Encoding"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"code"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_items_module_keyword", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{ module Ascii }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 20),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Ascii"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"module Ascii"|})).toBe(
      false,
    );
    expect.bool(string_contains(result, {|"label":"code"|})).toBe(false);
    expect.int(code).toBe(0);
  });

  test_or_skip("completion_use_module_name_before_brackets", ({expect}) => {
    let code_uri = "file:///a.gr";
    let code = {|module A
from "char" include Char
use Char.{  }
|};
    let (setup_request, teardown_request) =
      lsp_setup_teardown_requests(code_uri, code);
    let request =
      lsp_input(
        "textDocument/completion",
        lsp_text_document_position(code_uri, 2, 6),
      );
    let (result, code) =
      lsp(setup_request ++ lsp_request(request) ++ teardown_request);

    expect.bool(string_contains(result, {|"label":"Char"|})).toBe(true);
    expect.bool(string_contains(result, {|"label":"type Encoding"|})).toBe(
      false,
    );
    expect.int(code).toBe(0);
  });
});
