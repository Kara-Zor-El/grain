external languagePtr: unit => nativeint = "caml_grain_tree_sitter_language";

let language = () => Tree_sitter.Language.of_address(languagePtr());
