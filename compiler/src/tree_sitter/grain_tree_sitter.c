#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

typedef struct TSLanguage TSLanguage;

const TSLanguage *tree_sitter_grain(void);

CAMLprim value caml_grain_tree_sitter_language(value unit) {
  CAMLparam1(unit);
  const TSLanguage *lang = tree_sitter_grain();
  CAMLreturn(caml_copy_nativeint((intnat)lang));
}
