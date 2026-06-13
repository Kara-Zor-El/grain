module Grammar =
  MenhirSdk.Cmly_read.Read({
    let filename = "parser.cmly";
  });

let is_keyword = terminal =>
  List.exists(
    attr => Grammar.Attribute.label(attr) == "keyword",
    Grammar.Terminal.attributes(terminal),
  );

let keywords =
  Grammar.Terminal.fold(
    (terminal, acc) => {
      let name = Grammar.Terminal.name(terminal) |> String.lowercase_ascii;
      is_keyword(terminal) ? [name, ...acc] : acc;
    },
    [],
  )
  |> List.sort_uniq(compare);

let () = {
  print_string("let all = [");
  List.iter(keyword => print_string("\"" ++ keyword ++ "\", "), keywords);
  print_string("];");
};
