open Completion_types;
open Completion_types.ResponseResult;

let make_candidate =
    (
      ~source,
      ~label,
      ~kind,
      ~detail=?,
      ~sort_group: Sort_group.t,
      ~context: Syntax.Types.t,
      ~filter_text=?,
      ~insert_text=?,
      ~tags=?,
      (),
    ) => {
  let insert = Option.value(~default=label, insert_text);
  let text_edit = {
    let edit: Protocol.text_edit = {
      range: context.replace_range,
      new_text: insert,
    };
    Some(edit);
  };
  let item = {
    label,
    kind,
    detail,
    sort_text: Sort_group.to_sort_text(~group=sort_group, label),
    filter_text,
    insert_text: None,
    insert_text_format: None,
    text_edit,
    tags,
  };
  {
    item,
    source,
    sort_group,
  };
};
