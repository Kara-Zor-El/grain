type point = {
  row: int,
  column: int,
};

type t = {
  start_byte: int,
  old_end_byte: int,
  new_end_byte: int,
  start_point: point,
  old_end_point: point,
  new_end_point: point,
};

let clamp = (~min_value, ~max_value, value) =>
  max(min_value, min(max_value, value));

let byte_to_point = (source, byte_offset) => {
  let offset =
    clamp(~min_value=0, ~max_value=String.length(source), byte_offset);
  let rec scan = (idx, row, col) =>
    if (idx >= offset) {
      {
        row,
        column: col,
      };
    } else if (source.[idx] == '\n') {
      scan(idx + 1, row + 1, 0);
    } else {
      scan(idx + 1, row, col + 1);
    };
  scan(0, 0, 0);
};

let compute = (old_source, new_source) =>
  if (old_source == new_source) {
    None;
  } else {
    let old_len = String.length(old_source);
    let new_len = String.length(new_source);
    if (old_len == 0) {
      Some({
        start_byte: 0,
        old_end_byte: 0,
        new_end_byte: new_len,
        start_point: {
          row: 0,
          column: 0,
        },
        old_end_point: {
          row: 0,
          column: 0,
        },
        new_end_point: byte_to_point(new_source, new_len),
      });
    } else if (new_len == 0) {
      Some({
        start_byte: 0,
        old_end_byte: old_len,
        new_end_byte: 0,
        start_point: {
          row: 0,
          column: 0,
        },
        old_end_point: byte_to_point(old_source, old_len),
        new_end_point: {
          row: 0,
          column: 0,
        },
      });
    } else {
      let rec common_prefix = idx =>
        if (idx >= old_len
            || idx >= new_len
            || old_source.[idx] != new_source.[idx]) {
          idx;
        } else {
          common_prefix(idx + 1);
        };
      let prefix = common_prefix(0);
      let rec common_suffix = (old_idx, new_idx) =>
        if (old_idx < prefix || new_idx < prefix) {
          (old_idx, new_idx);
        } else if (old_source.[old_idx] != new_source.[new_idx]) {
          (old_idx, new_idx);
        } else {
          common_suffix(old_idx - 1, new_idx - 1);
        };
      let (old_suffix, new_suffix) = common_suffix(old_len - 1, new_len - 1);
      let start_byte = prefix;
      let old_end_byte = old_suffix + 1;
      let new_end_byte = new_suffix + 1;
      let start_point = byte_to_point(old_source, start_byte);
      let old_end_point = byte_to_point(old_source, old_end_byte);
      let new_end_point = byte_to_point(new_source, new_end_byte);
      Some({
        start_byte,
        old_end_byte,
        new_end_byte,
        start_point,
        old_end_point,
        new_end_point,
      });
    };
  };
