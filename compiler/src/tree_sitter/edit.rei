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

let clamp: (~min_value: int, ~max_value: int, int) => int;

let compute: (string, string) => option(t);
