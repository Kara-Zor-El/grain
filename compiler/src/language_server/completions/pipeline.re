open Completion_types;

let candidate_cap = 100;

let rec insert_dedup_candidate = (candidate, candidates) =>
  switch (candidates) {
  | [] => [candidate]
  | [head, ...rest]
      when
        (candidate.item.label, candidate.item.kind)
        == (head.item.label, head.item.kind) => [
      if (source_rank(candidate.source) < source_rank(head.source)) {
        candidate;
      } else {
        head;
      },
      ...rest,
    ]
  | [head, ...rest] => [head, ...insert_dedup_candidate(candidate, rest)]
  };

let dedupe_candidates = candidates =>
  List.fold_left(
    (acc, candidate) => insert_dedup_candidate(candidate, acc),
    [],
    candidates,
  );

let compare_candidate = (left, right) =>
  switch (
    Int.compare(
      Sort_group.rank(left.sort_group),
      Sort_group.rank(right.sort_group),
    )
  ) {
  | 0 => String.compare(left.item.label, right.item.label)
  | cmp => cmp
  };

let filter_prefix = (~prefix, candidates) =>
  List.filter(
    candidate => {
      let text =
        switch (candidate.item.filter_text) {
        | Some(filter_text) => filter_text
        | None => candidate.item.label
        };
      prefix == "" || String.starts_with(~prefix, text);
    },
    candidates,
  );

let take_candidates = (count, candidates) => {
  let rec loop = (remaining, acc, candidates) =>
    if (remaining <= 0) {
      List.rev(acc);
    } else {
      switch (candidates) {
      | [] => List.rev(acc)
      | [candidate, ...rest] =>
        loop(remaining - 1, [candidate, ...acc], rest)
      };
    };
  loop(count, [], candidates);
};

let finalize_candidates = (~prefix, candidates) => {
  let candidates =
    candidates
    |> dedupe_candidates
    |> filter_prefix(~prefix)
    |> List.sort(compare_candidate);
  let is_incomplete = List.length(candidates) > candidate_cap;
  let items =
    candidates
    |> take_candidates(candidate_cap)
    |> List.map(candidate => candidate.item);
  (is_incomplete, items);
};
