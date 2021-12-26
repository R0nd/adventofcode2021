open Lib.Util;
open List;

let str_split = Str.split(Str.regexp(" -> "));

let rec string_to_list =
  fun
  | "" => []
  | source => [
      source.[0],
      ...source
         |> String.length
         |> pred
         |> String.sub(source, 1)
         |> string_to_list,
    ];

let make_rule =
  fun
  | [pair, insertion] => (pair |> string_to_list, insertion.[0])
  | _ => raise(Failure("Invalid rule parts"));

let parse_line = line => line |> str_split |> make_rule;

let rec group = (acc, ns) =>
  switch (ns) {
  | [] => acc
  | [n, ...rest] when acc |> mem_assoc(n) =>
    rest
    |> group([(n, acc |> assoc(n) |> succ), ...acc |> remove_assoc(n)])
  | [n, ...rest] => rest |> group([(n, 1), ...acc])
  };

let rec merge = (ns, ms) =>
  switch (ms) {
  | [m, ...mrest] when ns |> mem_assoc(m |> fst) =>
    mrest
    |> merge([
         (m |> fst, (m |> snd) + (ns |> assoc(m |> fst))),
         ...ns |> remove_assoc(m |> fst),
       ])
  | [m, ...mrest] => mrest |> merge([m, ...ns])
  | [] => ns
  };

let insert = (rules, pair) =>
  switch (rules) {
  | _ when rules |> mem_assoc(pair |> fst) =>
    let i = rules |> assoc(pair |> fst);
    let count = pair |> snd;
    [[pair |> fst |> hd, i], [i, pair |> fst |> tl |> hd]]
    |> map(p => (p, count));
  | _ => [pair]
  };

let score =
  fun
  | [n, ...rest] => (rest |> rev |> hd) - n
  | _ => raise(Failure("Invalid element list"));

let file_stream = "14.txt" |> read_file;

let template = file_stream |> Stream.next |> string_to_list;
file_stream |> Stream.next;
let rules = file_stream |> stream_map(parse_line);

let template_pairs = template |> tl |> combine(template |> rev |> tl |> rev);

let pairs_to_groups = pairs =>
  pairs
  |> fold_left(
       (acc, (pair: list(char), n: int)) =>
         [(pair |> hd, n)] |> merge(acc),
       [(template |> rev |> hd, 1)],
     );

[10, 40]
|> map(i =>
     init(i, _ => ())
     |> fold_left(
          (groups, _) =>
            groups
            |> fold_left(
                 (acc, pair) => insert(rules, pair) |> merge(acc),
                 [],
               ),
          template_pairs |> map(((a, b)) => [a, b]) |> group([]),
        )
     |> pairs_to_groups
     |> split
     |> snd
     |> sort((-))
     |> score
     |> print_int
     |> print_newline
   );