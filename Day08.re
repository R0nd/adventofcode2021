open Lib.Util;
open List;

let split = delim => Str.split(Str.regexp(delim));

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

module CharSet =
  Set.Make({
    let compare = Char.compare;
    type t = char;
  });

let list_to_tuple =
  fun
  | [a, b] => (a, b)
  | _ => raise(Failure("Invalid source list"));

let parse_line = line =>
  line
  |> split(" | ")
  |> map(part =>
       part
       |> split(" ")
       |> map(segment => segment |> string_to_list |> CharSet.of_list)
     )
  |> list_to_tuple;

let length_predicate = (len, ns) => ns |> CharSet.elements |> length == len;

let segment_to_digit = (segments, segment) =>
  switch (segment |> CharSet.elements |> length) {
  | 2 => 1
  | 3 => 7
  | 4 => 4
  | 5 =>
    switch (segment, segments) {
    | _
        when
          segment |> CharSet.subset(segments |> find(length_predicate(2))) => 3
    | _
        when
          segment
          |> CharSet.inter(segments |> find(length_predicate(4)))
          |> length_predicate(3) => 5
    | _ => 2
    }
  | 6 =>
    switch (segment, segments) {
    | _
        when
          segment |> CharSet.subset(segments |> find(length_predicate(4))) => 9
    | _
        when
          segment |> CharSet.subset(segments |> find(length_predicate(2))) => 0
    | _ => 6
    }
  | 7 => 8
  | _ => raise(Failure("Invalid segment"))
  };

let input = "8.txt" |> read_file |> stream_map(parse_line);

let solve = ((segments, output)) =>
  output |> map(segment_to_digit(segments));

let outputs = input |> map(solve);

outputs
|> flatten
|> filter(i => [1, 4, 7, 8] |> mem(i))
|> length
|> print_int
|> print_newline;

let rec list_to_int =
  fun
  | [] => 0
  | [n, ...rest] => n + 10 * (rest |> list_to_int);

outputs
|> map(rev)
|> map(list_to_int)
|> fold_left((+), 0)
|> print_int
|> print_newline;