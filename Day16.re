open Lib.Fn;
open Lib.Util;
open List;

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

let hex_to_bin =
  fun
  | '0' => [0, 0, 0, 0]
  | '1' => [0, 0, 0, 1]
  | '2' => [0, 0, 1, 0]
  | '3' => [0, 0, 1, 1]
  | '4' => [0, 1, 0, 0]
  | '5' => [0, 1, 0, 1]
  | '6' => [0, 1, 1, 0]
  | '7' => [0, 1, 1, 1]
  | '8' => [1, 0, 0, 0]
  | '9' => [1, 0, 0, 1]
  | 'A' => [1, 0, 1, 0]
  | 'B' => [1, 0, 1, 1]
  | 'C' => [1, 1, 0, 0]
  | 'D' => [1, 1, 0, 1]
  | 'E' => [1, 1, 1, 0]
  | 'F' => [1, 1, 1, 1]
  | _ => raise(Failure("Invalid hex char"));

let rec bin_to_int_inner =
  fun
  | [] => 0
  | [n] => n
  | [n, ...rest] => n + 2 * (rest |> bin_to_int_inner);

let bin_to_int = x => x |> rev |> bin_to_int_inner;

type packet =
  | Operator(int, int, list(packet))
  | Literal(int, int);

let first = (f, (a, b)) => (a |> f, b);

let rec parse_literal_segments =
  fun
  | [1, b0, b1, b2, b3, ...rest] => {
      rest
      |> parse_literal_segments
      |> first(segments => [b0, b1, b2, b3, ...segments]);
    }
  | [0, b0, b1, b2, b3, ...rest] => ([b0, b1, b2, b3], rest)
  | rest => ([], rest);

let parse_literal = (bits, version) =>
  bits
  |> parse_literal_segments
  |> first(bin_to_int)
  |> first(segments => Literal(version, segments));

let new_operator = (version, packet_type, subpackets) =>
  Operator(version, packet_type, subpackets);

let rec parse_packet = (bits): (packet, list(int)) => {
  let rec subpackets_by_count = (bits, count) =>
    switch (count) {
    | 0 => ([], bits)
    | _ =>
      let (packet, rest) = bits |> parse_packet;
      count
      |> pred
      |> subpackets_by_count(rest)
      |> first(packets => [packet, ...packets]);
    };

  let rec subpackets_by_total = (bits, len) =>
    switch (len) {
    | 0 => ([], bits)
    | _ =>
      let (packet, rest) = bits |> parse_packet;
      len
      - ((bits |> length) - (rest |> length))
      |> subpackets_by_total(rest)
      |> first(packets => [packet, ...packets]);
    };

  let parse_operator = (bits, packet_type, version) =>
    switch (bits) {
    | [0, ...rest] =>
      rest
      |> take(15)
      |> bin_to_int
      |> subpackets_by_total(rest |> skip(15))
      |> first(new_operator(version, packet_type))
    | [1, ...rest] =>
      rest
      |> take(11)
      |> bin_to_int
      |> subpackets_by_count(rest |> skip(11))
      |> first(new_operator(version, packet_type))
    | _ => raise(Failure("Invalid operator format"))
    };

  switch (bits) {
  | [v0, v1, v2, t0, t1, t2, ...rest] =>
    switch ([t0, t1, t2] |> bin_to_int) {
    | 4 => [v0, v1, v2] |> bin_to_int |> parse_literal(rest)
    | pt => [v0, v1, v2] |> bin_to_int |> parse_operator(rest, pt)
    }
  | _ => raise(Failure("Invalid packet format"))
  };
};

let rec score =
  fun
  | Literal(v, _) => v
  | Operator(v, _, subops) => subops |> map(score) |> fold_left((+), v);

let rec calc =
  fun
  | Literal(_, v) => v
  | Operator(_, 0, subops) => subops |> map(calc) |> fold_left((+), 0)
  | Operator(_, 1, subops) => subops |> map(calc) |> fold_left(( * ), 1)
  | Operator(_, 2, subops) => subops |> map(calc) |> fold_left(min, max_int)
  | Operator(_, 3, subops) => subops |> map(calc) |> fold_left(max, 0)
  | Operator(_, 5, subops) =>
    subops
    |> map(calc)
    |> (
      fun
      | [a, b] when a > b => 1
      | _ => 0
    )
  | Operator(_, 6, subops) =>
    subops
    |> map(calc)
    |> (
      fun
      | [a, b] when a < b => 1
      | _ => 0
    )
  | Operator(_, 7, subops) =>
    subops
    |> map(calc)
    |> (
      fun
      | [a, b] when a == b => 1
      | _ => 0
    )
  | _ => raise(Failure("Invalid operator type"));

let root_packet =
  "16.txt"
  |> read_file
  |> Stream.next
  |> string_to_list
  |> map(hex_to_bin)
  |> flatten
  |> parse_packet
  |> fst;

root_packet |> score |> print_int |> print_newline;
root_packet |> calc |> print_int |> print_newline;