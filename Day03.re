open Lib.Util;

let string_tail = source =>
  String.sub(source, 1, (source |> String.length) - 1);

let rec string_to_charlist = source =>
  switch (source) {
  | "" => []
  | _ => [source.[0], ...source |> string_tail |> string_to_charlist]
  };

let parse_digit = digit =>
  switch (digit) {
  | '0' => 0
  | '1' => 1
  | _ => raise(Failure("Unexpected digit"))
  };

let parse = source => source |> string_to_charlist |> List.map(parse_digit);

let rec internal_bits_to_int = bits =>
  switch (bits) {
  | [] => 0
  | [bit, ...rest] => bit + 2 * (rest |> internal_bits_to_int)
  };

let bits_to_int = bits => bits |> List.rev |> internal_bits_to_int;

let flip = bit =>
  switch (bit) {
  | 0 => 1
  | 1 => 0
  | _ => raise(Failure("Unexpected value"))
  };

let rec fill = (length, value) =>
  switch (length) {
  | 0 => []
  | _ => [value, ...fill(length |> pred, value)]
  };

let input = "3.txt" |> read_file |> stream_map(parse);

let most_common = ns =>
  ns
  |> List.fold_left(
       (acc, row) => List.map2((sum, cell) => sum + cell, acc, row),
       0 |> fill(12),
     )
  |> List.map(cell => cell >= (ns |> List.length) / 2 ? 1 : 0);

let gamma_bits = input |> most_common;
let epsilon_bits = gamma_bits |> List.map(flip);

let gamma = gamma_bits |> bits_to_int;
let epsilon = epsilon_bits |> bits_to_int;

gamma * epsilon |> print_int |> print_newline;

let rec criteriarize = (f, depth, ns) =>
  switch (ns) {
  | [n] => n
  | _ =>
    ns
    |> List.filter(n =>
         depth |> List.nth(n) == (depth |> List.nth(ns |> most_common |> f))
       )
    |> criteriarize(f, depth |> succ)
  };

let oxygen = input |> criteriarize(x => x, 0) |> bits_to_int;
let co2 = input |> criteriarize(List.map(flip), 0) |> bits_to_int;

oxygen * co2 |> print_int |> print_newline;