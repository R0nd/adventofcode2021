open Lib.Util;
open List;

type number =
  | Regular(int)
  | Pair(number, number);

let rec string_to_charlist =
  fun
  | "" => []
  | str => [
      str.[0],
      ...String.sub(str, 1, str |> String.length |> pred)
         |> string_to_charlist,
    ];

let skip = (item, list) =>
  switch (list) {
  | [head, ...rest] when head == item => rest
  | _ => list
  };

let rec parse =
  fun
  | ['[', ...rest] => {
      let (a, arest) = rest |> parse;
      let (b, brest) = arest |> parse;
      (Pair(a, b), brest |> skip(']') |> skip(','));
    }
  | [dig, ...rest] when dig |> String.contains("0123456789") => (
      Regular(dig |> String.make(1) |> int_of_string),
      rest |> skip(','),
    )
  | _ => raise(Failure("parse"));

let rec inc_left = (v, number) =>
  switch (number) {
  | Pair(a, b) => Pair(a |> inc_left(v), b)
  | Regular(a) => Regular(a + v)
  };

let rec inc_right = (v, number) =>
  switch (number) {
  | Pair(a, b) => Pair(a, b |> inc_right(v))
  | Regular(a) => Regular(a + v)
  };

let rec explode = (depth, a) =>
  switch (a) {
  | Pair(Regular(aa), Regular(ab)) when depth >= 4 => (
      Regular(0),
      Some((aa, ab)),
    )
  | Pair(Pair(aa, ab), Regular(b)) =>
    Pair(aa, ab)
    |> explode(depth |> succ)
    |> (
      fun
      | (exp, Some((exp_a, exp_b))) => (
          Pair(exp, Regular(b) |> inc_left(exp_b)),
          Some((exp_a, 0)),
        )
      | (_, None) => (a, None)
    )
  | Pair(Regular(aa), Pair(ba, bb)) =>
    Pair(ba, bb)
    |> explode(depth |> succ)
    |> (
      fun
      | (exp, Some((exp_a, exp_b))) => (
          Pair(Regular(aa) |> inc_right(exp_a), exp),
          Some((0, exp_b)),
        )
      | (_, None) => (a, None)
    )
  | Pair(Pair(aa, ab), Pair(ba, bb)) =>
    Pair(aa, ab)
    |> explode(depth |> succ)
    |> (
      fun
      | (exp, Some((exp_a, exp_b))) => (
          Pair(exp, Pair(ba, bb) |> inc_left(exp_b)),
          Some((exp_a, 0)),
        )
      | (_, None) =>
        Pair(ba, bb)
        |> explode(depth |> succ)
        |> (
          fun
          | (exp, Some((exp_a, exp_b))) => (
              Pair(Pair(aa, ab) |> inc_right(exp_a), exp),
              Some((0, exp_b)),
            )
          | (_, None) => (a, None)
        )
    )
  | _ => (a, None)
  };

let rec split = a =>
  switch (a) {
  | Regular(c) when c > 9 => Pair(Regular(c / 2), Regular(c / 2 + c mod 2))
  | Regular(_) => a
  | Pair(aa, ab) =>
    let split_aa = aa |> split;
    split_aa == aa
      ? {
        Pair(aa, ab |> split);
      }
      : Pair(split_aa, ab);
  };

let rec debug_inner =
  fun
  | Regular(c) => c |> print_int
  | Pair(a, b) => {
      '[' |> print_char;
      a |> debug_inner;
      ',' |> print_char;
      b |> debug_inner;
      ']' |> print_char;
    };

let rec reduce = a => {
  let b = a |> explode(0) |> fst;
  b == a
    ? {
      let c = a |> split;
      c == a ? a : c |> reduce;
    }
    : b |> reduce;
};

let add = (a, b) => Pair(a, b) |> reduce;

let rec magnitude =
  fun
  | Pair(a, b) => 3 * (a |> magnitude) + 2 * (b |> magnitude)
  | Regular(v) => v;

let rec combinations =
  fun
  | [n, ...ns] =>
    (ns |> fold_left((acc, nn) => [(n, nn), (nn, n), ...acc], []))
    @ (ns |> combinations)
  | [] => [];

let input =
  "18.txt"
  |> read_file
  |> stream_map(line => line |> string_to_charlist |> parse |> fst);

input
|> tl
|> fold_left(add, input |> hd)
|> magnitude
|> print_int
|> print_newline;

input
|> combinations
|> map(((a, b)) => add(a, b) |> magnitude)
|> fold_left(max, 0)
|> print_int
|> print_newline;