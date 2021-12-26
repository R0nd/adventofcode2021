open Lib.Util;
open List;

let parse = line =>
  line
  |> String.split_on_char(':')
  |> rev
  |> hd
  |> String.trim
  |> int_of_string;

let go = (pos, roll) => ((pos |> pred) + roll) mod 10 |> succ;

let roll = die =>
  init(3, (~+))
  |> map(i => (i + die) mod 100 |> succ)
  |> fold_left((+), 0)
  |> (sum => (sum, die + 3));
let rec turn = (die, players) =>
  switch (players) {
  | (_, (_, score)) when score >= 1000 => (players, die)
  | ((a, ascore), b) =>
    let (roll_sum, next_die) = die |> roll;
    let next_a = roll_sum |> go(a);
    (b, (next_a, ascore + next_a)) |> turn(next_die);
  };

let seq = (ns, ms) => ns |> map(n => ms |> map((+)(n))) |> flatten;
let dirac_roll =
  [1, 2, 3]
  |> seq([1, 2, 3])
  |> seq([1, 2, 3])
  |> fold_left(
       (acc, n) =>
         [
           (n, acc |> assoc_opt(n) |> Option.value(~default=0) |> succ),
           ...acc |> remove_assoc(n),
         ],
       [],
     );
let rec dirac_turn = players =>
  switch (players) {
  | (_, (_, score)) when score >= 21 => [0, 1]
  | ((a, ascore), b) =>
    dirac_roll
    |> map(((roll, mul)) => {
         let next_a = roll |> go(a);
         (b, (next_a, ascore + next_a)) |> dirac_turn |> map(( * )(mul));
       })
    |> fold_left((acc, outcome) => acc |> map2((+), outcome), [0, 0])
    |> rev
  };

let list_to_tuple =
  fun
  | [a, b] => (a, b)
  | _ => failwith("list_to_tuple");

let input = "21.txt" |> read_file |> stream_map(parse);
let start = input |> map(pos => (pos, 0)) |> list_to_tuple;

let (((_, a), (_, b)), die) = start |> turn(0);
min(a, b) * die |> print_int |> print_newline;

start |> dirac_turn |> fold_left(max, 0) |> print_int |> print_newline;