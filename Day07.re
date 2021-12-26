open Lib.Util;
open List;

let split = Str.split(Str.regexp(","));

let input =
  "7.txt" |> read_file |> Stream.next |> split |> map(int_of_string);

let count = input |> length;
let sorted = input |> sort((a, b) => a - b);
let median = count / 2 |> nth(sorted);

let abs_dev = (sample, value) =>
  sample |> fold_left((acc, n) => acc + abs(n - value), 0);

median |> abs_dev(input) |> print_int |> print_newline;

let crab_fuel = n => (n * n + n) / 2;
let crab_dev = (sample, value) =>
  sample |> fold_left((acc, n) => acc + (n - value |> abs |> crab_fuel), 0);

let mean = (input |> fold_left((+), 0)) / count;
mean |> crab_dev(input) |> print_int |> print_newline;