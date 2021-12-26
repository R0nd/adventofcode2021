open Lib.Util;
open List;

let split = Str.split(Str.regexp(","));

let array_mut = (f, arr, i) => arr[i] |> f |> Array.set(arr, i);

let tick = (fishes, next_fishes, i) => {
  let weekday = i mod 7;
  next_fishes[(i + 2) mod 7] = fishes[weekday];
  weekday |> array_mut(n => n + next_fishes[weekday], fishes);
  next_fishes[weekday] = 0;
};

let rec repeat = (max, f, i) =>
  i == max
    ? ()
    : {
      i |> f;
      i |> succ |> repeat(max, f);
    };

let input =
  "6.txt" |> read_file |> Stream.next |> split |> map(int_of_string);

let fish_groups = Array.make(7, 0);
let next_fishes = Array.make(7, 0);

input |> iter(array_mut(succ, fish_groups));

let intermediate_day = 80;

0 |> repeat(intermediate_day, tick(fish_groups, next_fishes));

let count_fishes = (groups_a, groups_b) =>
  groups_a |> Array.append(groups_b) |> Array.fold_left((a, b) => a + b, 0);

count_fishes(fish_groups, next_fishes) |> print_int |> print_newline;

intermediate_day |> repeat(256, tick(fish_groups, next_fishes));

count_fishes(fish_groups, next_fishes) |> print_int |> print_newline;