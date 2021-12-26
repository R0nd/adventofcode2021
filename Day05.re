open Lib.Fn;
open Lib.Util;
open List;

let split = delim => Str.split(Str.regexp(delim));

let list_to_tuple =
  fun
  | [a, b] => (a, b)
  | _ => raise(Failure("Invalid list length"));

let parse_coord = coord_str =>
  coord_str |> split(",") |> map(int_of_string) |> list_to_tuple;

let parse_line = line =>
  switch (line |> split(" -> ")) {
  | [] => raise(Failure("Invalid command format"))
  | ns => ns |> map(parse_coord) |> list_to_tuple
  };

let input = "5.txt" |> read_file |> stream_map(parse_line);

let is_horizontal = (((_, y1), (_, y2))) => y1 == y2;
let is_vertical = (((x1, _), (x2, _))) => x1 == x2;

let grid = 0 |> Array.make_matrix(1000, 1000);
let rec range = ((a, b)) =>
  switch (a, b) {
  | _ when a == b => [a]
  | _ => [a, ...range((a + 1, b))]
  };

let array_succ = (i, arr) =>
  i |> Array.get(arr) |> succ |> Array.set(arr, i);
let grid_succ = (x, y, grid) => y |> Array.get(grid) |> array_succ(x);

let rangify = ns => ns |> sort((a, b) => a - b) |> list_to_tuple |> range;

let tally = points =>
  points |> flatten |> iter(((x, y)) => grid |> grid_succ(x, y));

input
|> filter(is_horizontal)
|> map((((x1, y), (x2, _))) => [x1, x2] |> rangify |> map(x => (x, y)))
|> tally;

input
|> filter(is_vertical)
|> map((((x, y1), (_, y2))) => [y1, y2] |> rangify |> map(y => (x, y)))
|> tally;

let print_sum = grid =>
  grid
  |> Array.fold_left(
       (acc, row) =>
         acc
         + (
           row |> Array.fold_left((acc, n) => acc |> (n > 1 ? succ : id), 0)
         ),
       0,
     )
  |> print_int
  |> print_newline;

grid |> print_sum;

let is_diagonal = (((x1, y1), (x2, y2))) => abs(y2 - y1) == abs(x2 - x1);
let rec diagonal_range = (((x1, y1), (x2, y2))) =>
  switch (x1, x2, y1, y2) {
  | _ when x1 == x2 && y1 == y2 => [(x1, y1)]
  | _ => [
      (x1, y1),
      ...diagonal_range((
           (x1 |> (x2 > x1 ? succ : pred), y1 |> (y2 > y1 ? succ : pred)),
           (x2, y2),
         )),
    ]
  };

input
|> filter(is_diagonal)
|> map(((a, b)) =>
     [a, b]
     |> sort(((_, y1), (_, y2)) => y1 - y2)
     |> list_to_tuple
     |> diagonal_range
   )
|> tally;

grid |> print_sum;