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

type octopus =
  | Flashed(int)
  | NotFlashed(int);

let parse_line = line =>
  line
  |> string_to_list
  |> map(c => NotFlashed(c |> String.make(1) |> int_of_string))
  |> Array.of_list;

let octosucc = (x, y, grid) =>
  grid[y][x] = (
    switch (grid[y][x]) {
    | Flashed(i) => Flashed(i |> succ)
    | NotFlashed(i) => NotFlashed(i |> succ)
    }
  );

let prevnext = (i, arr) =>
  switch (i) {
  | 0 => [i, i |> succ]
  | _ when i == (arr |> Array.length |> pred) => [i |> pred, i]
  | _ => [i |> pred, i, i |> succ]
  };
let adjacent = (x, y, grid) =>
  grid
  |> prevnext(y)
  |> map(yy => grid[yy] |> prevnext(x) |> map(xx => (xx, yy)))
  |> flatten
  |> filter(((xx, yy)) => !(xx == x && yy == y));

let rec flash = (x, y, grid) =>
  switch (grid[y][x]) {
  | NotFlashed(i) when i > 9 =>
    grid[y][x] = Flashed(i);
    grid
    |> adjacent(x, y)
    |> map(((xx, yy)) => {
         grid |> octosucc(xx, yy);
         grid |> flash(xx, yy);
       })
    |> fold_left((+), 0)
    |> succ;
  | _ => 0
  };

let drain = (x, y, grid) => {
  switch (grid[y][x]) {
  | Flashed(_) => grid[y][x] = NotFlashed(0)
  | _ => ()
  };
};

let input = "11.txt" |> read_file |> stream_map(parse_line) |> Array.of_list;

let apply = (f, grid) =>
  grid
  |> Array.iteri((yy, row) =>
       row |> Array.iteri((xx, _) => grid |> f(xx, yy))
     );

let apply_sum = (f, grid) =>
  grid
  |> Array.mapi((yy, row) =>
       row |> Array.mapi((xx, _) => grid |> f(xx, yy))
     )
  |> Array.map(Array.fold_left((+), 0))
  |> Array.fold_left((+), 0);

let tick = grid => {
  grid |> apply(octosucc);
  let result = grid |> apply_sum(flash);
  grid |> apply(drain);
  result;
};

let rec repeat = (f, n) =>
  switch (n) {
  | 1 => f()
  | _ => f() + (n |> pred |> repeat(f))
  };

100 |> repeat(() => input |> tick) |> print_int |> print_newline;

let rec wait_all = i => {
  input |> apply(octosucc);
  input |> apply_sum(flash) |> ignore;
  let result =
    input
    |> Array.map(row =>
         row
         |> Array.fold_left(
              (acc, cell) =>
                switch (cell) {
                | Flashed(_) => acc |> succ
                | _ => acc
                },
              0,
            )
       )
    |> Array.fold_left((+), 0);
  input |> apply(drain);
  switch (result) {
  | 100 => i |> print_int |> print_newline
  | _ => i |> succ |> wait_all
  };
};

101 |> wait_all;