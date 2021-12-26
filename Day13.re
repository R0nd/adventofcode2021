open Lib.Fn;
open Lib.Util;
open List;

let str_split = delim => Str.split(Str.regexp(delim));

type dimension =
  | X
  | Y;

type line =
  | Dot(int, int)
  | Fold(dimension, int)
  | Empty;

let parse_dot =
  fun
  | [x, y] => Dot(x, y)
  | _ => raise(Failure("Invalid dot coordinates length"));

let parse_fold = (dim, coord) =>
  switch (dim) {
  | "x" => Fold(X, coord)
  | "y" => Fold(Y, coord)
  | _ => raise(Failure("Invalid fold dimension"))
  };

let parse_line = line =>
  switch (line) {
  | "" => Empty
  | _ when Str.string_match(Str.regexp("[0-9]+,[0-9]+"), line, 0) =>
    line |> str_split(",") |> map(int_of_string) |> parse_dot
  | _
      when
        Str.string_match(
          Str.regexp("fold along \\([xy]\\)=\\([0-9]+\\)"),
          line,
          0,
        ) =>
    parse_fold(
      Str.matched_group(1, line),
      Str.matched_group(2, line) |> int_of_string,
    )
  | _ => raise(Failure("Unrecognized input line"))
  };

let is_folded = ((dim, coord), (x, y)) =>
  switch (dim) {
  | X => x > coord
  | Y => y > coord
  };

let fold_dot = ((dim, coord), (x, y)) =>
  switch (dim) {
  | X => (coord - (x - coord), y)
  | Y => (x, coord - (y - coord))
  };

let fold_dots = (fold, dots) => {
  let (folded, unfolded) = dots |> partition(is_folded(fold));
  unfolded
  @ (
    folded
    |> map(fold_dot(fold))
    |> filter(dot => unfolded |> mem(dot) |> not)
  );
};

let (raw_dots, raw_folds) =
  "13.txt"
  |> read_file
  |> stream_map(parse_line)
  |> filter(x => x != Empty)
  |> partition(
       fun
       | Dot(_) => true
       | _ => false,
     );

let dots =
  raw_dots
  |> map(
       fun
       | Dot(x, y) => (x, y)
       | _ => raise(Failure("raw_dots")),
     );

let folds =
  raw_folds
  |> map(
       fun
       | Fold(dim, coord) => (dim, coord)
       | _ => raise(Failure("raw_folds")),
     );

dots |> fold_dots(folds |> hd) |> length |> print_int |> print_newline;

let folded_dots =
  folds |> fold_left((acc, fold) => acc |> fold_dots(fold), dots);
let (width, height) =
  folded_dots
  |> split
  |> (((xs, ys)) => (xs |> fold_left(max, 0), ys |> fold_left(max, 0)));

let serialize_line = (width, dots) =>
  String.init(width |> succ, x =>
    dots |> exists(((xx, _)) => xx == x) ? '#' : '.'
  );

let rec serialize_grid = (dots, width, y) =>
  switch (y) {
  | (-1) => []
  | _ => [
      dots |> filter(((_, yy)) => yy == y) |> serialize_line(width),
      ...y |> pred |> serialize_grid(dots, width),
    ]
  };

height
|> serialize_grid(folded_dots, width)
|> rev
|> iter(line => line |> print_string |> print_newline);