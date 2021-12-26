open List;
open Lib.Fn;
open Lib.Util;

let string_tail = source =>
  source |> String.length |> pred |> String.sub(source, 1);

let rec string_to_charlist = source =>
  switch (source) {
  | "" => []
  | _ => [
      source.[0] |> String.make(1),
      ...source |> string_tail |> string_to_charlist,
    ]
  };

let parse = source =>
  source |> string_to_charlist |> map(int_of_string) |> Array.of_list;

let prevnext_idx = (arr, i) =>
  switch (i) {
  | 0 => [i |> succ]
  | _ when i == (arr |> Array.length |> pred) => [i |> pred]
  | _ => [i |> pred, i |> succ]
  };
let prevnext = (arr, i) => prevnext_idx(arr, i) |> map(idx => arr[idx]);
let adjacent = (x, y, mat) =>
  y |> prevnext(mat) |> map(row => row[x]) |> append(x |> prevnext(mat[y]));

let is_lowest = (mat, x, y) =>
  mat |> adjacent(x, y) |> for_all(a => mat[y][x] < a);

let input = "9.txt" |> read_file |> stream_map(parse) |> Array.of_list;

input
|> Array.mapi((y, row) =>
     row
     |> Array.mapi((x, cell) => is_lowest(input, x, y) ? cell |> succ : 0)
   )
|> Array.map(Array.fold_left((+), 0))
|> Array.fold_left((+), 0)
|> print_int
|> print_newline;

let rec process = (x, y, mat) =>
  mat[y][x]
    ? {
      mat[y][x] = false;
      y
      |> prevnext_idx(mat)
      |> map(yy => (x, yy))
      |> append(x |> prevnext_idx(mat[y]) |> map(xx => (xx, y)))
      |> map(((xx, yy)) => mat |> process(xx, yy))
      |> fold_left((+), 0)
      |> succ;
    }
    : 0;

let basinate = mat =>
  mat
  |> Array.mapi((y, row) =>
       row |> Array.mapi((x, _) => mat |> process(x, y)) |> Array.to_list
     )
  |> Array.to_list
  |> flatten;

input
|> Array.map(row => row |> Array.map(cell => cell !== 9))
|> basinate
|> sort((-))
|> rev
|> take(3)
|> fold_left(( * ), 1)
|> print_int
|> print_newline;