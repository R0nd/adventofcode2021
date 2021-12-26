open Lib.Util;
open Lib.Fn;

let split = delimiter => Str.split(Str.regexp(delimiter));

let parse_row = row =>
  row |> split(" +") |> List.map(cell => Some(cell |> int_of_string));

let rec parse_table = stream =>
  switch (stream |> Stream.next) {
  | "" => []
  | exception Stream.Failure => []
  | line => [line |> parse_row, ...stream |> parse_table]
  };

let rec parse_tables = stream =>
  switch (stream |> parse_table) {
  | [] => []
  | table => [table, ...stream |> parse_tables]
  };

let parse_file = stream => {
  let call_order =
    stream |> Stream.next |> split(",") |> List.map(int_of_string);
  stream |> Stream.junk;
  let tables = stream |> parse_tables;
  (call_order, tables);
};

let has_empty_rows = table =>
  table |> List.exists(List.for_all(cell => cell == None));
let squash_cells = (a, b) => a == None && b == None ? None : Some(0);
let squash_rows = (a, b) => List.map2(squash_cells, a, b);
let has_empty_cols = table =>
  table
  |> List.tl
  |> List.fold_left(squash_rows, table |> List.hd)
  |> List.mem(None);
let is_winning = table => table |> has_empty_rows || table |> has_empty_cols;

let call_cell = (n, cell) =>
  switch (cell) {
  | Some(c) when c == n => None
  | _ => cell
  };
let call_row = (n, row) => row |> List.map(call_cell(n));
let call_table = (n, table) => table |> List.map(call_row(n));
let rec call = (call_order, tables) =>
  switch (call_order) {
  | [n, ...ns] =>
    let called_tables = tables |> List.map(call_table(n));
    switch (called_tables |> List.filter(is_winning)) {
    | [table] => (n, table)
    | [] => called_tables |> call(ns)
    | _ => raise(Failure("Multiple tables won"))
    };
  | _ => raise(Failure("No tables won"))
  };

let score = ((n, table)) =>
  n * (table |> List.flatten |> List.filter_map(id) |> sum);

let (call_order, tables) = "4.txt" |> read_file |> parse_file;

tables |> call(call_order) |> score |> print_int |> print_newline;

let rec call_til_last = (call_order, tables) =>
  switch (call_order) {
  | [n, ...ns] =>
    let called_tables =
      tables
      |> List.map(call_table(n))
      |> List.filter(table => table |> is_winning |> not);
    switch (called_tables) {
    | [table] => (ns, table)
    | [] => raise(Failure("No tables left"))
    | _ => called_tables |> call_til_last(ns)
    };
  | _ => raise(Failure("No calls left"))
  };
let call_til_wins = ((call_order, table)) => call(call_order, [table]);

tables
|> call_til_last(call_order)
|> call_til_wins
|> score
|> print_int
|> print_newline;