open Lib.Util;
open Lib.Fn;

let rec increases = ns => {
  switch (ns) {
  | []
  | [_] => 0
  | [n, next, ...rest] when n < next => ([next, ...rest] |> increases) + 1
  | [_, ...rest] => rest |> increases
  };
};

let rec window_increases = ns => {
  switch (ns) {
  | _ when ns |> List.length < 4 => 0
  | _ when ns |> List.tl |> take(3) |> sum > (ns |> take(3) |> sum) =>
    (ns |> List.tl |> window_increases) + 1
  | _ => ns |> List.tl |> window_increases
  };
};

let input = "1.txt" |> read_file |> stream_map(int_of_string);

input |> increases |> print_int |> print_newline;

input |> window_increases |> print_int |> print_newline;