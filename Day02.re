open Lib.Util;

let rec move = (forward, depth, commands) =>
  switch (commands) {
  | [] => forward * depth
  | [("forward", value), ...rest] => rest |> move(forward + value, depth)
  | [("down", value), ...rest] => rest |> move(forward, depth + value)
  | [("up", value), ...rest] => rest |> move(forward, depth - value)
  | _ => raise(Failure("Invalid command"))
  };

let rec move_aimed = (forward, depth, aim, commands) =>
  switch (commands) {
  | [] => forward * depth
  | [("forward", value), ...rest] =>
    rest |> move_aimed(forward + value, depth + value * aim, aim)
  | [("down", value), ...rest] =>
    rest |> move_aimed(forward, depth, aim + value)
  | [("up", value), ...rest] =>
    rest |> move_aimed(forward, depth, aim - value)
  | _ => raise(Failure("Invalid command"))
  };

let split = Str.split(Str.regexp(" "));

let parse = line =>
  switch (line |> split) {
  | [command, value] => (command, value |> int_of_string)
  | _ => raise(Failure("Invalid command format"))
  };

let input = "2.txt" |> read_file |> stream_map(parse);

input |> move(0, 0) |> print_int |> print_newline;
input |> move_aimed(0, 0, 0) |> print_int |> print_newline;