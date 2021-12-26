open Lib.Fn;
open Lib.Util;
open List;

let split = Str.split(Str.regexp("-"));

type node =
  | Start
  | End
  | Large(string)
  | Small(string);

let parse_node =
  fun
  | "start" => Start
  | "end" => End
  | key when Str.string_match(Str.regexp("[A-Z][A-Z]"), key, 0) =>
    Large(key)
  | key => Small(key);

let parse_line = line => line |> split |> map(parse_node);

let visit = (node, visited) =>
  switch (node) {
  | Small(_) => [node, ...visited]
  | _ => visited
  };

let rec search = (edges, visited, visited_twice, node) => {
  switch (node) {
  | End => 1
  | _ =>
    let (visited, unvisited) =
      edges
      |> filter(mem(node))
      |> map(find(n => n != node))
      |> filter(n => n != Start)
      |> partition(n => visited |> mem(n));

    unvisited
    |> map(n => n |> search(edges, visited |> visit(n), visited_twice))
    |> (
      visited_twice
        ? id : append(visited |> map(n => n |> search(edges, visited, true)))
    )
    |> fold_left((+), 0);
  };
};

let input = "12.txt" |> read_file |> stream_map(parse_line);

Start |> search(input, [], true) |> print_int |> print_newline;
Start |> search(input, [], false) |> print_int |> print_newline;