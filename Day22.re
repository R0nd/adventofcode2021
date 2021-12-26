open Lib.Util;
open List;

type range =
  | Range(int, int);
type cuboid =
  | Cuboid(range, range, range);
type action =
  | Action(bool, cuboid);

let parse_cuboid = cuboid_string =>
  cuboid_string
  |> String.split_on_char(',')
  |> map(s => String.sub(s, 2, (s |> String.length) - 2))
  |> map(s =>
       s
       |> String.split_on_char('.')
       |> (
         ss =>
           Range(ss |> hd |> int_of_string, ss |> rev |> hd |> int_of_string)
       )
     )
  |> (
    fun
    | [x, y, z] => Cuboid(x, y, z)
    | _ => failwith("parse_cuboid")
  );

let parse = line =>
  line
  |> String.split_on_char(' ')
  |> (
    fun
    | [on_off, cuboid_string] =>
      Action(on_off == "on", cuboid_string |> parse_cuboid)
    | _ => failwith("Illegal action")
  );

let range_intersects = (Range(a0, a1), Range(b0, b1)) =>
  b0 <= a1 && b1 >= a0;

let range_intersection = (Range(a0, a1), Range(b0, b1)) =>
  Range(max(a0, b0), min(a1, b1));

let intersection = (Cuboid(xa, ya, za), Cuboid(xb, yb, zb)) =>
  for_all2(range_intersects, [xa, ya, za], [xb, yb, zb])
    ? Some(
        Cuboid(
          range_intersection(xa, xb),
          range_intersection(ya, yb),
          range_intersection(za, zb),
        ),
      )
    : None;

let left_right =
    (Cuboid(Range(xa0, xa1), y, z), Cuboid(Range(xb0, xb1), _, _)) => [
  Cuboid(Range(xa0, xb0 - 1), y, z),
  Cuboid(Range(xb1 + 1, xa1), y, z),
];

let forward_back =
    (Cuboid(_, Range(ya0, ya1), z), Cuboid(x, Range(yb0, yb1), _)) => [
  Cuboid(x, Range(ya0, yb0 - 1), z),
  Cuboid(x, Range(yb1 + 1, ya1), z),
];

let up_down =
    (Cuboid(_, _, Range(za0, za1)), Cuboid(x, y, Range(zb0, zb1))) => [
  Cuboid(x, y, Range(za0, zb0 - 1)),
  Cuboid(x, y, Range(zb1 + 1, za1)),
];

let positive = (Cuboid(Range(x0, x1), Range(y0, y1), Range(z0, z1))) =>
  for_all2((<=), [x0, y0, z0], [x1, y1, z1]);

let sub = (a, b) =>
  switch (intersection(a, b)) {
  | Some(i) =>
    [left_right, forward_back, up_down]
    |> map(f => f(a, i))
    |> flatten
    |> filter(positive)
  | _ => [a]
  };

let act = (cuboids, Action(value, b)) =>
  cuboids
  |> map(a => sub(a, b))
  |> flatten
  |> (res => value ? [b, ...res] : res);

let len = (Range(a0, a1)) => a1 - a0 + 1;

let volume = (Cuboid(x, y, z)) =>
  [x, y, z] |> map(len) |> fold_left(( * ), 1);

let solve = actions =>
  actions
  |> fold_left(act, [])
  |> map(volume)
  |> fold_left((+), 0)
  |> print_int
  |> print_newline;

let input = "22.txt" |> read_file |> stream_map(parse);

let section = Cuboid(Range(-50, 50), Range(-50, 50), Range(-50, 50));

input
|> map((Action(v, c)) =>
     intersection(c, section) |> Option.map(i => Action(v, i))
   )
|> filter(Option.is_some)
|> map(Option.get)
|> solve;

input |> solve;