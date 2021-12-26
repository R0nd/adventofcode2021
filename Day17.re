open Lib.Util;
open List;

let first = (f, (a, b)) => (a |> f, b);
let second = (f, (a, b)) => (a, b |> f);

let min_of_tuple = ((a, b)) => min(a, b);
let max_of_tuple = ((a, b)) => max(a, b);

let sign = n =>
  switch (n) {
  | 0 => 0
  | _ when n < 0 => (-1)
  | _ => 1
  };

let rec hit_test = (x, y, xvel, yvel, ((tx0, tx1), (ty0, ty1))) =>
  switch (x, y) {
  | _ when x < tx0 && y >= ty0 || y > ty1 && x <= tx1 =>
    hit_test(
      x + xvel,
      y + yvel,
      xvel - (xvel |> sign),
      yvel |> pred,
      ((tx0, tx1), (ty0, ty1)),
    )
  | _ when x > tx1 || y < ty0 => false
  | _ => true
  };

let input = "17.txt" |> read_file |> Stream.next;

Str.string_match(
  Str.regexp(
    "target area: x=\\(-?[0-9]+\\)..\\(-?[0-9]+\\), y=\\(-?[0-9]+\\)..\\(-?[0-9]+\\)",
  ),
  input,
  0,
);

let target =
  [1, 2, 3, 4]
  |> map(i => Str.matched_group(i, input) |> int_of_string)
  |> (
    fun
    | [x0, x1, y0, y1] => ((x0, x1), (y0, y1))
    | _ => raise(Failure("Failed to parse target"))
  );

let xvels =
  target
  |> fst
  |> first(x =>
       x * 2 |> float_of_int |> sqrt |> floor |> int_of_float |> pred
     )
  |> second(x => x * 2 |> float_of_int |> sqrt |> ceil |> int_of_float |> pred);

let min_height = target |> snd |> min_of_tuple |> abs;
min_height * (min_height |> pred) / 2 |> print_int |> print_newline;

let min_yvel = target |> snd |> min_of_tuple;
let max_yvel = target |> snd |> min_of_tuple |> abs;

let min_xvel = xvels |> min_of_tuple;
let max_xvel = target |> fst |> max_of_tuple;

init(max_yvel - min_yvel |> succ, i => min_yvel + i)
|> map(y =>
     init(max_xvel - min_xvel |> succ, i => min_xvel + i)
     |> map(x => hit_test(0, 0, x, y, target))
   )
|> flatten
|> filter(x => x)
|> length
|> print_int
|> print_newline;