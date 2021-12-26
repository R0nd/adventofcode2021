open Lib.Fn;
open List;

type room =
  | Room(list(int), list(int), list(int), int);

let align_left = (distances, storage) =>
  storage |> take(distances |> length);
let align_right = (distances, storage) =>
  storage |> skip((storage |> length) - (distances |> length));

let replacei = (i, m, ns) => ns |> mapi((ii, n) => ii == i ? m : n);

let rec moves = i =>
  fun
  | [(dis, None), ...rest] => [(i, dis), ...rest |> moves(i |> succ)]
  | _ => [];

let rec push = (t, i) =>
  fun
  | [(_, None), ...rest] => rest |> push(t, i |> succ)
  | [(dis, Some(tt)), ..._] when tt == t => [(i, dis)]
  | _ => [];

let min_list = fold_left(min, max_int);

let rec move = (sum, room_size, cap, rooms, storage) =>
  if (sum > cap) {
    max_int;
  } else if (rooms
             |> for_all((Room(b, _, _, t)) =>
                  b |> length == room_size && b |> for_all((==)(t))
                )) {
    sum;
  } else {
    let lefts =
      rooms
      |> mapi((i, room) =>
           switch (room) {
           | Room([cell, ...rest] as cells, distances, dr, t)
               when !for_all(cell => cell == t, cells) =>
             storage
             |> align_left(distances)
             |> combine(distances)
             |> rev
             |> moves(0)
             |> fold_left(
                  (acc, (j, m)) =>
                    move(
                      sum + cell * (room_size - (rest |> length) + m),
                      room_size,
                      acc,
                      rooms |> replacei(i, Room(rest, distances, dr, t)),
                      storage
                      |> replacei(
                           (distances |> length |> pred) - j,
                           Some(cell),
                         ),
                    )
                    |> min(acc),
                  cap,
                )
           | _ => max_int
           }
         )
      |> min_list;
    let rights =
      rooms
      |> mapi((i, room) =>
           switch (room) {
           | Room([cell, ...rest] as cells, dl, distances, t)
               when !for_all(cell => cell == t, cells) =>
             storage
             |> align_right(distances)
             |> combine(distances)
             |> moves(0)
             |> fold_left(
                  (acc, (j, m)) =>
                    move(
                      sum + cell * (room_size - (rest |> length) + m),
                      room_size,
                      acc,
                      rooms |> replacei(i, Room(rest, dl, distances, t)),
                      storage |> replacei(j + (dl |> length), Some(cell)),
                    )
                    |> min(acc),
                  cap,
                )
           | _ => max_int
           }
         )
      |> min_list;
    let pushes =
      rooms
      |> mapi((i, room) =>
           switch (room) {
           | Room(cells, dl, dr, t) when cells |> for_all(tt => tt == t) =>
             storage
             |> align_left(dl)
             |> combine(dl)
             |> rev
             |> push(t, 0)
             |> fold_left(
                  (acc, (j, dis)) =>
                    move(
                      sum + t * (dis + (room_size - (cells |> length))),
                      room_size,
                      acc,
                      rooms |> replacei(i, Room([t, ...cells], dl, dr, t)),
                      storage |> replacei((dl |> length |> pred) - j, None),
                    )
                    |> min(acc),
                  cap,
                )
             |> min(
                  storage
                  |> align_right(dr)
                  |> combine(dr)
                  |> push(t, 0)
                  |> fold_left(
                       (acc, (j, dis)) =>
                         move(
                           sum + t * (dis + (room_size - (cells |> length))),
                           room_size,
                           acc,
                           rooms
                           |> replacei(i, Room([t, ...cells], dl, dr, t)),
                           storage |> replacei((dl |> length) + j, None),
                         )
                         |> min(acc),
                       cap,
                     ),
                )
           | _ => max_int
           }
         )
      |> min_list;
    [lefts, rights, pushes] |> min_list;
  };

let storage = init(7, _ => None);

let room_a_short = Room([100, 10], [2, 1], [1, 3, 5, 7, 8], 1);
let room_b_short = Room([1000, 1], [4, 3, 1], [1, 3, 5, 6], 10);
let room_c_short = Room([1000, 10], [6, 5, 3, 1], [1, 3, 4], 100);
let room_d_short = Room([1, 100], [8, 7, 5, 3, 1], [1, 2], 1000);

move(
  0,
  2,
  max_int,
  [room_a_short, room_b_short, room_c_short, room_d_short],
  storage,
)
|> print_int
|> print_newline;

let room_a = Room([100, 1000, 1000, 10], [2, 1], [1, 3, 5, 7, 8], 1);
let room_b = Room([1000, 100, 10, 1], [4, 3, 1], [1, 3, 5, 6], 10);
let room_c = Room([1000, 10, 1, 10], [6, 5, 3, 1], [1, 3, 4], 100);
let room_d = Room([1, 1, 100, 100], [8, 7, 5, 3, 1], [1, 2], 1000);

move(0, 4, max_int, [room_a, room_b, room_c, room_d], storage)
|> print_int
|> print_newline;