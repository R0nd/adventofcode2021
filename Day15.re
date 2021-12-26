open Lib.Util;
open List;

let rec string_to_list =
  fun
  | "" => []
  | source => [
      String.sub(source, 0, 1),
      ...source
         |> String.length
         |> pred
         |> String.sub(source, 1)
         |> string_to_list,
    ];

type node =
  | Unmarked(int)
  | Marked(int, int)
  | Visited(int);

let prevnext = (arr, i) =>
  switch (i) {
  | 0 => [i |> succ]
  | _ when arr |> Array.length |> pred == i => [i |> pred]
  | _ => [i |> succ, i |> pred]
  };

let adjacent = (arr, x, y) =>
  (y |> prevnext(arr) |> map(yy => (x, yy)))
  @ (x |> prevnext(arr[y]) |> map(xx => (xx, y)));

let scan_buckets = buckets =>
  buckets
  |> Array.fold_left(
       (acc, n) =>
         switch (acc) {
         | [] => n
         | _ => acc
         },
       [],
     )
  |> (
    fun
    | [] => None
    | [n, ..._] => Some(n)
  );

let rec visit = (arr, buckets, x, y) => {
  switch (arr[y][x]) {
  | Unmarked(dis)
  | Marked(_, dis) =>
    adjacent(arr, x, y)
    |> iter(((xx, yy)) =>
         switch (arr[yy][xx]) {
         | Unmarked(i) =>
           arr[yy][xx] = Marked(i, dis + i);
           buckets[dis + i] = [
             (xx, yy),
             ...buckets[dis + i] |> filter(b => b != (xx, yy)),
           ];
         | Marked(i, ii) =>
           let next_dis = dis + i |> min(ii);
           arr[yy][xx] = Marked(i, next_dis);
           buckets[next_dis] = [
             (xx, yy),
             ...buckets[next_dis] |> filter(b => b != (xx, yy)),
           ];
         | _ => ()
         }
       );

    arr[y][x] = Visited(dis);
    buckets
    |> Array.iteri((i, _) =>
         buckets[i] = buckets[i] |> filter(b => b != (x, y))
       );
  | _ => ()
  };
  buckets
  |> scan_buckets
  |> (
    fun
    | None => ()
    | Some((xx, yy)) => visit(arr, buckets, xx, yy)
  );
};

let unwrap_visited =
  fun
  | Visited(i) => i
  | _ => raise(Failure("Not visited"));

let raw_input =
  "15.txt"
  |> read_file
  |> stream_map(x =>
       x |> string_to_list |> map(int_of_string) |> Array.of_list
     )
  |> Array.of_list;

let score = (h, w, mat) =>
  (mat[h |> pred][w |> pred] |> unwrap_visited)
  - (mat[0][0] |> unwrap_visited)
  |> print_int
  |> print_newline;

let input = raw_input |> Array.map(Array.map(cell => Unmarked(cell)));
let height = input |> Array.length;
let width = input[0] |> Array.length;
let max_dis = ((width |> pred) + (height |> pred)) * 9;
visit(input, Array.init(max_dis, _ => []), 0, 0);

input |> score(height, width);

let map_height = height * 5;
let map_width = width * 5;
let map =
  Array.init(map_height, y =>
    Array.init(map_width, x =>
      Unmarked(
        (x / width + y / height + raw_input[y mod height][x mod width] - 1)
        mod 9
        + 1,
      )
    )
  );
let max_dis_map = ((map_width |> pred) + (map_height |> pred)) * 9;
visit(map, Array.init(max_dis_map, _ => []), 0, 0);

map |> score(map_height, map_width);