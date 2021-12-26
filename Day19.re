open Lib.Util;
open List;

let rec split = (delim, acc, ns) =>
  switch (ns) {
  | [n, ...rest] when n == delim => rest |> split(delim, [[], ...acc])
  | [n, ...rest] => rest |> split(delim, [[n, ...acc |> hd], ...acc |> tl])
  | [] => acc
  };

let row = (i, a) => a[i];

let cols = a => a[0] |> Array.mapi((j, _) => a |> Array.map(aa => aa[j]));

let mul = (a, b) =>
  a
  |> Array.map(arow =>
       b
       |> cols
       |> Array.map(bcol =>
            bcol |> Array.map2(( * ), arow) |> Array.fold_left((+), 0)
          )
     );

let uniq = arr =>
  arr |> fold_left((acc, n) => acc |> mem(n) ? acc : [n, ...acc], []);

let scans =
  "19.txt"
  |> read_file
  |> stream_map(x => x)
  |> split("", [[]])
  |> map(batch =>
       batch
       |> filter(line => String.sub(line, 0, 3) != "---")
       |> map(line =>
            line
            |> String.split_on_char(',')
            |> map(int_of_string)
            |> map(x => [|x|])
            |> Array.of_list
          )
     );

let o = [|[|1, 0, 0|], [|0, 1, 0|], [|0, 0, 1|]|];
let x = [|[|1, 0, 0|], [|0, 0, (-1)|], [|0, 1, 0|]|];
let y = [|[|0, 0, 1|], [|0, 1, 0|], [|(-1), 0, 0|]|];
let z = [|[|0, (-1), 0|], [|1, 0, 0|], [|0, 0, 1|]|];

let x2 = mul(x, x);
let x3 = mul(x2, x);
let xs = [o, x, x2, x3];

let y2 = mul(y, y);
let y3 = mul(y2, y);
let ys = [o, y, y2, y3];

let z2 = mul(z, z);
let z3 = mul(z2, z);
let zs = [o, z, z2, z3];

let rotations =
  xs
  |> map(xx => ys |> map(yy => mul(xx, yy)))
  |> flatten
  |> map(xy => zs |> map(zz => mul(xy, zz)))
  |> flatten
  |> uniq;

let sub = (a, b) => Array.map2((-), a, b);
let sub_mat = (a, b) => Array.map2(sub, a, b);

let add = (a, b) => Array.map2((+), a, b);
let add_mat = (a, b) => Array.map2(add, a, b);

let manhattan = a =>
  a
  |> Array.fold_left(
       (acc, aa) => acc + (aa |> Array.map(abs) |> Array.fold_left((+), 0)),
       0,
     );

let align = (scan, point) =>
  scan
  |> map(cell => point |> sub_mat(cell))
  |> map(diff => (scan |> map(cell => diff |> sub_mat(cell)), diff));

let overlap = (a, b) => a |> filter(aa => b |> mem(aa)) |> length >= 12;

type overlay =
  | Overlap(list(array(array(int))), array(array(int)))
  | Miss(list(array(array(int))))
  | Undefined;

let overlap_scan = (f, ns): overlay =>
  ns
  |> fold_left(
       (acc, n) =>
         switch (acc) {
         | Overlap(_) => acc
         | _ => n |> f
         },
       Undefined,
     );

let second = (f, (a, b)) => (a, b |> f);
let both = (f, (a, b)) => (a |> f, b |> f);

let overlap_partition = ns =>
  ns
  |> partition(
       fun
       | Overlap(_) => true
       | _ => false,
     );

let unwrap =
  fun
  | Overlap(n, _) => n
  | Miss(n) => n
  | Undefined => raise(Failure("unwrap"));

let unwrap_position =
  fun
  | Overlap(_, p) => p
  | _ => raise(Failure("unwrap_position"));

let rec reconstruct = (composite, scans) =>
  switch (scans) {
  | [] => composite
  | _ =>
    let (os, ms) =
      scans
      |> map(scan =>
           rotations
           |> overlap_scan(rotation => {
                let scan_rotation = scan |> map(mul(rotation));
                composite
                |> overlap_scan(comp_scan =>
                     comp_scan
                     |> unwrap
                     |> map(align(scan_rotation))
                     |> flatten
                     |> overlap_scan(((alignment, distance)) =>
                          alignment |> overlap(comp_scan |> unwrap)
                            ? Overlap(alignment, distance) : Miss(scan)
                        )
                   );
              })
         )
      |> overlap_partition;
    '.' |> String.make(os |> length) |> print_string;
    reconstruct(composite @ os, ms |> map(unwrap));
  };

let composite =
  scans
  |> tl
  |> reconstruct([Overlap(scans |> hd, [|[|0|], [|0|], [|0|]|])]);

print_newline();

composite
|> map(unwrap)
|> flatten
|> uniq
|> length
|> print_int
|> print_newline;

let rec pairs =
  fun
  | [] => []
  | [n, ...rest] => (rest |> map(m => (n, m))) @ (rest |> pairs);

composite
|> map(unwrap_position)
|> pairs
|> map(((a, b)) => sub_mat(a, b) |> manhattan)
|> fold_left(max, 0)
|> print_int
|> print_newline;