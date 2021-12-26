open Lib.Util;
open List;

let parse = line =>
  line |> String.to_seq |> Seq.map((==)('#')) |> List.of_seq;

let pad = (z, ns) => [z, ...ns] @ [z];
let pad_image = (filler, image) =>
  image
  |> map(pad(filler))
  |> pad(init((image |> hd |> length) + 2, _ => filler));

let nth_opt = (ns, i) => i < 0 ? None : i |> nth_opt(ns);

let pred_succ = n => [pred, (~+), succ] |> map((|>)(n));
let slice_row = (col_i, filler, row) =>
  col_i
  |> pred_succ
  |> map(nth_opt(row))
  |> map(Option.value(~default=filler));
let slice = (row_i, col_i, filler, image) =>
  row_i
  |> pred_succ
  |> map(i =>
       i
       |> nth_opt(image)
       |> Option.map(slice_row(col_i, filler))
       |> Option.value(~default=init(3, _ => filler))
     )
  |> flatten;

let rec bin_to_int_internal =
  fun
  | [] => 0
  | [n, ...rest] => (n ? 1 : 0) + 2 * (rest |> bin_to_int_internal);

let bin_to_int = ns => ns |> rev |> bin_to_int_internal;

let enhance = (algorithm, filler, image) =>
  image
  |> mapi((i, row) =>
       row
       |> mapi((j, _) =>
            image |> slice(i, j, filler) |> bin_to_int |> nth(algorithm)
          )
     );

let file = "20.txt" |> read_file;
let algorithm = file |> Stream.next |> parse;
file |> Stream.next;
let image = file |> stream_map(parse);

[2, 50]
|> iter(e =>
     init(e, i => i mod 2 > 0)
     |> fold_left(
          (acc, filler) =>
            acc |> pad_image(filler) |> enhance(algorithm, filler),
          image,
        )
     |> flatten
     |> filter(x => x)
     |> length
     |> print_int
     |> print_newline
   );