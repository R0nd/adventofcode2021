open Lib.Util;
open List;

type direction =
  | E
  | S;

let parse_cell =
  fun
  | '.' => None
  | '>' => Some(E)
  | 'v' => Some(S)
  | _ => failwith("parse_cell");

let rec parse =
  fun
  | "" => []
  | line => [
      line.[0] |> parse_cell,
      ...String.sub(line, 1, line |> String.length |> pred) |> parse,
    ];

let tl_emp =
  fun
  | [] => []
  | [_, ...rest] => rest;

let hd_eql = (a, ns) =>
  switch (ns) {
  | [n, ..._] => a == n
  | _ => false
  };

let move_e = line =>
  line
  |> fold_left(
       ((acc, (prev, rest)), n) => {
         let nn =
           switch (n) {
           | Some(E) when rest |> hd_eql(None) => None
           | Some(E)
               when
                 acc
                 |> length == (line |> length |> pred)
                 && line
                 |> hd == None =>
             None
           | None when prev == Some(E) => Some(E)
           | None when acc == [] && line |> rev |> hd == Some(E) => Some(E)
           | _ => n
           };
         ([nn, ...acc], (n, rest |> tl_emp));
       },
       ([], (None, line |> tl)),
     )
  |> fst
  |> rev;

let transpose =
  fun
  | [] => None
  | nss =>
    Some(nss |> hd |> mapi((i, _) => nss |> map(ns => i |> nth(ns))));

let move_s = lines =>
  lines
  |> fold_left(
       ((acc_outer, (prev, rest)), line) => {
         let nns =
           fold_left2(
             (acc, n, (prev, rest)) => {
               let nn =
                 switch (n) {
                 | Some(S) when rest |> hd_eql(None) => None
                 | Some(S)
                     when
                       acc_outer
                       |> length == (lines |> length |> pred)
                       && acc
                       |> length
                       |> nth(lines |> hd) == None =>
                   None
                 | None when prev == Some(S) => Some(S)
                 | None
                     when
                       acc_outer == []
                       && acc
                       |> length
                       |> nth(lines |> rev |> hd) == Some(S) =>
                   Some(S)
                 | _ => n
                 };
               [nn, ...acc];
             },
             [],
             line,
             combine(
               prev,
               rest
               |> transpose
               |> Option.value(~default=prev |> map(_ => [])),
             ),
           )
           |> rev;
         ([nns, ...acc_outer], (line, rest |> tl_emp));
       },
       ([], (init(lines |> hd |> length, _ => None), lines |> tl)),
     )
  |> fst
  |> rev;

let rec move = (n, field) => {
  let next_field = field |> map(move_e) |> move_s;
  next_field == field ? n |> succ : next_field |> move(n |> succ);
};

let input = "25.txt" |> read_file |> stream_map(parse);
input |> move(0) |> print_int |> print_newline;