open List;

let memoize = (f, memo, k) =>
  k |> fst >= 5
    ? k
      |> Hashtbl.find_opt(memo)
      |> (
        fun
        | Some(cache) => cache
        | None => {
            let res = f();
            res |> Hashtbl.add(memo, k);
            res;
          }
      )
    : f();

let rec process = (params, digits, memo, z) =>
  switch (params) {
  | [] => z == 0 ? Some([]) : None
  | [(p0, p1, p2), ...rest] =>
    memoize(
      () =>
        digits
        |> find_map(w =>
             (
               w != z mod 26 + p1
                 ? (p0 ? z - z mod 26 : z * 26) + (w + p2) : p0 ? z / 26 : z
             )
             |> process(rest, digits, memo)
             |> Option.map(ws => [w, ...ws])
           ),
      memo,
      (params |> length, z),
    )
  };

let params0 = [
  false,
  false,
  false,
  false,
  true,
  true,
  false,
  true,
  false,
  true,
  false,
  true,
  true,
  true,
];
let params1 = [
  11,
  11,
  14,
  11,
  (-8),
  (-5),
  11,
  (-13),
  12,
  (-1),
  14,
  (-5),
  (-4),
  (-8),
];
let params2 = [1, 11, 1, 11, 2, 9, 7, 11, 6, 15, 7, 1, 8, 6];

let params =
  params2 |> map2(((a, b), c) => (a, b, c), params1 |> combine(params0));

[init(9, (-)(9)), init(9, succ)]
|> iter(digits =>
     process(params, digits, Hashtbl.create(1000), 0)
     |> Option.get
     |> iter(print_int)
     |> print_newline
   );