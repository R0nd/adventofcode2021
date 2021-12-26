open List;
open Lib.Util;

let is_closing =
  fun
  | ')'
  | ']'
  | '}'
  | '>' => true
  | _ => false;

let is_legal = open_char =>
  fun
  | ')' => open_char == '('
  | ']' => open_char == '['
  | '}' => open_char == '{'
  | '>' => open_char == '<'
  | _ => raise(Failure("Invalid char"));

let error_score =
  fun
  | ')' => 3
  | ']' => 57
  | '}' => 1197
  | '>' => 25137
  | _ => raise(Failure("Invalid char"));

type result =
  | Incomplete(Stack.t(char))
  | Illegal(char);

let line_error_score =
  fun
  | Illegal(char) => char |> error_score
  | _ => 0;

let completion_score =
  fun
  | '(' => 1
  | '[' => 2
  | '{' => 3
  | '<' => 4
  | _ => raise(Failure("Invalid char"));

let rec incomplete_score_internal =
  fun
  | stack when stack |> Stack.is_empty => []
  | stack => {
      let n = stack |> Stack.pop |> completion_score;
      [n, ...stack |> incomplete_score_internal];
    };

let incomplete_score = stack =>
  stack |> incomplete_score_internal |> fold_left((acc, n) => acc * 5 + n, 0);

let string_hdtl = source => (
  source.[0],
  source |> String.length |> pred |> String.sub(source, 1),
);

let rec next = (stack, chunk) =>
  switch (chunk) {
  | "" => Incomplete(stack)
  | _ =>
    let (head, rest) = chunk |> string_hdtl;
    if (head |> is_closing) {
      head |> is_legal(stack |> Stack.pop)
        ? rest |> next(stack) : Illegal(head);
    } else {
      stack |> Stack.push(head);
      rest |> next(stack);
    };
  };

let middle_value = ns => (ns |> length |> pred) / 2 |> nth(ns);

let input = "10.txt" |> read_file;
let results = input |> stream_map(x => x |> next(Stack.create()));

results
|> map(line_error_score)
|> fold_left((+), 0)
|> print_int
|> print_newline;

results
|> map(
     fun
     | Incomplete(stack) => stack |> incomplete_score
     | _ => 0,
   )
|> filter(x => x > 0)
|> sort((-))
|> middle_value
|> print_int
|> print_newline;