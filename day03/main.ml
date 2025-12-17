module Heap(Ord: Map.OrderedType) = struct
    type elt = Ord.t
    type t =
        | Empty
        | Node of t * elt * t

  let empty = Empty

  let rec insert x = function
    | Empty -> Node (Empty, x, Empty)
    | Node (l, y, r) ->
        if Ord.compare x y < 0 then Node (insert x l, y, r)
        else Node (l, y, insert x r)

  let peek = function
    | Empty -> None
    | Node (_, x, _) -> Some x
end

module PairIntHeap = Heap(struct
  type t = int * int

  let compare (v1, pos1) (v2, pos2) =
    if v1 = v2 then pos1 - pos2
    else x1 - x2
end)

let parse_line line =
    String.fold_right
        (fun c acc -> int_of_char c - int_of_char '0' :: acc)
        line []

let rec parse_lines chan = match Stdlib.input_line chan with
    | line -> parse_line line :: parse_lines chan
    | exception End_of_file -> []

let parse_file file =
    let open In_channel in
    with_open_text file parse_lines

let max2 digits =
    let rec aux (hi, lo) = function
        | [] -> (hi, lo)
        | x :: [] -> (hi, max lo x)
        | x :: (y :: _ as tl) ->
            if x > hi then aux (x, y) tl
            else if y > lo then aux (hi, y) tl
            else aux (hi, lo) tl
    in
    let (hi, lo) = aux (0, 0) digits in
    hi * 10 + lo

let pp_bank fmt bank =
    let open Format in
    fprintf fmt "[%a - %d]"
        (pp_print_list pp_print_int) bank
        (max2 bank)

let () =
    if Array.length Sys.argv < 2 then begin
        failwith "usage: day03 <file>"
    end;
    let file = Sys.argv.(1) in
    let banks = parse_file file in
    let joltages = List.map (fun digits -> max2 digits) banks in
    Format.printf "joltages: %a\n" (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_bank) banks;
    Format.printf "sum: %d\n" (List.fold_left (+) 0 joltages);