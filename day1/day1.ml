let parse chan = 
    match Lexing.from_channel chan |> Parser.parse Lexer.token with
    | [] -> []
    | nbs -> nbs

let parse_input file =
    let open In_channel in
    with_open_text file parse

(*
let pp_print_int_list =
    Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int
*)

let rotate x r =
    let s = (x + r) mod 100 in
    if s < 0 then 100 + s else s

let seq_from_rotations =
    List.fold_left_map (fun acc r ->
        let x = rotate acc r in
        (x, x)
    )

let count_zeros_click x r =
    let s = (x + r) in
    if s > 0 then s / 100
    else (100 - s) / 100 - if x = 0 then 1 else 0

let zeros_from_rotations =
    List.fold_left_map (fun acc r -> rotate acc r, count_zeros_click acc r)

let () =
    if Array.length Sys.argv < 2 then begin
        failwith "usage: day1 <file>"
    end;
    let file = Sys.argv.(1) in
    let rotations = parse_input file in
    let start = 50 in
    let (_, values) = seq_from_rotations start rotations in
    let zeros = List.fold_left (fun acc x -> if x = 0 then acc + 1 else acc) 0 values in
    Format.printf "Part 1: %d\n" zeros;
    let (_, zeros) = zeros_from_rotations start rotations in
    let sum = List.fold_left (fun acc x -> acc + x) 0 zeros in
    Format.printf "Part 2: %d\n" sum
