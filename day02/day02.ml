let parse chan = 
    match Lexing.from_channel chan |> Parser.parse Lexer.token with
    | [] -> []
    | nbs -> nbs

let parse_input file =
    let open In_channel in
    with_open_text file parse

(*
let pp_pair fmt (a, b) =
    Format.fprintf fmt "%d" (a lxor b)
*)

let range a b =
    Seq.unfold (fun x -> if x > b then None else Some (x, x + 1)) a

(* Shall return a string to compare on odd number of substrings *)
let is_twice_str str =
    let n = String.length str in
    if n mod 2 <> 0 then
        false
    else
        let left = String.sub str 0 (n / 2) in
        let right = String.sub str (n / 2) (n / 2) in
        String.equal left right

let is_twice x =
    let str = string_of_int x in
    let n = String.length str in
    if n mod 2 <> 0 then
        false
    else
        let left = String.sub str 0 (n / 2) in
        let right = String.sub str (n / 2) (n / 2) in
        String.equal left right

let is_divisible_by i = function
    | "" -> true
    | str ->
        if i mod 2 = 0 then
            is_twice_str str
        else
            let n = String.length str in
            let left = String.sub str 0 (n / i) in
            let right = is_twice_str left in
            (* TODO: Compare left and right on odd number of substrings *)
            false

(* TODO: Get all divisors of string legth and try dividing and find common subgroups *)

let () =
    if Array.length Sys.argv < 2 then begin
        failwith "usage: day02 <file>"
    end;
    let file = Sys.argv.(1) in
    let input = parse_input file in
    let intervals = List.map (fun (a, b) -> range a b) input in
    let filtered = List.map (fun x -> Seq.filter is_twice x) intervals in
    let sum = List.fold_left (fun acc x -> acc + Seq.fold_left (fun acc x -> acc + x) 0 x) 0 filtered in
    List.iter (fun f -> Seq.iter (fun x -> Format.printf "%d\n" x) f) filtered;
    Format.printf "sum: %d\n" sum;
    ()