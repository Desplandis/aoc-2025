let parse chan =
  match Lexing.from_channel chan |> Parser.parse Lexer.token with
  | [] -> []
  | coords -> coords

let parse_input file =
  let open In_channel in
  with_open_text file parse

let area (a, b) (x, y) = (abs (x - a) + 1) * (abs (y - b) + 1)

let rects_of_reds =
  let rec aux_red acc red = function
    | [] -> acc
    | pos :: tl ->
      let rect = if red > pos then (pos, red) else (red, pos) in
      aux_red (rect :: acc) red tl
  in
  let rec aux acc = function
    | [] -> acc
    | red :: tl -> aux (aux_red acc red tl) tl
  in aux []

let lines_of_reds reds =
  let first = List.hd reds in
  let rec aux = function
    | [] -> []
    | [ p ] -> [ p, first ]
    | p1 :: (p2 :: _ as tl) -> (p1, p2) :: aux tl
  in aux reds

let max_area rects =
  List.map (fun (c1, c2) -> area c1 c2) rects |>
  List.sort compare |>
  List.rev |>
  List.hd

(*
let local_max_area coord =
  let rec aux mx = function
    | [] -> mx
    | x :: tl -> aux (max mx (area coord x)) tl
  in aux 0

let max_area =
  let rec aux mx = function
    | [] -> mx
    | x :: tl -> aux (max mx (local_max_area x tl)) tl
  in aux 0

let pos_of_coord xcoords ycoords (x, y) =
  let index p =
    let rec aux i = function
      | [] -> i
      | a :: l -> if p a then i else aux (i + 1) l
    in aux 0
  in
  let row = index (fun v -> v > x) xcoords in
  let col = index (fun v -> v > y) ycoords in
  (row, col)

let grid_of_coords (coords: (int * int) list) =
  let coords = List.sort compare coords in
  let xcoords, ycoords = List.split coords in
  let xcoords = List.sort_uniq compare xcoords in
  let ycoords = List.sort_uniq compare ycoords in
  let grid =
    Stdlib.Array.make_matrix
      (List.length xcoords + 2)
      (List.length ycoords + 2)
      false
  in
  let rec aux = function
    | [] -> ()
    | (x, y) :: tl ->
      let (row, col) = pos_of_coord xcoords ycoords (x, y) in
      grid.(row).(col) <- true;
      aux tl
  in aux coords; grid
*)

let pp_print_coord fmt (x, y) = Format.fprintf fmt "(%d, %d)" x y
let pp_print_coords = Format.pp_print_list pp_print_coord

let pp_print_rect fmt (c1, c2) =
  Format.fprintf fmt "[%a, %a]" pp_print_coord c1 pp_print_coord c2

let pp_print_line fmt (c1, c2) =
  Format.fprintf fmt "%a -> %a" pp_print_coord c1 pp_print_coord c2

let _pp_print_grid =
  let open Format in
  let pp_bool fmt b = fprintf fmt "%c" (if b then '#' else '.') in
  let pp_line fmt = pp_print_array ~pp_sep:(fun _ _ -> ()) pp_bool fmt in
  pp_print_array ~pp_sep:pp_print_newline pp_line

let () =
  let file = Sys.argv.(1) in
  let coords = parse_input file in
  let rects = rects_of_reds coords in
  let lines = lines_of_reds coords in
  Format.printf "%a\n" pp_print_coords coords;
  Format.printf "%a\n" (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_print_rect) rects;
  Format.printf "%a\n" (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_print_line) lines;
  Format.printf "%d\n" (max_area rects);
  ()
