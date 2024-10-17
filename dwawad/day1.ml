open Core

let file_input = "inputs/input1.txt"

let rec group = function
  | [] -> []
  | lst ->
    let g = List.take_while ~f:(fun x -> 0 <> String.compare x "") lst in
    [ g ] @ group (List.drop lst (List.length g + 1))
;;

let read_input file_name =
  let file_contents = In_channel.read_lines file_name |> group in
  let parsed =
    List.map file_contents ~f:(fun x -> List.map ~f:int_of_string x)
  in
  let sums = List.map parsed ~f:(fun x -> List.fold x ~f:( + ) ~init:0) in
  List.sort ~compare:(fun x y -> if x > y then -1 else 1) sums
;;

let parsed_input = read_input file_input
let solution_a = List.hd parsed_input

let solution_b =
  let top_three = List.take parsed_input 3 in
  List.fold_left ~f:( + ) ~init:0 top_three
;;
