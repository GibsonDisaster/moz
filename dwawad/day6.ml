open Batteries

let filename : string = "inputs/input6.txt"

let file_contents =
  let f = File.lines_of filename in
  BatList.of_enum f |> BatList.hd
;;

let rec remove_duplicates l =
  let rec contains l n =
    match l with
    | [] -> false
    | h :: t -> h = n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
    let acc = remove_duplicates t in
    if contains acc h then acc else h :: acc
;;

let all_unique chars = List.equal Char.equal chars (remove_duplicates chars)

let rec find_signal stream =
  match stream with
  | [] -> 0
  | lst ->
    if all_unique (List.take 4 lst) then 4 else 1 + find_signal (List.tl lst)
;;

let solution_a = file_contents |> String.to_list |> find_signal

let rec find_message stream =
  match stream with
  | [] -> 0
  | lst ->
    if all_unique (List.take 14 lst) then 14 else 1 + find_message (List.tl lst)
;;

let solution_b = file_contents |> String.to_list |> find_message
