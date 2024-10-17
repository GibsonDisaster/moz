open Core

let foo = "2-4,6-8"

module IntSet = Set.Make (Int)

let intersects (c1, c2) =
  let intersection =
    Set.to_list (Set.inter (IntSet.of_list c1) (IntSet.of_list c2))
  in
  List.equal ( = ) intersection c1 || List.equal ( = ) intersection c2
;;

let get_inter (c1, c2) =
  Set.to_list (Set.inter (IntSet.of_list c1) (IntSet.of_list c2))
;;

let parse_line line =
  let sections = String.split_on_chars line ~on:[ ',' ] in
  sections
;;

let parse_pair pair =
  match pair with
  | x :: y :: z :: w :: _ ->
    int_of_string x, int_of_string y, int_of_string z, int_of_string w
  | _ -> -1000, -1000, -1000, -1000
;;

let read_file filename =
  let file_contents = In_channel.read_all filename in
  let lines = String.split_lines file_contents in
  let parsed = List.map ~f:parse_line lines in
  let flattened =
    List.map
      ~f:List.concat
      (List.map parsed ~f:(fun x ->
         List.map x ~f:(fun y -> String.split_on_chars y ~on:[ '-' ])))
  in
  List.map flattened ~f:parse_pair
;;

let solution_a =
  let parsed_input = read_file "inputs/temp.txt" in
  let ranges =
    List.map parsed_input ~f:(fun (x, y, z, w) ->
      List.range x (y + 1), List.range z (w + 1))
  in
  List.map ranges ~f:intersects |> List.filter ~f:(fun x -> x) |> List.length
;;

let solution_b =
  let parsed_input = read_file "inputs/input4.txt" in
  let ranges =
    List.map parsed_input ~f:(fun (x, y, z, w) ->
      List.range x (y + 1), List.range z (w + 1))
  in
  List.map ranges ~f:get_inter
  |> List.filter ~f:(fun x -> List.length x > 0)
  |> List.length
;;
